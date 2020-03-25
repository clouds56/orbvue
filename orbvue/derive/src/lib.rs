extern crate proc_macro;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate syn;
#[macro_use] extern crate quote;

use compile::{compile, Context};
use parse::{parse_stream, Template};

use syn::parse::{Parser, ParseStream, Result};
use proc_macro::TokenStream;

mod compile;
mod parse;

fn pop_attrs(attrs: Vec<syn::Attribute>, list: bool) -> (Vec<syn::Attribute>, Option<String>) {
  use syn::*;
  let mut result = Vec::new();
  let mut name = None;
  for attr in attrs {
    if attr.style == syn::AttrStyle::Outer && attr.path.is_ident("vue") {
      match attr.parse_meta().expect("parse_meta") {
        Meta::NameValue(MetaNameValue { lit: Lit::Str(lit_str), .. }) if !list => name = Some(lit_str.value()),
        Meta::List(MetaList { nested, .. }) if list => {
          let nested = quote! { #nested };
          name = Some(nested.to_string());
        },
        _ => (),
      }
      continue
    }
    println!("attr: {:?}", attr);
    result.push(attr);
  }
  (result, name)
}

fn transform_block(stmts: Vec<syn::Stmt>, body: proc_macro2::TokenStream) -> (Vec<syn::Stmt>, bool) {
  use syn::*;
  let mut result = Vec::new();
  let mut found = false;
  fn parse_stmt_attr(s: ParseStream) -> Result<(Vec<Attribute>, Stmt)> {
    let attrs = s.call(Attribute::parse_outer).unwrap_or_default();
    Ok((attrs, s.call(parse_stmt)?))
  }
  fn parse_stmt(s: ParseStream) -> Result<Stmt> {
    let mut stmts = s.call(Block::parse_within)?;
    if stmts.is_empty() {
      Err(s.error("should be exact 1 stmt"))
    } else {
      Ok(stmts.pop().unwrap())
    }
  }
  for stmt in stmts {
    let tokens = quote!{ #stmt };
    let (attrs, rest) = Parser::parse2(parse_stmt_attr, tokens).expect("internal parse stmt_attr");
    let (attrs, s) = pop_attrs(attrs, true);
    let tokens = if s.is_some() {
      found = true;
      match rest {
        Stmt::Local(Local { attrs: _attrs, let_token, pat, init: Some((eq, expr)), semi_token}) =>
          quote!{ #(#attrs)* #let_token #pat #eq #expr #body #semi_token },
        Stmt::Expr(e) => quote! { #(#attrs)* #e #body },
        Stmt::Semi(e, semi) => quote! { #(#attrs)* #e #body #semi },
        _ => panic!("no expr in #[vue] stmt"),
      }
    } else {
      quote! { #(#attrs)* #rest }
    };
    result.push(Parser::parse2(parse_stmt, tokens).expect("body should extend to expr"));
  }
  (result, found)
}

#[proc_macro]
pub fn build_template(input: TokenStream) -> TokenStream {
  let mut ctx = Context::new();
  let (mut itemfn, template) = Parser::parse(parse_input, input).expect("parse args");
  fn parse_input(input: ParseStream) -> Result<(syn::ItemFn, Template)> {
    let itemfn = input.parse()?;
    input.parse::<Token![,]>()?;
    Ok((itemfn, input.call(parse_stream)?))
  }
  itemfn.sig.inputs = itemfn.sig.inputs.into_iter().map(|i| {
    use syn::{FnArg, Pat};
    match i {
      FnArg::Receiver(mut i) => {
        let (attrs, d) = pop_attrs(i.attrs, false);
        i.attrs = attrs;
        if d.is_some() {
          ctx.set_name(&d.unwrap(), "self".to_string());
        }
        FnArg::Receiver(i)
      },
      FnArg::Typed(mut i) => {
        let (attrs, d) = pop_attrs(i.attrs, false);
        i.attrs = attrs;
        if d.is_some() {
          if let Pat::Ident(v) = i.pat.as_ref() {
            ctx.set_name(&d.unwrap(), v.ident.to_string());
          }
        }
        FnArg::Typed(i)
      },
    }
  }).collect();
  let body = compile(template, &ctx);
  let mut block = *itemfn.block;
  let (stmts, found) = transform_block(block.stmts, body);
  if !found { panic!("there should be #[vue] in stmts"); }
  block.stmts = stmts;
  itemfn.block = Box::new(block);
  let result = quote! {
    #itemfn
  };
  result.into()
}
