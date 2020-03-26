use crate::common::*;
use crate::template::Template;
use crate::compile::{compile, Context as CompileContext};

use proc_macro2::{
  TokenTree,
  Ident as IdentToken
};
use syn::parse::Result;

pub trait Parse2: Sized {
  fn parse<P: Parsable, T: syn::parse::Parse>(parser: P) -> Result<(T, P::Marker)> {
    parser.parse()
  }
}

pub struct Context {
  lang: String,
}
impl Default for Context {
  fn default() -> Self {
    Self { lang: "rs".to_string() }
  }
}
parse_context!(impl for Context);

#[derive(Debug)]
pub struct Ident(pub IdentToken);
impl Parse for Ident {
  type Context = ();
  fn parse<C: Cursor>(cursor: C, _: &mut Self::Context) -> Result<(Self, C::Marker)> {
    if let Some((TokenTree::Ident(i), cursor_next)) = cursor.token() {
      Ok((Self(i), cursor_next))
    } else {
      error(cursor.span(), "expect ident")
    }
  }
}

#[derive(Debug)]
pub struct ScriptTag;
impl XmlTag for ScriptTag {
  type Context = Context;
  fn name() -> Option<&'static str> { Some("script") }
}
pub type Script = MetaXml<ScriptTag, Ident, Child>;

#[derive(Debug)]
pub struct RustItem(pub syn::Item);
impl Parse for RustItem {
  type Context = ();
  fn parse<C: Cursor>(cursor: C, _: &mut Self::Context) -> Result<(Self, C::Marker)> {
    let (i, cursor_next) = cursor.parse()?;
    Ok((Self(i), cursor_next))
  }
}
#[derive(Debug)]
pub enum Child {
  I(RustItem)
}

impl Parse for Child {
  type Context = Context;
  fn parse<C: Cursor>(cursor: C, ctx: &mut Self::Context) -> Result<(Self, C::Marker)> {
    if let Ok((k, cursor_next)) = RustItem::parse(cursor.clone(), ctx.as_ctx()) {
      Ok((Child::I(k), cursor_next))
    } else {
      error(cursor.span(), "not valid child")
    }
  }
}

#[cfg(test)]
const SCRIPT: &'static str = r##"
<script lang="rs">
fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
  #[orbvue="expr"]
  self
}

impl Template for MainView {
  #[orbvue::template]
  fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
    #[orbvue="expr"]
    self
  }
}
</script>
  "##;

#[test]
fn test_script() {
  let s = parse_str::<Script>(SCRIPT).unwrap();
  println!("{:?}", s);
}

pub struct AttrValue {
  pub style: syn::AttrStyle,
  pub path: syn::Path,
  pub meta: syn::Meta,
}

impl AttrValue {
  pub fn value_str(&self) -> Option<String> {
    use syn::*;
    if self.path.is_ident("orbvue") {
      match self.meta {
        Meta::NameValue(MetaNameValue { lit: Lit::Str(ref lit_str), .. }) => return Some(lit_str.value()),
        // Meta::List(MetaList { nested, .. }) if list => {
        //   let nested = quote! { #nested };
        //   name = Some(nested.to_string());
        // },
        _ => (),
      }
    }
    None
  }
}

fn pop_attrs(attrs: Vec<syn::Attribute>) -> (Vec<syn::Attribute>, Option<AttrValue>) {
  let mut attrs2 = Vec::new();
  let mut result = Vec::new();
  for attr in attrs {
    if attr.path.segments.first().map(|s| &s.ident.to_string() == "orbvue") == Some(true) {
      let meta = attr.parse_meta().expect("parse_meta");
      result.push(AttrValue { style: attr.style, path: attr.path, meta })
    } else {
      attrs2.push(attr);
    }
  }
  (attrs2, result.pop())
}

pub fn transform_block(stmts: Vec<syn::Stmt>, body: proc_macro2::TokenStream) -> (Vec<syn::Stmt>, bool) {
  use syn::*;
  use syn::parse::*;
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
    let (attrs, value) = pop_attrs(attrs);
    let tokens = if value.is_some() {
      // TODO check orbvue
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

pub struct ItemFnVisitor<'a> {
  template: &'a Template,
  ctx: CompileContext,
}
impl syn::visit_mut::VisitMut for ItemFnVisitor<'_> {
  fn visit_item_fn_mut(&mut self, itemfn: &mut syn::ItemFn) {
    self.transform_fn(&mut itemfn.attrs, &mut itemfn.sig, &mut itemfn.block);
    syn::visit_mut::visit_item_fn_mut(self, itemfn);
  }
  fn visit_impl_item_method_mut(&mut self, itemfn: &mut syn::ImplItemMethod) {
    self.transform_fn(&mut itemfn.attrs, &mut itemfn.sig, &mut itemfn.block);
    syn::visit_mut::visit_impl_item_method_mut(self, itemfn);
  }
}
impl<'a> ItemFnVisitor<'a> {
  pub fn new(template: &'a Template, ctx: CompileContext) -> Self {
    Self { template, ctx }
  }
  pub fn transform_fn(&self, attrs: &mut Vec<syn::Attribute>, sig: &mut syn::Signature, block: &mut syn::Block) {
    let (attrs2, orbvue) = pop_attrs(std::mem::replace(attrs, vec![]));
    std::mem::replace(attrs, attrs2);
    if orbvue.is_none() {
      // TODO: check path
      return
    }
    let mut ctx = self.ctx.clone();
    let inputs = std::mem::replace(&mut sig.inputs, syn::punctuated::Punctuated::new());
    sig.inputs = inputs.into_iter().map(|i| {
      use syn::{FnArg, Pat};
      match i {
        FnArg::Receiver(mut i) => {
          let (attrs, value) = pop_attrs(i.attrs);
          i.attrs = attrs;
          if value.is_some() {
            ctx.set_name(&value.unwrap().value_str().expect("parse name failed in attr"), "self".to_string());
          }
          FnArg::Receiver(i)
        },
        FnArg::Typed(mut i) => {
          let (attrs, value) = pop_attrs(i.attrs);
          i.attrs = attrs;
          if value.is_some() {
            if let Pat::Ident(v) = i.pat.as_ref() {
              ctx.set_name(&value.unwrap().value_str().expect("parse name failed in attr"), v.ident.to_string());
            }
          }
          FnArg::Typed(i)
        },
      }
    }).collect();
    let body = compile(self.template, &ctx);
    let stmts = std::mem::replace(&mut block.stmts, vec![]);
    let (stmts, found) = transform_block(stmts, body);
    if !found { panic!("there should be #[orbvue] in stmts"); }
    std::mem::replace(&mut block.stmts, stmts);
  }
}

#[test]
fn test_gen_script() {
  use syn::visit_mut::VisitMut;
  const TEMPLATE: &'static str = r##"<template name="hello" />"##;
  let s = parse_str::<Script>(SCRIPT).unwrap();
  let t = parse_str::<Template>(TEMPLATE).unwrap();
  let mut visitor = ItemFnVisitor::new(&t, CompileContext::new());
  for Child::I(item) in s.children {
    let mut item = item.0;
    visitor.visit_item_mut(&mut item);
    println!("{}", quote!{#item});
  }
}
