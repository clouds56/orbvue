extern crate proc_macro;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate quote;

#[macro_use] mod common;
mod compile;
mod template;
mod script;
mod css;

use common::*;
use compile::Context;
use template::Template;
use script::{Script, ItemFnVisitor, VisitMut};
use css::{Css, gen_css};

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse::{Parser, ParseStream, Result};

fn build_template_raw(input: TokenStream2) -> Result<TokenStream2> {
  let ctx = Context::new();
  let (template, script, css) = Parser::parse2(parse_input, input)?;
  fn parse_input(input: ParseStream) -> Result<(Template, Script, Option<Css>)> {
    Ok((input.call(parse_stream)?, input.call(parse_stream)?, input.call(parse_stream)?))
  }
  let mut visitor = ItemFnVisitor::new(&template, ctx.clone());
  let items = script.children.into_iter().map(|script::Child::I(mut item)| {
    visitor.visit_item_mut(&mut item.0); item.0
  }).collect::<Vec<_>>();

  let css = css.map(|css| {
    let css = gen_css(&css);
    quote!{ pub const CSS: &'static str = #css; }
  });

  let result = quote! {
    mod __orbvue__build__ {
      use orbtk::prelude::*;
      #(#items)*
      #css
    }
    pub use __orbvue__build__::*;
  };
  Ok(result)
}

#[proc_macro]
pub fn build_template(input: TokenStream) -> TokenStream {
  match build_template_raw(input.into()) {
    Ok(i) => i,
    Err(e) => e.to_compile_error(),
  }.into()
}
