extern crate proc_macro;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate quote;

#[macro_use] mod common;
mod compile;
mod template;
mod script;

use common::*;
use compile::Context;
use template::Template;
use script::{Script, ItemFnVisitor};
use syn::visit_mut::VisitMut;

use syn::parse::{Parser, ParseStream, Result};
use proc_macro::TokenStream;

#[proc_macro]
pub fn build_template(input: TokenStream) -> TokenStream {
  let ctx = Context::new();
  let (template, script) = Parser::parse(parse_input, input).expect("parse args");
  fn parse_input(input: ParseStream) -> Result<(Template, Script)> {
    Ok((input.call(parse_stream)?, input.call(parse_stream)?))
  }
  let mut visitor = ItemFnVisitor::new(&template, ctx.clone());
  let items = script.children.into_iter().map(|script::Child::I(mut item)| {
    visitor.visit_item_mut(&mut item.0); item.0
  }).collect::<Vec<_>>();

  let result = quote! {
    mod __orbvue__build__ {
      use orbtk::prelude::*;
      #(#items)*
    }
    pub use __orbvue__build__::*;
  };
  result.into()
}
