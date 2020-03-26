use crate::common::*;

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

pub struct Context<T: Parsable> {
  lang: String,
  buffer: Option<T>,
}
impl<T: Parsable> Default for Context<T> {
  fn default() -> Self {
    Self { lang: "rs".to_string(), buffer: None }
  }
}
parse_context!(impl(T: Parsable) for Context<T>);

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
  type Context = ();
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
  type Context = ();
  fn parse<C: Cursor>(cursor: C, ctx: &mut Self::Context) -> Result<(Self, C::Marker)> {
    if let Ok((k, cursor_next)) = RustItem::parse(cursor.clone(), ctx.as_ctx()) {
      Ok((Child::I(k), cursor_next))
    } else {
      error(cursor.span(), "not valid child")
    }
  }
}

#[cfg(test)]
pub const SCRIPT: &'static str = r##"
<script lang="rs">
fn generate_digit_button(
  ctx: &mut BuildContext,
  id: Entity,
  sight: char,
  primary: bool,
  column: usize,
  column_span: usize,
  row: usize,
) -> Entity {
  let mut button = Button::create()
    .class("single_content")
    .min_size(48.0, 48.0)
    .text(sight.to_string())
    .on_click(move |states, _| -> bool {
      states.get_mut::<MainViewState>(id).action(Action::Digit(sight));
      true
    })
    .attach(Grid::column(column))
    .attach(Grid::row(row))
    .attach(Grid::column_span(column_span));

  if primary {
    button = button.class("primary");
  }

  button.build(ctx)
}

fn generate_operation_button(
  ctx: &mut BuildContext,
  id: Entity,
  sight: char,
  primary: bool,
  column: usize,
  column_span: usize,
  row: usize,
) -> Entity {
  let mut button = Button::create()
    .class("single_content")
    .min_size(48.0, 48.0)
    .text(sight.to_string())
    .class("square")
    .on_click(move |states, _| -> bool {
      states.get_mut::<MainViewState>(id).action(Action::Operator(sight));
      true
    })
    .attach(Grid::column(column))
    .attach(Grid::column_span(column_span))
    .attach(Grid::row(row));

  if primary {
    button = button.class("primary");
  }

  button.build(ctx)
}

impl Template for MainView {
  #[orbvue::build]
  fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
    #[orbvue(template)]
    self
  }
}
</script>
  "##;

#[test]
fn test_script() {
  use crate::common::parse_str;
  let t = parse_str::<Script>(SCRIPT).unwrap();
  println!("{:?}", t);
}
