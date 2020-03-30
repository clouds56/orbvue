use crate::common::*;

use proc_macro2::{
  TokenTree, TokenStream,
  Group as GroupToken,
  Delimiter, Span,
};
use syn::parse::Result;

pub struct Context {
  pub scoped: bool,
}
impl Default for Context {
  fn default() -> Self {
    Self { scoped: false }
  }
}
parse_context!(Context);

pub use crate::script::Ident;

#[derive(Debug)]
pub struct CssTag;
impl XmlTag for CssTag {
  type Context = Context;
  fn name() -> Option<&'static str> { Some("css") }
}
pub type Css = MetaXml<CssTag, Ident, Child>;

#[derive(Debug)]
pub struct CssItem(pub TokenStream, pub GroupToken, pub Span);
impl Parse for CssItem {
  type Context = ();
  fn parse<C: Cursor>(mut cursor: C, _: &mut Self::Context) -> Result<(Self, C::Marker)> {
    let mut span = Span::call_site();
    let mut rule = Vec::new();
    while let Some((token, cursor_next)) = cursor.token() {
      match token {
        TokenTree::Punct(t) if rule.is_empty() && t.as_char() == '<' =>
          return error(cursor.span(), "prefix <"),
        TokenTree::Group(t) if t.delimiter() == Delimiter::Brace => {
          span = span.join(t.span()).expect("css same file");
          return Ok((Self(rule.into_iter().collect(), t, span), cursor_next))
        },
        _ => {
          if rule.is_empty() {
            span = token.span();
          } else {
            span = span.join(token.span()).expect("css same file");
          }
          rule.push(token)
        },
      }
      cursor.seek(cursor_next)
    }
    error(cursor.span(), "unexpected eof")
  }
}
impl ToString for CssItem {
  fn to_string(&self) -> String {
    // TODO: span.source()
    format!("{} {{\n{}\n}}\n", Self::format_stream(&self.0), Self::format_stream(&self.1.stream()))
  }
}
impl CssItem {
  fn format_stream(stream: &TokenStream) -> String {
    let mut last = None;
    let mut s = String::new();
    for token in stream.clone().into_iter() {
      let current = token.span();
      let diff = SpanDiff::new(last, current);
      if last.is_none() {
        let n = current.start().column;
        s.push_str(&" ".repeat(n));
      }
      if diff.0 > 0 {
        s.push('\n');
      }
      if diff.1 > 0 {
        s.push_str(&" ".repeat(diff.1));
      }
      s.push_str(&token.to_string());
      last = Some(current)
    }
    s
  }
}

#[derive(Debug)]
pub enum Child {
  I(Literal)
}

impl Parse for Child {
  type Context = Context;
  fn parse<C: Cursor>(cursor: C, ctx: &mut Self::Context) -> Result<(Self, C::Marker)> {
    if let Some((TokenTree::Punct(t), _)) = cursor.token() {
      if t.as_char() == '<' {
        return error(cursor.span(), "child end");
      }
    }
    if let Ok((k, cursor_next)) = Literal::parse(cursor.clone(), ctx.as_ctx()) {
      Ok((Child::I(k), cursor_next))
    } else {
      error(cursor.span(), "not valid child")
    }
  }
}

#[cfg(test)]
const CSS: &str = r##"
<css scoped=true>r#"
.header {
  background: #444e55;
}

.content {
  background: #3b434a;
}

text_box {
  background: transparent;
  border-width: 0;
  color: #9dafbf;
  font-size: 16;
}

#input {
  font-size: 16;
}

text-block {
  font-size: 42;
  color: #dfebf5;
}

#input {
  background: transparent;
}

button {
  border-radius: 1;
  font-size: 20;
}
"#""</css>
  "##;

#[test]
fn test_css() {
  let s = parse_str::<Css>(CSS).unwrap();
  for Child::I(i) in s.children {
    println!("{}", i.value_str().unwrap());
  }
}

pub fn gen_css(css: &Css) -> TokenStream {
  if css.children.is_empty() {
    quote!("")
  } else if css.children.len() == 1 {
    let Child::I(css) = css.children.last().unwrap();
    let lit = &css.lit;
    quote!(#lit)
  } else {
    let css = css.children.iter().map(|Child::I(css)| {
      let lit = &css.lit;
      quote!(#lit)
    }).collect::<Vec<_>>();
    quote!{
      concat!(#(#css),*)
    }
  }
}

#[test]
fn test_gen_css() {
  let s = parse_str::<Css>(CSS).unwrap();
  println!("{}", gen_css(&s));
}
