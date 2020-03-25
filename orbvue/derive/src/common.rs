use proc_macro2::{
  TokenTree, Span,
  Ident as IdentToken,
  Literal as LiteralToken,
};
use syn::Result;

pub struct SpanDiff(pub usize, pub usize);
impl ToString for SpanDiff {
  fn to_string(&self) -> String {
    let mut s = String::new();
    if self.0 != 0 {
      s.push_str(&"\n".repeat(self.0));
    }
    if self.1 != 0 {
      s.push_str(&" ".repeat(self.1));
    }
    s
  }
}
impl SpanDiff {
  pub fn new(last: Option<Span>, current: Span) -> Self {
    if last.is_none() {
      return SpanDiff(0, 0);
    }
    let l = current.start().line - last.unwrap().end().line;
    if l != 0 {
      return SpanDiff(l, 0);
    }
    if current.start().column < last.unwrap().end().column {
      SpanDiff(0, 0)
    } else {
      SpanDiff(0, current.start().column - last.unwrap().end().column)
    }
  }
  pub fn is_empty(&self) -> bool {
    self.0 == 0 && self.1 == 0
  }
}

pub trait Cursor: Clone + Sized {
  fn next(&self) -> Option<(TokenTree, Self)>;
  fn span(&self) -> Span;
}

impl Cursor for syn::buffer::Cursor<'_> {
  fn next(&self) -> Option<(TokenTree, Self)> {
    self.token_tree()
  }
  fn span(&self) -> Span { Self::span(*self) }
}

pub trait Parse: Sized {
  fn parse<C: Cursor>(cursor: C) -> Result<(Self, C)>;
}

pub(crate) fn parse_stream<T: Parse>(stream: syn::parse::ParseStream) -> Result<T> {
  stream.step(|cursor| {
    T::parse(*cursor)
  })
}

#[cfg(test)]
pub fn parse_str<T: Parse>(s: &str) -> Result<T> {
  syn::parse::Parser::parse_str(parse_stream, s)
}

pub(crate) fn error<T>(c: Span, s: &str) -> syn::parse::Result<T> {
  Err(syn::Error::new(c, s))
}

pub type Attrs<Ident> = Vec<(Ident, LiteralToken)>;

pub trait XmlTag {
  fn name() -> Option<&'static str> { None }
  fn check(name: &IdentToken) -> Result<()> {
    match Self::name() {
      Some(tag) if tag != &name.to_string() =>
        error(name.span(), "wrong tag name"),
      _ => Ok(()),
    }
  }
}
#[derive(Debug)]
pub struct AnyTag;
impl XmlTag for AnyTag { }

#[derive(Debug)]
pub struct MetaXml<Tag: XmlTag, Ident: Parse, Child: Parse> {
  pub name: IdentToken,
  pub attrs: Attrs<Ident>,
  pub children: Vec<Child>,
  _marker: std::marker::PhantomData<Tag>,
}

impl<Tag: XmlTag, Ident: Parse, Child: Parse> MetaXml<Tag, Ident, Child> {
  fn new(name: IdentToken, attrs: Attrs<Ident>, children: Vec<Child>) -> Self {
    Self { name, attrs, children, _marker: Default::default() }
  }
}
impl<Tag: XmlTag, Ident: Parse, Child: Parse> Parse for MetaXml<Tag, Ident, Child> {
  fn parse<C: Cursor>(mut cursor: C) -> Result<(Self, C)> {
    let mut state = State::T0;
    enum State<Ident: Parse, Child: Parse> {
      T0 /* (<) */, T1 /* < (ident) */,
      A0(IdentToken, Attrs<Ident>) /* <ident (attr, /, >) */,
      A1(IdentToken, Attrs<Ident>, Ident) /* <ident attr (=) */,
      A2(IdentToken, Attrs<Ident>, Ident) /* <ident attr= (string) */,
      C0(IdentToken, Attrs<Ident>, Vec<Child>) /* <ident attr=string> (<, {...}) */,
      S1(IdentToken, Attrs<Ident>) /* <ident / (>) */,
      S2(IdentToken, Attrs<Ident>) /* <ident /> */,
      E0(IdentToken, Attrs<Ident>, Vec<Child>) /* ... (<) */,
      E1(IdentToken, Attrs<Ident>, Vec<Child>) /* ... < (/) */,
      E2(IdentToken, Attrs<Ident>, Vec<Child>) /* ... </ (ident) */,
      E3(IdentToken, Attrs<Ident>, Vec<Child>) /* ... </ident (>) */,
      Complete(IdentToken, Attrs<Ident>, Vec<Child>),
    }
    impl<Ident: Parse, Child: Parse> State<Ident, Child> {
      fn expect(&self) -> &'static str {
        "unexpected"
      }
    }

    while let Some((token, cursor_next)) = cursor.next() {
      // TODO: check span for space
      state = match token {
        TokenTree::Punct(t) if t.as_char() == '<' => {
          match state {
            State::T0 => State::T1,
            State::E0(i, a, v) => State::E1(i, a, v),
            _ => return error(cursor.span(), state.expect()),
          }
        },
        TokenTree::Punct(t) if t.as_char() == '>' => {
          match state {
            State::A0(i, a) => State::C0(i, a, vec![]),
            State::S1(i, a) => State::S2(i, a),
            State::E3(i, a, v) => State::Complete(i, a, v),
            _ => return error(cursor.span(), state.expect()),
          }
        },
        TokenTree::Punct(t) if t.as_char() == '/' => {
          match state {
            State::A0(i, a) => State::S1(i, a),
            State::E1(i, a, v) => State::E2(i, a, v),
            _ => return error(cursor.span(), state.expect()),
          }
        }
        TokenTree::Punct(t) if t.as_char() == '=' => {
          match state {
            State::A1(i, a, k) => State::A2(i, a, k),
            _ => return error(cursor.span(), state.expect()),
          }
        },
        TokenTree::Literal(t) => {
          match state {
            State::A2(i, mut a, k) => { a.push((k, t.clone())); State::A0(i, a) },
            _ => return error(cursor.span(), state.expect()),
          }
        },
        TokenTree::Ident(t) => {
          match state {
            State::T1 => {
              Tag::check(&t)?;
              State::A0(t.clone(), vec![])
            },
            State::E2(i, a, v) if t.to_string() == i.to_string() => State::E3(i, a, v),
            _ => return error(cursor.span(), state.expect()),
          }
        },
        _ => return error(cursor.span(), state.expect())
      };
      cursor = cursor_next;

      state = match state {
        State::A0(i, a) => {
          if let Ok((k, cursor_next)) = Ident::parse(cursor.clone()) {
            cursor = cursor_next;
            State::A1(i, a, k)
          } else {
            State::A0(i, a)
          }
        },
        State::C0(i, a, mut v) => {
          while let Ok((child, cursor_next)) = Child::parse(cursor.clone()) {
            cursor = cursor_next;
            v.push(child);
          }
          State::E0(i, a, v)
        },
        State::S2(i, a) => State::Complete(i, a, vec![]),
        _ => state,
      };
      if let State::Complete(name, attrs, children) = state {
        Tag::check(&name).expect("unreachable name check");
        return Ok((Self::new(name, attrs, children), cursor))
      }
    };
    error(cursor.span(), "unexpected eof")
  }
}
