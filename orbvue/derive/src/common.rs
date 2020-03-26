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

pub trait Spanable {
  fn span(&self) -> Span;
}
pub trait Parsable: CursorCore {
  fn parse<T: syn::parse::Parse>(&self) -> Result<(T, Self::Marker)>;
}

pub trait CursorCore: Spanable + Clone {
  type Marker: Spanable;
  // note every const reference should not mutate inner state especially `token`
  fn token(&self) -> Option<(TokenTree, Self::Marker)>;
  fn seek(&mut self, dst: Self::Marker);
  fn tell(&self) -> Self::Marker;
}

pub trait Cursor: CursorCore + Parsable { }
impl<T: CursorCore + Parsable> Cursor for T { }

impl Spanable for syn::buffer::Cursor<'_> {
  fn span(&self) -> Span { Self::span(*self) }
}

impl CursorCore for syn::buffer::Cursor<'_> {
  type Marker = Self;
  fn token(&self) -> Option<(TokenTree, Self::Marker)> { self.token_tree() }
  fn seek(&mut self, dst: Self::Marker) {
    std::mem::replace(self, dst);
  }
  fn tell(&self) -> Self::Marker { *self }
}

pub enum MaybeOwned<'a, B> { Borrowed(&'a B), Owned(B) }
impl<'a, B> std::ops::Deref for MaybeOwned<'a, B> {
  type Target = B;
  fn deref(&self) -> &Self::Target {
    match self {
      MaybeOwned::Borrowed(b) => b,
      MaybeOwned::Owned(b) => &b,
    }
  }
}

pub struct ParseStream<'a>(MaybeOwned<'a, syn::parse::ParseBuffer<'a>>);
impl<'a> From<syn::parse::ParseStream<'a>> for ParseStream<'a> {
  fn from(stream: syn::parse::ParseStream<'a>) -> Self {
    Self(MaybeOwned::Borrowed(stream))
  }
}
impl Clone for ParseStream<'_> {
  fn clone(&self) -> Self {
    Self(MaybeOwned::Owned(self.0.fork()))
  }
}

impl Spanable for ParseStream<'_> {
  fn span(&self) -> Span { self.0.span() }
}
impl Parsable for ParseStream<'_> {
  fn parse<T: syn::parse::Parse>(&self) -> Result<(T, Self::Marker)> {
    let state = self.clone();
    let node = state.0.parse::<T>()?;
    Ok((node, state.tell()))
  }
}

impl<'a> CursorCore for ParseStream<'a> {
  type Marker = syn::buffer::Cursor<'a>;
  fn token(&self) -> Option<(TokenTree, Self::Marker)> { self.tell().token_tree() }
  fn seek(&mut self, dst: Self::Marker) {
    // hack, if scope of Cursor and ParseStream are the same, we could safely advance it
    // see https://github.com/dtolnay/syn/pull/649
    self.0.step(|_| Ok(((), unsafe { std::mem::transmute(dst) }))).unwrap();
  }
  fn tell(&self) -> Self::Marker {
    self.0.cursor()
  }
}

pub trait ParseContext: Default { }
pub trait AsParseContext<Target: ParseContext> {
  fn as_ctx(&mut self) -> &mut Target;
}

pub type UnitContext = ();
impl ParseContext for UnitContext {}
impl AsParseContext<UnitContext> for UnitContext {
  fn as_ctx(&mut self) -> &mut Self { self }
}

macro_rules! parse_context {
  (impl$(($($t:tt)*))? for $name:ident $($t2:tt)*) => {
    impl$(<$($t)*>)? ParseContext for $name $($t2)* {}
    impl$(<$($t)*>)? AsParseContext<$name $($t2)*> for $name $($t2)* {
      fn as_ctx(&mut self) -> &mut Self { self }
    }
    impl$(<$($t)*>)? AsParseContext<UnitContext> for $name $($t2)* {
      fn as_ctx(&mut self) -> &mut UnitContext { unsafe { std::mem::transmute(self) } }
    }
  };
}

pub trait Parse: Sized {
  type Context: ParseContext;
  fn parse<C: Cursor>(cursor: C, ctx: &mut Self::Context) -> Result<(Self, C::Marker)>;
}

// pub(crate) fn parse_stream<T: Parse>(stream: syn::parse::ParseStream) -> Result<T> {
//   let mut ctx = Default::default();
//   stream.step(|cursor| {
//     T::parse(*cursor, &mut ctx)
//   })
// }

pub(crate) use parse_stream2 as parse_stream;

#[allow(dead_code)]
pub(crate) fn parse_stream2<T: Parse>(stream: syn::parse::ParseStream) -> Result<T> {
  let mut ctx = Default::default();
  let (t, _) = T::parse(ParseStream::from(stream), &mut ctx)?;
  Ok(t)
}

#[cfg(test)]
pub fn parse_str<T: Parse>(s: &str) -> Result<T> {
  syn::parse::Parser::parse_str(parse_stream, s)
}

pub(crate) fn error<T, S: ToString>(c: Span, s: S) -> syn::parse::Result<T> {
  Err(syn::Error::new(c, s.to_string()))
}

pub type Attrs<Ident> = Vec<(Ident, LiteralToken)>;

pub trait XmlTag {
  type Context: ParseContext;
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
impl<Tag: XmlTag, Ident: Parse, Child: Parse> Parse for MetaXml<Tag, Ident, Child>
  where Tag::Context: AsParseContext<Ident::Context>, Tag::Context: AsParseContext<Child::Context> {
  type Context = Tag::Context;
  fn parse<C: Cursor>(mut cursor: C, ctx: &mut Self::Context) -> Result<(Self, C::Marker)> {
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
        use State::*;
        match self {
          T0 => "expect < at state T0",
          T1 => "expect ident at state T1",
          A0(_, _) => "expect attr, /, > at state A0",
          A1(_, _, _) => "expect = at state A1",
          A2(_, _, _) => "expect string at state A2",
          C0(_, _, _) => "expect child at state C0",
          S1(_, _) => "expect > at state S1",
          S2(_, _) => "expect nothing at state S2",
          E0(_, _, _) => "expect < state E0",
          E1(_, _, _) => "expect / state E1",
          E2(_, _, _) => "expect ident state E2",
          E3(_, _, _) => "expect > state E3",
          Complete(_, _, _) => "expect nothing state Complete",
        }
      }
    }

    while let Some((token, cursor_next)) = cursor.token() {
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
      cursor.seek(cursor_next);

      state = match state {
        State::A0(i, a) => {
          if let Ok((k, cursor_next)) = Ident::parse(cursor.clone(), ctx.as_ctx()) {
            cursor.seek(cursor_next);
            State::A1(i, a, k)
          } else {
            State::A0(i, a)
          }
        },
        State::C0(i, a, mut v) => {
          while let Ok((child, cursor_next)) = Child::parse(cursor.clone(), ctx.as_ctx()) {
            cursor.seek(cursor_next);
            v.push(child);
          }
          State::E0(i, a, v)
        },
        State::S2(i, a) => State::Complete(i, a, vec![]),
        _ => state,
      };
      if let State::Complete(name, attrs, children) = state {
        Tag::check(&name).expect("unreachable name check");
        return Ok((Self::new(name, attrs, children), cursor.tell()))
      }
    };
    error(cursor.span(), "unexpected eof")
  }
}
