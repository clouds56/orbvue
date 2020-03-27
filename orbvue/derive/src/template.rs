use crate::common::*;

use proc_macro2::{
  TokenStream, TokenTree,
  Ident as IdentToken,
  Delimiter, Span,
};
use syn::parse::Result;

pub struct MustacheContext {
  prefix: String,
  suffix: String,
}
impl Default for MustacheContext {
  fn default() -> Self {
    Self { prefix: "{{".to_string(), suffix: "}}".to_string() }
  }
}
parse_context!(MustacheContext);

#[derive(Default)]
pub struct Context {
  mustache: MustacheContext,
}
parse_context!(Context);
impl AsParseContext<MustacheContext> for Context {
  fn as_ctx(&mut self) -> &mut MustacheContext {
    &mut self.mustache
  }
}

pub enum IdentComponent {
  V(IdentToken),
  Colon,
  At,
  Bracket(TokenStream),
  Dollar,
  I(IdentToken),
}
impl std::fmt::Debug for IdentComponent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use IdentComponent::*;
    match self {
      Colon => write!(f, ":"),
      At => write!(f, "@"),
      Dollar => write!(f, "$"),
      V(i) => write!(f, "V({})", i.to_string()),
      Bracket(i) => write!(f, "B[{}]", i.to_string()),
      I(i) => write!(f, "I({})", i.to_string()),
    }
  }
}
// prefix := "@" | ":" | "v-" str ":"
// main_part := str | "[" str "]"
// main := prefix main_part | "v-" str
// suffix := ":" | "$"
// ident := main suffix
pub struct Ident {
  pub prefix: Option<IdentComponent>,
  pub main: IdentComponent,
  pub suffix: Option<IdentComponent>,
}

impl std::fmt::Debug for Ident {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut g = f.debug_tuple("Ident");
    if self.prefix.is_some() { g.field(&self.prefix.as_ref().unwrap()); }
    else if self.suffix.is_some() { g.field(&self.prefix); }
    g.field(&self.main);
    if self.suffix.is_some() { g.field(self.suffix.as_ref().unwrap()); }
    g.finish()
  }
}

pub enum MustachePrefix {
  And,
  None,
}
pub enum MustacheItem {
  K(TokenTree),
  G(Delimiter, Vec<MustacheItem>, Span),
  P(IdentToken),
}
impl MustacheItem {
  pub fn from(stream: TokenStream) -> Vec<Self> {
    let mut result = Vec::new();
    let mut stream = stream.into_iter();
    while let Some(i) = stream.next() {
      match i {
        TokenTree::Punct(ref t) if t.as_char() == '$' => {
          match stream.next() {
            Some(TokenTree::Ident(i)) => result.push(MustacheItem::P(i)),
            Some(j) => { result.push(MustacheItem::K(i)); result.push(MustacheItem::K(j)); }
            _ => { result.push(MustacheItem::K(i)); }
          }
        },
        TokenTree::Group(t) => {
          result.push(MustacheItem::G(t.delimiter(), Self::from(t.stream()), t.span()))
        }
        _ => result.push(MustacheItem::K(i))
      };
    }
    result
  }
}

pub struct Mustache {
  pub prefix: MustachePrefix,
  pub content: Vec<MustacheItem>,
}

impl Mustache {
  pub fn parse_lit(lit: &Literal) -> Result<Self> {
    let span = lit.span();
    let v = lit.value_str().ok_or_else(|| syn::Error::new(span, "parse_lit"))?;
    let stream = v.parse::<TokenStream>().map_err(|_| syn::Error::new(span, "parse TokenStream"))?;
    Ok(Self { prefix: MustachePrefix::None, content: MustacheItem::from(stream) })
  }
}

pub struct Comment {
  pub content: String
}

impl std::fmt::Debug for MustacheItem {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      MustacheItem::K(k) => write!(f, "{}", k),
      MustacheItem::G(k, v, _) => match k {
        Delimiter::Brace => f.debug_set().entries(v.iter()).finish(),
        Delimiter::Bracket => f.debug_list().entries(v.iter()).finish(),
        Delimiter::Parenthesis => v.iter().fold(&mut f.debug_tuple(""), |acc, x| acc.field(x)).finish(),
        Delimiter::None => write!(f, "{:?}", v),
      }
      MustacheItem::P(p) => write!(f, "$:{}", p),
    }
  }
}
impl std::fmt::Debug for Mustache {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "M{:?}", self.content)
  }
}
impl std::fmt::Debug for Comment {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "/* {} */", self.content)
  }
}

#[derive(Debug)]
pub struct TemplateTag;
impl XmlTag for TemplateTag {
  type Context = Context;
  fn name() -> Option<&'static str> { Some("template") }
}
#[derive(Debug)]
pub struct InnerTag;
impl XmlTag for InnerTag {
  type Context = Context;
  fn name() -> Option<&'static str> { None }
}
pub type TemplateXml<Tag> = MetaXml<Tag, Ident, Child>;
pub type Template = TemplateXml<TemplateTag>;
pub type TemplateInner = TemplateXml<InnerTag>;

#[derive(Debug)]
pub enum Child {
  T(TemplateInner),
  M(Mustache),
  C(Comment),
}

impl Parse for Ident {
  type Context = UnitContext;
  fn parse<C: Cursor>(mut cursor: C, _: &mut Self::Context) -> Result<(Self, C::Marker)> {
    let mut state = State::P0;
    enum State {
      P0 /* (@,:,str,[expr]) */, V1(IdentToken) /* v (-,:,$,=) */, V2 /* v- (str) */,
      V3(IdentToken) /* v- str (:,$,=) */,
      V4(IdentToken) /* v- str : (str,[expr],=) */,
      M0(IdentComponent) /* @ or : (str,[expr]) */,
      S0(Option<IdentComponent>, IdentComponent) /* (:,$,=) */,
      Complete(Option<IdentComponent>, IdentComponent, IdentComponent) /* (=) */,
    }
    impl State {
      fn expect(&self) -> &'static str {
        use State::*;
        match self {
          P0 => "expect @, :, ident for attr",
          V1(_) => "expect :, $, = after name",
          V2 => "expect ident after v-",
          V3(_) => "expect :, $, = after v-ident",
          V4(_) => "expect ident, = after v-ident:",
          M0(_) => "expect ident after @ or :",
          S0(_, _) => "expect :, $, = after ident",
          Complete(_, _,_) => "expect = after attr name",
        }
      }
    }

    while let Some((token, cursor_next)) = cursor.token() {
      // TODO: check span for space
      state = match token {
        TokenTree::Punct(t) if t.as_char() == '=' => {
          let result = match state {
            State::V1(main) => Ident { prefix: None, main: IdentComponent::I(main), suffix: None },
            State::V3(main) => Ident { prefix: None, main: IdentComponent::V(main), suffix: None },
            State::V4(main) => Ident { prefix: None, main: IdentComponent::V(main), suffix: None },
            State::S0(prefix, main) => Ident { prefix, main, suffix: None},
            State::Complete(prefix, main, suffix) => Ident { prefix, main, suffix: Some(suffix) },
            _ => return error(cursor.span(), state.expect()),
          };
          return Ok((result, cursor.tell()))
        },
        TokenTree::Punct(t) if t.as_char() == '@' => {
          match state {
            State::P0 => State::M0(IdentComponent::At),
            _ => return error(cursor.span(), state.expect()),
          }
        }
        TokenTree::Punct(t) if t.as_char() == ':' => {
          match state {
            State::P0 => State::M0(IdentComponent::Colon),
            State::V3(i) => State::V4(i),
            State::S0(i, j) => State::Complete(i, j, IdentComponent::Colon),
            _ => return error(cursor.span(), state.expect()),
          }
        }
        TokenTree::Punct(t) if t.as_char() == '$' => {
          match state {
            State::V3(i) => State::Complete(None, IdentComponent::V(i), IdentComponent::Dollar),
            State::S0(i, j) => State::Complete(i, j, IdentComponent::Dollar),
            _ => return error(cursor.span(), state.expect()),
          }
        }
        TokenTree::Punct(t) if t.as_char() == '-' => {
          match state {
            State::V1(_) => State::V2,
            _ => return error(cursor.span(), state.expect()),
          }
        }
        TokenTree::Ident(i) => {
          match state {
            State::P0 if i == "v" => State::V1(i.clone()),
            State::P0 => State::S0(None, IdentComponent::I(i.clone())),
            State::V2 => State::V3(i.clone()),
            State::V4(j) => State::S0(Some(IdentComponent::V(j)), IdentComponent::I(i.clone())),
            State::M0(j) => State::S0(Some(j), IdentComponent::I(i.clone())),
            _ => return error(cursor.span(), state.expect()),
          }
        }
        TokenTree::Group(i) if i.delimiter() == Delimiter::Bracket => {
          match state {
            State::P0 => State::S0(None, IdentComponent::Bracket(i.stream())),
            State::V4(j) => State::S0(Some(IdentComponent::V(j)), IdentComponent::Bracket(i.stream())),
            State::M0(j) => State::S0(Some(j), IdentComponent::Bracket(i.stream())),
            _ => return error(cursor.span(), state.expect()),
          }
        }
        _ => return error(cursor.span(), state.expect()),
      };
      cursor.seek(cursor_next);
    }
    error(cursor.span(), "unexpected eof")
  }
}

#[test]
fn parse_ident() {
  fn parse_stream_ident(stream: syn::parse::ParseStream) -> Result<Ident> {
    let s = parse_stream::<Ident>(stream)?;
    stream.parse::<syn::token::Eq>().map(|_| s)
  };
  let parse_str = |s: &str| syn::parse::Parser::parse_str(parse_stream_ident, s);
  assert_eq!(format!("{:?}", parse_str("v=").unwrap()), "Ident(I(v))".to_string());
  assert_eq!(format!("{:?}", parse_str("@v=").unwrap()), "Ident(@, I(v))".to_string());
  assert_eq!(format!("{:?}", parse_str(":v=").unwrap()), "Ident(:, I(v))".to_string());
  assert_eq!(format!("{:?}", parse_str("v-bind:a$=").unwrap()), "Ident(V(bind), I(a), $)".to_string());
  assert_eq!(format!("{:?}", parse_str(":a$=").unwrap()), "Ident(:, I(a), $)".to_string());
  assert_eq!(format!("{:?}", parse_str("v-bind:[a]:=").unwrap()), "Ident(V(bind), B[a], :)".to_string());
  assert_eq!(format!("{:?}", parse_str(":[a]=").unwrap()), "Ident(:, B[a])".to_string());
}

impl Parse for Mustache {
  type Context = MustacheContext;
  fn parse<C: Cursor>(mut cursor: C, _: &mut Self::Context) -> Result<(Self, C::Marker)> {
    if let Some((TokenTree::Group(t), cursor_next)) = cursor.token() {
      let mut inner = t.stream().into_iter();
      if t.delimiter() != Delimiter::Brace {
        return error(cursor.span(), "expect brace");
      }
      let (prefix, content) = if let Some(TokenTree::Group(t)) = inner.next() {
        if t.delimiter() != Delimiter::Brace {
          return error(cursor.span(), "expect double brace");
        }
        let mut stream = t.stream().into_iter().peekable();
        let prefix = match stream.peek() {
          Some(TokenTree::Punct(i)) if i.as_char() == '&' => {
            stream.next();
            MustachePrefix::And
          },
          _ => MustachePrefix::None
        };
        (prefix, stream.collect())
      } else {
        return error(cursor.span(), "expect double brace");
      };
      if let Some(t) = inner.next() {
        return error(t.span(), "unexpected token");
      }
      cursor.seek(cursor_next);
      return Ok((Mustache { prefix, content: MustacheItem::from(content) }, cursor.tell()))
    }
    error(cursor.span(), "expect group")
  }
}

#[test]
fn parse_mustache() {
  let parse_str = |s: &str| parse_str::<Mustache>(s);
  assert_eq!(format!("{:?}", parse_str("{{hello$world{$3$end$}}}").unwrap()), "M[hello, $:world, {$, 3, $:end, $}]");
  assert_eq!(parse_str("{}").err().unwrap().to_string(), "expect double brace");
  assert_eq!(parse_str("{1,2,3}").err().unwrap().to_string(), "expect double brace");
  assert_eq!(parse_str("{{1}=}").err().unwrap().to_string(), "unexpected token");
}

impl Parse for Comment {
  type Context = UnitContext;
  fn parse<C: Cursor>(mut cursor: C, _: &mut Self::Context) -> Result<(Self, C::Marker)> {
    let mut state = State::M0;
    enum State {
      M0, M1 /* < (!) */, M2(usize) /* <! (--) */,
      M3(usize, String, usize, SpanDiff) /* <!-- (any, -) */, M4(String),
    }
    impl State {
      fn expect(&self) -> &'static str {
        "unexpected"
      }
    }
    let mut last: Option<Span> = None;
    while let Some((t, cursor_next)) = cursor.token() {
      let current = t.span();
      let diff = SpanDiff::new(last, current);
      state = match state {
        State::M0 if &t.to_string() == "<" => State::M1,
        State::M1 if &t.to_string() == "!" => State::M2(0),
        State::M2(i) if &t.to_string() == "-" && diff.is_empty() => State::M2(i+1),
        State::M2(i) if i >= 2 => State::M3(i, t.to_string(), 0, diff),
        State::M3(i, s, 0, _) if &t.to_string() == "-" => State::M3(i, s, 1, diff),
        State::M3(i, s, j, d) if &t.to_string() == "-" && diff.is_empty() => State::M3(i, s, j+1, d),
        State::M3(i, mut s, j, d) if j >= i && &t.to_string() == ">" => {
          if j > i {
            s.push_str(&d.to_string());
            s.push_str(&"-".repeat(j-i));
          }
          State::M4(s)
        },
        State::M3(i, mut s, j, d) => {
          if j > 0 {
            s.push_str(&d.to_string());
            s.push_str(&"-".repeat(j));
          }
          s.push_str(&diff.to_string());
          if &t.to_string() == "-" {
            State::M3(i, s, 1, diff)
          } else {
            s.push_str(&t.to_string());
            State::M3(i, s, 0, diff)
          }
        },
        _ => return error(cursor.span(), state.expect()),
      };
      cursor.seek(cursor_next);
      if let State::M4(content) = state {
        return Ok((Comment { content }, cursor.tell()));
      }
      last = Some(current);
    }
    error(cursor.span(), "expect group")
  }
}

#[test]
fn parse_comment() {
  let parse_str = |s: &str| parse_str::<Comment>(s);
  assert_eq!(format!("{:?}", parse_str("<!-- hello:world  ! -->").unwrap()), "/* hello:world  ! */".to_string());
  assert_eq!(format!("{:?}", parse_str("<!--- hello-world  ! --->").unwrap()), "/* hello-world  ! */".to_string());
  assert_eq!(format!("{:?}", parse_str("<!-- - hello-world  !  --->").unwrap()), "/* - hello-world  !  - */".to_string());
}

impl Parse for Child {
  type Context = Context;
  fn parse<C: Cursor>(cursor: C, ctx: &mut Self::Context) -> Result<(Self, C::Marker)> {
    if let Ok((k, cursor_next)) = Comment::parse(cursor.clone(), ctx.as_ctx()) {
      Ok((Child::C(k), cursor_next))
    } else if let Ok((k, cursor_next)) = TemplateInner::parse(cursor.clone(), ctx) {
      Ok((Child::T(k), cursor_next))
    } else if let Ok((k, cursor_next)) = Mustache::parse(cursor.clone(), ctx.as_ctx()) {
      Ok((Child::M(k), cursor_next))
    } else {
      error(cursor.span(), "not valid child")
    }
  }
}

#[allow(dead_code)]
pub fn parse_litstr(s: String) -> Option<String> {
  let mut state = State::S0;
  enum Escape {
    E1 /* \ */, E2 /* \u */, E3(String) /* \u{ */, Complete(char),
  }
  fn parse_escape(iter: &mut std::str::Chars<'_>, mut state: Escape) -> std::result::Result<char, &'static str> {
    for i in iter {
      state = match (state, i) {
        (Escape::E1, 'u') => Escape::E2,
        (Escape::E2, '{') => Escape::E3(String::new()),
        (Escape::E2, _) => return Err("expect { after \\u"),
        (Escape::E3(s), '}') => {
          let n = u32::from_str_radix(&s, 16).map_err(|_| "parse escape failed")?;
          let c = std::char::from_u32(n).ok_or("escape out of range")?;
          Escape::Complete(c)
        },
        (Escape::E3(mut s), c) => { s.push(c); Escape::E3(s) },
        (Escape::E1, '0') => Escape::Complete('\0'),
        (Escape::E1, 't') => Escape::Complete('\t'),
        (Escape::E1, 'r') => Escape::Complete('\r'),
        (Escape::E1, 'n') => Escape::Complete('\n'),
        (Escape::E1, c) => Escape::Complete(c),
        (Escape::Complete(_), _) => unreachable!(),
      };
      if let Escape::Complete(c) = state {
        return Ok(c)
      }
    }
    Err("unexpected eof")
  }
  enum State {
    S0, S1(char, String) /* " */, S2(char, String, Escape),
    R1(usize) /* r */, R2(usize, String) /* r#" */, R3(usize, String, usize),
    Complete(String, String),
  }
  let mut iter = s.chars();
  while let Some(c) = iter.next() {
    state = match (state, c) {
      (State::S0, '"') => State::S1(c, String::new()),
      (State::S1(q, s), '"') => State::Complete(q.to_string(), s),
      (State::S1(q, s), '\\') => State::S2(q, s, Escape::E1),
      (State::S1(q, mut s), c) => { s.push(c); State::S1(q, s) },

      (State::S0, 'r') => State::R1(0),
      (State::R1(i), '#') => State::R1(i+1),
      (State::R1(i), '"') if i > 0 => State::R2(i, String::new()),
      (State::R2(i, s), '"') => State::R3(i, s, 0),
      (State::R2(i, mut s), c) => { s.push(c); State::R2(i, s) }
      (State::R3(i, s, j), '#') if j+1 == i => State::Complete("#".repeat(i), s),
      (State::R3(i, s, j), '#') => State::R3(i, s, j+1),
      (State::R3(i, mut s, j), c) => {
        s.push('"');
        s.push_str(&"#".repeat(j));
        s.push(c);
        State::R2(i, s)
      }

      (State::S0, _) => return None,
      (State::R1(_), _) => return None,
      (State::S2(_, _, _), _) => unreachable!(),
      (State::Complete(_, _), _) => unreachable!(),
    };
    state = match state {
      State::S2(q, mut s, s2) => {
        let c = parse_escape(&mut iter, s2).expect("lit escape");
        s.push(c);
        State::S1(q, s)
      },
      _ => state,
    };
    if let State::Complete(_, s) = state {
      return Some(s)
    }
  }
  None
}

#[cfg(test)]
const TEMPLATE: &'static str = r##"
<template>
  <Grid :a="" b:="100" c$="* *" v-if:b="" @c=r#""c""# >
    <Text v-for="x in y" key:="x.id" v-bind:[var]="value" />
    {{{ func($ctx, $id) }}}
    <!-- {{=<% %>=}} -->
    <!-- <% () %> -->
    <!-- <%={{ }}=%> -->
  </Grid>
</template>
  "##;

#[test]
fn test_template() {
  let t = parse_str::<Template>(TEMPLATE).unwrap();
  println!("{:?}", t);
}
