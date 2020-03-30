use proc_macro2::{TokenTree, TokenStream, Delimiter, Group};

fn check_lead(v: &[TokenTree]) -> Option<String> {
  if v.len() != 3 { return None; }
  match &v[0] {
    TokenTree::Punct(i) if i.as_char() == '<' => (),
    _ => return None,
  }
  match &v[2] {
    TokenTree::Punct(i) if i.as_char() == '>' => (),
    _ => return None,
  }
  match &v[1] {
    TokenTree::Ident(i) => Some(i.to_string()),
    _ => None,
  }
}

fn token_tree_group(g: Group, delimiter: Option<Delimiter>, stream: TokenStream) -> TokenTree {
  let span = g.span();
  let mut g = Group::new(delimiter.unwrap_or_else(|| g.delimiter()), stream);
  g.set_span(span);
  TokenTree::Group(g)
}

pub type Functor<'a> = &'a mut dyn FnMut(TokenStream) -> syn::Result<TokenStream>;
pub fn apply_brace(input: TokenStream, functors: &mut std::collections::HashMap<String, Box<Functor>>) -> TokenStream {
  input.into_iter().map(|i| {
    // println!("apply_brace token: {:?} {:?}", i.span(), i.to_string());
    match i {
      TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => {
        let mut stream = g.stream().into_iter().peekable();
        match stream.peek() {
          Some(TokenTree::Punct(c)) if c.as_char() == '<' => {
            let ahead: Vec<_> = (0..3).filter_map(|_| stream.next()).collect();
            if let Some(f) = check_lead(&ahead).and_then(|s| functors.get_mut(&s)) {
              let stream = match (*f)(stream.collect()) {
                Ok(s) => s,
                Err(e) => {
                  let mut stream = e.to_compile_error(); stream.extend(g.stream());
                  stream
                },
                // e => e.unwrap(),
              };
              token_tree_group(g, Some(Delimiter::None), stream)
            } else {
              let old_stream = stream;
              let mut stream: TokenStream = ahead.into_iter().collect();
              stream.extend(old_stream);
              token_tree_group(g, None, apply_brace(stream, functors))
            }
          }
          _ => {
            token_tree_group(g, None, apply_brace(stream.collect(), functors))
          }
        }
      },
      TokenTree::Group(g) => {
        let stream = g.stream();
        token_tree_group(g, None, apply_brace(stream, functors))
      }
      t => t,
    }
  }).collect()
}

pub mod functors {
  use syn::*;
  use crate::Spanable;
  use syn::parse::{Parse, ParseStream};
  use syn::punctuated::{Punctuated, Pair};
  use proc_macro2::{TokenStream, Span, TokenTree, Delimiter};
  use quote::ToTokens;

  #[derive(Clone)]
  pub struct Prop(pub Ident, pub Token![:], pub Type, pub Option<TokenTree>);
  impl Spanable for Prop {
    fn span(&self) -> Span { self.0.span() }
  }
  impl Parse for Prop {
    fn parse(input: ParseStream) -> Result<Self> {
      Ok(Self(input.parse()?, input.parse()?, input.parse()?, Self::parse_group(input)))
    }
  }
  impl ToTokens for Prop {
    fn to_tokens(&self, tokens: &mut TokenStream) {
      let Self(a, b, c, d) = self;
      tokens.extend(quote!(#a #b #c #d))
    }
  }
  impl Prop {
    pub fn name(&self) -> String { self.0.to_string() }
    pub fn parse_group(input: ParseStream) -> Option<TokenTree> {
      input.step(|cursor| {
        match cursor.token_tree() {
          Some((TokenTree::Group(g), c)) if g.delimiter() == Delimiter::Brace =>
            Ok((g.into(), c)),
          _ => Err(cursor.error("not brace group")),
        }
      }).ok()
    }
  }
  #[derive(Clone)]
  struct Compute {
    ident: Prop,
    deps_token: token::Bracket,
    deps: Punctuated<Ident, Token![,]>,
  }
  impl Spanable for Compute {
    fn span(&self) -> Span { self.ident.span() }
  }
  impl Parse for Compute {
    fn parse(input: ParseStream) -> Result<Self> {
      let content;
      Ok(Self {
        ident: input.parse()?,
        deps_token: bracketed!(content in input),
        deps: Punctuated::parse_terminated(&content)?,
      })
    }
  }
  impl ToTokens for Compute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
      self.ident.to_tokens(tokens);
      self.deps_token.surround(tokens, |tokens| self.deps.to_tokens(tokens));
    }
  }
  impl Compute {
    pub fn dep_names(&self) -> Vec<String> { self.deps.iter().map(|i| i.to_string()).collect() }
  }
  struct Model {
    bracket: token::Bracket,
    props_token: (Token![@], Ident, token::Brace),
    props: Punctuated<Prop, Token![,]>,
    states_token: (Token![@], Ident, token::Brace),
    states: Punctuated<Prop, Token![,]>,
    compute_token: (Token![@], Ident, token::Brace),
    compute: Punctuated<Compute, Token![,]>,
    other: TokenStream,
    sorted: Option<Punctuated<Prop, Token![,]>>,
  }
  impl Parse for Model {
    fn parse(input_all: ParseStream) -> Result<Self> {
      let input;

      let bracket = bracketed!(input in input_all);
      let mut content;
      Ok(Self {
        bracket,
        props_token: (input.parse()?, input.parse()?, braced!(content in input)),
        props: Punctuated::parse_terminated(&content)?,
        states_token: (input.parse()?, input.parse()?, braced!(content in input)),
        states: Punctuated::parse_terminated(&content)?,
        compute_token: (input.parse()?, input.parse()?, braced!(content in input)),
        compute: Punctuated::parse_terminated(&content)?,
        other: input.parse()?,
        sorted: None,
      })
    }
  }
  impl ToTokens for Model {
    fn to_tokens(&self, tokens: &mut TokenStream) {
      self.bracket.surround(tokens, |tokens| {
        let ((a, b, c), d) = (&self.props_token, &self.props);
        tokens.extend(quote! { #a#b });
        c.surround(tokens, |t| d.to_tokens(t));
        let ((a, b, c), d) = (&self.states_token, &self.states);
        tokens.extend(quote! { #a#b });
        c.surround(tokens, |t| d.to_tokens(t));
        let ((a, b, c), d) = (&self.compute_token, &self.compute);
        tokens.extend(quote! { #a#b });
        c.surround(tokens, |t| d.to_tokens(t));
        if let Some(sorted) = &self.sorted {
          tokens.extend(quote!{
            @sorted { #sorted }
          })
        }
        tokens.extend(self.other.clone());
      })
    }
  }
  impl Model {
    fn pair_to_tuple<T: Spanable + Clone>(pair: Pair<&T, &Token![,]>) -> (T, Token![,]) {
      match pair {
        Pair::Punctuated(i, v) => { (i.clone(), *v) },
        Pair::End(i) => { (i.clone(), token::Comma { spans: [i.span()] }) }
      }
    }
    pub fn sort(&mut self) {
      use std::collections::{HashSet, VecDeque};
      let mut queue = self.props.pairs().chain(self.states.pairs())
        .map(|pair| Some(Self::pair_to_tuple(pair))).collect::<VecDeque<_>>();
      let mut remain = self.compute.pairs().map(|pair| {
        let pair = Self::pair_to_tuple(pair);
        (pair.0.dep_names().into_iter().collect::<HashSet<_>>(), pair)
      }).collect::<Vec<_>>();
      let mut result = Vec::new();
      let mut result2 = Vec::new();

      queue.push_front(None);
      while let Some(pair) = queue.pop_front() {
        let name = match pair {
          Some((prop, comma)) => {
            let name = prop.name();
            result.push((prop, comma));
            Some(name)
          },
          _ => None,
        };
        // TODO: drain_filter: https://github.com/rust-lang/rust/issues/43244
        // https://doc.rust-lang.org/std/vec/struct.Vec.html#method.drain_filter
        // queue.extend(remain.drain_filter(|(deps, _)| { ... }).map(|(_, (i, comma))| (i.ident, comma)));
        let mut i = 0;
        while i != remain.len() {
          let (deps, _) = &mut remain[i];
          if let Some(name) = &name {
            deps.remove(name);
          }
          if deps.is_empty() {
            let (_, (compute, comma)) = remain.remove(i);
            queue.push_back(Some((compute.ident.clone(), comma)));
            result2.push((compute, comma));
          } else {
            i += 1;
          }
        }
      }
      self.compute = result2.into_iter().map(|(i, v)| Pair::Punctuated(i, v)).collect();
      self.sorted = Some(result.into_iter().map(|(i, v)| Pair::Punctuated(i, v)).collect());
    }
  }

  pub fn model_sort(input: TokenStream) -> Result<TokenStream> {
    let mut model: Model = syn::parse2(input)?;
    model.sort();
    Ok(quote!{ #model })
  }

  pub fn condition(input: TokenStream) -> Result<TokenStream> {
    let mut stream = input.into_iter();
    let true_branch = stream.next();
    let false_branch = stream.next();
    let count = stream.count();
    // println!("condition {:?} {}", input.to_string(), count);
    let result = if count > 0 {
      true_branch
    } else {
      false_branch
    };
    let result = match &result {
      Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => g.stream(),
      _ => result.into_token_stream(),
    };
    Ok(result)
  }
}
