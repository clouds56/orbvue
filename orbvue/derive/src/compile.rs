use std::collections::HashMap;
use std::sync::Arc;

use crate::common::*;
use crate::template::*;

use proc_macro2::{TokenStream, Ident, Span, TokenTree};

type DollarFn = dyn Fn(&Ident, &str) -> syn::Result<Vec<(Ident, TokenStream)>> + Send + Sync;
#[derive(Default, Clone)]
pub struct Context {
  pub dollar: HashMap<String, HashMap<String, Arc<DollarFn>>>,
  pub name: HashMap<String, String>,
}
impl Context {
  pub fn new() -> Self {
    DEFAULT_CONTEXT.clone()
  }
  pub fn get_dollar(&self, k1: &str, k2: &str) -> Option<&DollarFn> {
    self.dollar.get(k1)?.get(k2).map(|f| f.as_ref())
  }
  pub fn set_dollar(&mut self, k1: &str, k2: &str, v: &'static DollarFn) {
    self.dollar.entry(k1.to_string()).or_default().insert(k2.to_string(), Arc::new(v));
  }
  pub fn get_name(&self, k: &Ident) -> Option<Ident> {
    self.name.get(&k.to_string()).map(|v| Ident::new(v, k.span()))
  }
  pub fn set_name(&mut self, k: &str, v: String) {
    self.name.insert(k.to_string(), v);
  }
}

fn split_tokens<T: std::str::FromStr + quote::ToTokens>(s: &str) -> Vec<TokenStream> {
  let mut result = vec![];
  for s in s.split(' ') {
    if s.is_empty() { continue };
    let v = if let Ok(v) = s.parse::<T>() {
      quote!(#v)
    } else {
      quote!(#s)
    };
    result.push(v);
  }
  result
}

pub fn dollar_grid_rows(k: &Ident, s: &str) -> syn::Result<Vec<(Ident, TokenStream)>> {
  let value = split_tokens::<f64>(s);
  Ok(vec![(k.clone(), quote! {
    Rows::create()#(.row(#value))*.build()
  })])
}
pub fn dollar_grid_columns(k: &Ident, s: &str) -> syn::Result<Vec<(Ident, TokenStream)>> {
  let value = split_tokens::<f64>(s);
  Ok(vec![(k.clone(), quote! {
    Columns::create()#(.column(#value))*.build()
  })])
}

lazy_static! {
  pub static ref DEFAULT_CONTEXT: Context = {
    let mut ctx = Context::default();
    ctx.set_dollar("Grid", "rows", &dollar_grid_rows);
    ctx.set_dollar("Grid", "columns", &dollar_grid_columns);
    ctx
  };
}

impl Mustache {
  fn apply_item(item: &MustacheItem, ctx: &Context) -> TokenTree {
    match item {
      MustacheItem::K(k) => k.clone(),
      MustacheItem::G(d, v, span) => {
        use proc_macro2::Group;
        let v = v.iter().map(|i| Mustache::apply_item(i, ctx)).collect();
        let mut g = Group::new(*d, v);
        g.set_span(*span);
        g.into()
      },
      MustacheItem::P(p) => ctx.get_name(p).expect("get name").into(),
    }
  }

  pub fn apply(&self, ctx: &Context) -> TokenStream {
    match self.prefix {
      MustachePrefix::And | MustachePrefix::None =>
        self.content.iter().map(|i| Self::apply_item(i, ctx)).collect(),
    }
  }
}

pub fn compile<Tag: XmlTag>(t: &TemplateXml<Tag>, ctx: &Context) -> TokenStream {
  use IdentComponent as Component;
  let name = &t.name;
  let attrs = t.attrs.iter().map(|(k, v)| {
    if k.prefix.is_some() {
      unimplemented!("attr prefix not implemented");
    }
    let key = match &k.main {
      Component::I(t) => t,
      t @ Component::At | t @ Component::Colon | t @ Component::Dollar => panic!("unexpected component {:?}", t),
      _ => unimplemented!("other prefix not implemented"),
    };
    let value = match &k.suffix {
      Some(Component::Colon) => {
        match Mustache::parse_lit(v) {
          Ok(m) => {
            let v = m.apply(ctx);
            quote! { #v }
          },
          Err(e) => e.to_compile_error(),
        }
      },
      Some(Component::Dollar) => {
        let s = match v.value_str() {
          Ok(s) => s,
          Err(e) => return e.to_compile_error(),
        };
        let f= ctx.get_dollar(&name.to_string(), &key.to_string()).ok_or_else(|| format!("{}::{}", name, key)).expect("get_dollar");
        let v = match f(key, &s) {
          Ok(v) => v.into_iter().map(|(k, v)| quote! { .#k(#v) }).collect::<Vec<_>>(),
          Err(e) => return e.to_compile_error(),
        };
        return quote! { #(#v)* }
      },
      None => { let v = &v.lit; quote!{ #v } },
      t => panic!("unexpected suffix component {:?}", t),
    };
    quote! {
      .#key(#value)
    }
  }).collect::<Vec<_>>();
  let children = t.children.iter().filter_map(|c| {
    match c {
      Child::C(_) => None,
      Child::M(m) => {
        if let MustachePrefix::And = m.prefix {
          Some(m.apply(ctx))
        } else {
          unimplemented!("mustache without &");
        }
      },
      Child::T(t) => Some(compile(t, ctx)),
    }
  }).collect::<Vec<_>>();
  let ctx_name = ctx.get_name(&Ident::new("ctx", Span::call_site()));
  let (prefix, suffix) = match Tag::name() {
    Some("template") => (quote!(), quote!()),
    None => (quote!(#name::create()), quote!(.build(#ctx_name))),
    _ => unreachable!("unknown tag"),
  };

  quote!{
    #prefix #(#attrs)* #(.child(#children))* #suffix
  }
}

#[cfg(test)]
pub const TEMPLATE: &str = r##"
<template name="MainView" width:="212.0" height:="336.0" text="">
  <Grid rows$="72.0 *">
    <Container padding:="8.0" element="container" class="header" attach:="Grid::row(0)">
      <Grid>
        <ScrollViewer scroll_viewer_mode:=r#"("custom", "disabled")"#>
          <TextBlock width:="0.0" height:="14.0" text="" element="text-block" id="input" vertical_alignment="start" />
        </ScrollViewer>
        <TextBlock text:="$id" element="text-block" vertical_alignment="end" horizontal_alignment="end" />
      </Grid>
    </Container>
    <Container padding:="8.0" element="container" class="content" attach:="Grid::row(1)">
      <Grid columns$="48.0 4.0 48.0 4.0 48.0 4.0 48.0 4.0 48.0" rows$="48.0 4.0 48.0 4.0 48.0 4.0 48.0 4.0 48.0">
        <!-- row 0 -->
        {{& generate_operation_button($ctx, $id, 'C', false, 0, 5, 0) }}
        {{& generate_operation_button($ctx, $id, '/', true, 6, 1, 0) }}
        <!-- row 2 -->
        {{& generate_digit_button($ctx, $id, '7', false, 0, 1, 2) }}
        {{& generate_digit_button($ctx, $id, '8', false, 2, 1, 2) }}
        {{& generate_digit_button($ctx, $id, '9', false, 4, 1, 2) }}
        {{& generate_operation_button($ctx, $id, '*', true, 6, 1, 2) }}
        <!-- row 4 -->
        {{& generate_digit_button($ctx, $id, '4', false, 0, 1, 4) }}
        {{& generate_digit_button($ctx, $id, '5', false, 2, 1, 4) }}
        {{& generate_digit_button($ctx, $id, '6', false, 4, 1, 4) }}
        {{& generate_operation_button($ctx, $id, '-', true, 6, 1, 4) }}
        <!-- row 6 -->
        {{& generate_digit_button($ctx, $id, '1', false, 0, 1, 6) }}
        {{& generate_digit_button($ctx, $id, '2', false, 2, 1, 6) }}
        {{& generate_digit_button($ctx, $id, '3', false, 4, 1, 6) }}
        {{& generate_operation_button($ctx, $id, '+', true, 6, 1, 6) }}
        <!-- row 8 -->
        {{& generate_digit_button($ctx, $id, '0', false, 0, 3, 8) }}
        {{& generate_digit_button($ctx, $id, '.', false, 4, 1, 8) }}
        {{& generate_operation_button($ctx, $id, '=', true, 6, 1, 8) }}
      </Grid>
    </Container>
  </Grid>
</template>
  "##;

#[cfg(test)]
const SCRIPT: &str = r##"
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
  #[orbvue::template]
  fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
    #[orbvue="expr"]
    self
  }
}
</script>
  "##;

#[test]
fn test_compile() {
  use crate::common::parse_str;
  let mut ctx = Context::new();
  ctx.set_name("id", "__id__".to_string());
  ctx.set_name("ctx", "__ctx__".to_string());
  let t = parse_str::<Template>(TEMPLATE).unwrap();
  println!("{}", compile(&t, &ctx));
}

#[test]
fn test_gen_script() {
  use crate::script::{Script, ItemFnVisitor, Child, VisitMut};
  let s = parse_str::<Script>(SCRIPT).unwrap();
  let t = parse_str::<Template>(TEMPLATE).unwrap();
  let mut visitor = ItemFnVisitor::new(&t, Context::new());
  for Child::I(item) in s.children {
    let mut item = item.0;
    visitor.visit_item_mut(&mut item);
  }
}
