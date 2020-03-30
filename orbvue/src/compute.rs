use std::fmt::Debug;
use std::collections::BTreeMap;
use std::any::TypeId;
use std::rc::Rc;
use crate::vue::*;
pub use paste::item as paste_item;
// pub struct ComputedSource<P>(&'static str, Entity, std::marker::PhantomData<P>);
// impl<P: Component + Debug> IntoPropertySource<P> for ComputedSource<P> {
//   fn into_source(self) -> PropertySource<P> {
//     PropertySource::KeySource(self.0.to_string(), self.1)
//   }
// }
// pub fn source<P: Component + Debug>(k: &'static str, i: Entity) -> ComputedSource<P> {
//   ComputedSource(k, i, Default::default())
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ComputedKey(pub &'static str);
impl ComputedKey {
  const PREFIX: &'static str = "__computed";
  #[inline]
  pub fn build(&self, field: &'static str) -> String {
    format!("{}.{}.{}", Self::PREFIX, self.0, field)
  }
  pub fn deps(&self) -> String { self.build("deps") }
  pub fn value(&self) -> String { self.build("value") }
  pub fn handler(&self) -> String { self.build("handler") }
  pub fn default_handler(&self) -> String { self.build("default") }
  pub fn from_vec<Iter: IntoIterator<Item=&'static str>>(iter: Iter) -> Vec<Self> {
    iter.into_iter().map(Self).collect()
  }
}
/// a prop/state/computed xxx is store at __computed.xxx.value
/// if xxx is computed, it would not be a "link" (or shared component in dces)
/// the dependencies could be link
#[derive(Clone)]
pub struct ComputeHandler(Rc<dyn Fn(&mut StringComponentStore, Entity) -> bool>);

#[derive(Clone)]
pub struct PropInitHandler(Rc<dyn Fn(Entity) -> ComponentBox>);

impl std::fmt::Debug for ComputeHandler {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let raw_pointer = format!("{:?}", self.0.as_ref() as *const _ as *const ());
    f.debug_tuple("ComputeHandler").field(&raw_pointer).finish()
  }
}

impl std::fmt::Debug for PropInitHandler {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let raw_pointer = format!("{:?}", self.0.as_ref() as *const _ as *const ());
    f.debug_tuple("ComputeHandler").field(&raw_pointer).finish()
  }
}

impl PropInitHandler {
  pub fn from<F: Fn(Entity) -> ComponentBox + 'static>(f: F) -> Self {
    Self(Rc::new(f))
  }
  pub fn call(&self, id: Entity) -> ComponentBox {
    self.0(id)
  }
}

impl ComputeHandler {
  pub fn from<F: Fn(&mut StringComponentStore, Entity) -> bool + 'static>(f: F) -> Self {
    Self(Rc::new(f))
  }
  pub fn get<P: Component + Debug>(store: &StringComponentStore, key: ComputedKey, id: Entity) -> &P {
    store.get(&key.value(), id).expect("key not found")
  }
  pub fn set<P: Component + Debug>(store: &mut StringComponentStore, key: ComputedKey, id: Entity, value: P, replace: impl Fn(&mut P, P) -> bool) -> bool {
    let key = key.value();
    match store.get_mut(&key, id) {
      Ok(origin) => {
        replace(origin, value)
      }
      _ => {
        store.register(key, id, value);
        true
      }
    }
  }
}

pub enum ComputeBox {
  Computed(ComputeHandler),
  Prop(PropInitHandler),
  State(PropInitHandler),
}

impl From<ComputeHandler> for ComputeBox {
  fn from(handler: ComputeHandler) -> Self {
    ComputeBox::Computed(handler)
  }
}

pub struct ComputedProp {
  pub name: ComputedKey,
  pub typeid: TypeId,
  pub dep: ComputeDep,
  pub handler: ComputeBox,
}
#[derive(Debug, Clone)]
pub struct ComputeDep {
  pub downstream: Vec<ComputedKey>,
  pub upstream: Vec<ComputedKey>,
}

impl std::fmt::Debug for ComputedProp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ComputedProp")
      .field("name", &self.name)
      .field("typeid", &self.typeid)
      .field("upstream", &self.dep.upstream)
      .field("downstream", &self.dep.downstream)
      .finish()
  }
}

impl ComputedProp {
  pub fn build_template<W: AddComponent>(&self, mut this: W, id: Entity) -> W {
    this = match &self.handler {
      ComputeBox::Computed(handler) => {
        this.add_component_value(self.name.handler(), handler.clone())
      },
      ComputeBox::Prop(_) =>
        this.add_shared_component(self.typeid, self.name.value(), self.name.0.to_string(), id),
      ComputeBox::State(handler) => {
        let value = handler.call(id);
        this.add_component_value(self.name.default_handler(), handler.clone())
            .add_component_box( self.name.value(), value)
      }
    };
    this.add_component_value(self.name.deps(), self.dep.clone())
  }
}

pub struct BuildModel<M: Vue> {
  props: BTreeMap<ComputedKey, ComputedProp>,
  order: Vec<ComputedKey>,
  _marker: std::marker::PhantomData<M>,
}
impl<M: Vue> Default for BuildModel<M> {
  fn default() -> Self {
    Self {
      props: BTreeMap::new(),
      order: Vec::new(),
      _marker: Default::default(),
    }
  }
}

impl<M: Vue> BuildModel<M> {
  pub fn add_handler<P: Component + Debug>(&mut self, name: ComputedKey, deps: Vec<ComputedKey>, handler: ComputeHandler) {
    let prop = ComputedProp {
      name, typeid: TypeId::of::<P>(), dep: ComputeDep { upstream: deps, downstream: vec![] }, handler: ComputeBox::Computed(handler),
    };
    self.add_prop(prop);
  }

  pub fn add_prop(&mut self, prop: ComputedProp) {
    let name = prop.name;
    assert!(!self.props.contains_key(&name), "you should not add computed props multiple times: {:?}", name);
    // TODO: fix downstream here?
    for up in &prop.dep.upstream {
      match self.props.get_mut(up) {
        Some(up) => up.dep.downstream.push(prop.name),
        None => panic!("upstream {:?} not exists for {:?}", up, name),
      }
    }
    self.props.insert(name, prop);
    self.order.push(name);
  }

  pub fn add_prop_unchecked(&mut self, prop: ComputedProp) {
    let name = prop.name;
    assert!(!self.props.contains_key(&name), "you should not add computed props multiple times: {:?}", name);
    // TODO: fix downstream here?
    self.props.insert(name, prop);
  }

  pub fn build_template(&self, mut this: M::Widget, id: Entity) -> M::Widget {
    for v in self.props.values() {
      this = v.build_template(this, id)
    }
    this
  }
}

#[macro_export]
macro_rules! into_computed_prop {
  ($name:ident) => {
    impl From<$name> for $crate::compute::ComputedProp {
      fn from(_: $name) -> Self {
        Self {
          name: $crate::compute::ComputedKey($name::NAME),
          typeid: $name::typeid(),
          dep: $crate::compute::ComputeDep {
            upstream: $name::deps(),
            downstream: vec![]
          },
          handler: $name::handler(),
        }
      }
    }
  };
}

#[macro_export]
macro_rules! prop_object {
  ($name:ident!: $ty:ty $(=> $example:expr)?) => {
    prop_object!(@stage1 $name: $ty [@expr { panic!("props {} required but not set", Self::NAME) }] $(=> $example)? );
  };
  ($name:ident: $ty:ty $(=> $example:expr)?) => {
    prop_object!(@stage1 $name: $ty [@expr { <$ty>::default() }] $(=> $example)? );
  };
  ($name:ident: $ty:ty = $expr:expr $(=> $example:expr)?) => {
    prop_object!(@stage1 $name: $ty [@expr { $expr }] $(=> $example)? );
  };
  (@stage1 $name:ident: $ty:ty [$($tt:tt)*]) => {
    prop_object!(@stage_impl $name: $ty [$($tt)* @example { Self::init() }] );
  };
  (@stage1 $name:ident: $ty:ty [$($tt:tt)*] => $example:expr) => {
    prop_object!(@stage_impl $name: $ty [$($tt)* @example { $example }] );
  };
  (@stage_impl $name:ident: $ty:ty [@expr {$($expr:tt)*} @example {$($example:tt)*}]) => { $crate::compute::paste_item!{
    #[allow(non_camel_case_types)]
    pub struct [<prop_ $name>];
    impl [<prop_ $name>] {
      pub const NAME: &'static str = stringify!($name);
      pub fn init() -> $ty {
        $($expr)*
      }
      #[cfg(test)]
      #[allow(dead_code)]
      fn example() -> $ty {
        $($example)*
      }
      #[allow(clippy::let_unit_value)]
      fn wrapper(_id: $crate::vue::Entity) -> $crate::vue::ComponentBox {
        let $name = Self::init();
        $crate::vue::ComponentBox::new($name)
      }
      pub fn typeid() -> ::core::any::TypeId {
        ::core::any::TypeId::of::<$ty>()
      }
      pub fn handler() -> $crate::compute::ComputeBox {
        $crate::compute::ComputeBox::Prop($crate::compute::PropInitHandler::from(Self::wrapper))
      }
      pub fn deps() -> Vec<$crate::compute::ComputedKey> {
        vec![]
      }
    }
    into_computed_prop!([<prop_ $name>]);
  } };
  ($($tt:tt)*) => { compile_error!(concat!("macro_error prop_object!: ", stringify!($($tt)*))); };
}
#[macro_export]
macro_rules! state_object {
  ($name:ident: $ty:ty) => {
    state_object!($name: $ty = Default::default());
  };
  ($name:ident: $ty:ty = $expr:expr) => { $crate::compute::paste_item!{
    #[allow(non_camel_case_types)]
    pub struct [<state_ $name>];
    impl [<state_ $name>] {
      pub const NAME: &'static str = stringify!($name);
      pub fn init() -> $ty {
        $expr
      }
      fn wrapper(_id: $crate::vue::Entity) -> $crate::vue::ComponentBox {
        let $name = Self::init();
        $crate::vue::ComponentBox::new($name)
      }
      pub fn typeid() -> ::core::any::TypeId {
        ::core::any::TypeId::of::<$ty>()
      }
      pub fn handler() -> $crate::compute::ComputeBox {
        $crate::compute::ComputeBox::State($crate::compute::PropInitHandler::from(Self::wrapper))
      }
      pub fn deps() -> Vec<$crate::compute::ComputedKey> {
        vec![]
      }
    }
    into_computed_prop!([<state_ $name>]);
  } };
}
#[macro_export]
macro_rules! compute_object {
  ($name:ident [$($deps:ident: &$depty:ty),* $(,)?] -> $ty:ty { $($tt:tt)* }) => { $crate::compute::paste_item!{
    #[allow(non_camel_case_types)]
    pub struct [<compute_ $name>];
    impl [<compute_ $name>] {
      pub const NAME: &'static str = stringify!($name);
      #[allow(clippy::ptr_arg)]
      pub fn call($($deps:&$depty),*) -> $ty {
        $($tt)*
      }
      fn replace(src: &mut $ty, to: $ty) -> bool {
        if &to == src {
          return false
        }
        std::mem::replace(src, to);
        true
      }
      fn wrapper(store: &mut $crate::vue::StringComponentStore, id: $crate::vue::Entity) -> bool {
        use $crate::compute::{ComputedKey, ComputeHandler};
        $(
          let $deps = ComputeHandler::get::<$depty>(store, ComputedKey(stringify!($deps)), id);
        )*
        let [<new_ $name>] = Self::call($($deps),*);
        ComputeHandler::set(store, ComputedKey(Self::NAME), id, [<new_ $name>], Self::replace)
      }
      pub fn typeid() -> ::core::any::TypeId {
        ::core::any::TypeId::of::<$ty>()
      }
      pub fn handler() -> $crate::compute::ComputeBox {
        $crate::compute::ComputeHandler::from(Self::wrapper).into()
      }
      pub fn deps() -> Vec<$crate::compute::ComputedKey> {
        let deps: Vec<&'static str> = vec![$(stringify!($deps)),*];
        $crate::compute::ComputedKey::from_vec(deps)
      }
    }
    $crate::into_computed_prop!([<compute_ $name>]);
  } };
}

#[macro_export]
macro_rules! model_apply {
  (@is_run @init_panic $_:tt $($tt:tt)*) => { #[test] #[should_panic] $($tt)* };
  (@is_run $($tt:tt)*) => { #[test] $($tt)* };
  (@test_struct [
    @name $name:ident
    @props { $($name1:ident: $ty1:ty {$($_1:tt)*},)* }
    @states { $($name2:ident: $ty2:ty,)* }
    @compute { $($name3:ident:$ty3:ty [$($_2:tt)*],)* }
    $(@sorted $tt:tt)?
  ]) => { $crate::compute::paste_item! {
    #[allow(non_camel_case_types)]
    struct [<test_ $name _struct>] {
      $($name1:$ty1,)* $($name2:$ty2,)* $($name3:$ty3,)*
    }
  } };
  (@test_func [
    @name $name:ident
    @props { $($name1:ident: $ty1:ty {$(@uninit $uninit:tt)?},)* }
    @states { $($name2:ident: $ty2:ty,)* }
    @compute { $($name3:ident:$ty3:ty [$($dep3:ident),*],)* }
    @sorted $tt:tt
    $(@init_panic $tpanic:tt)?
  ]) => { $crate::orbvue_apply!{ $crate::compute::paste_item! {
    #[allow(non_snake_case, clippy::let_unit_value)]
    #[test]
    {<condition>
      {#[should_panic] fn [<test_ $name _init__should_panic>]()}
      {fn [<test_ $name _init>]()}
      $($($uninit)?)*
    } {
      $(let $name1:$ty1 = [<prop_ $name1>]::example();)*
      $(let $name2:$ty2 = [<state_ $name2>]::init();)*
      $(let $name3:$ty3 = [<compute_ $name3>]::call($(&$dep3),*);)*
    }
  } } };
  (@build_model [
    @name $name:ident
    @props { $($name1:ident: $ty1:ty {$(@uninit $uninit:tt)?},)* }
    @states { $($name2:ident: $ty2:ty,)* }
    @compute { $($name3:ident:$ty3:ty [$($dep3:ident),*],)* }
    @sorted $tt:tt
    $(@init_panic $tpanic:tt)?
  ]) => { $crate::orbvue_apply!{ $crate::compute::paste_item! {
    pub fn create<M: $crate::vue::Vue>() -> $crate::compute::BuildModel<M> {
      #[allow(unused_mut)]
      let mut model = $crate::compute::BuildModel::default();
      $(model.add_prop([<prop_ $name1>].into());)*
      $(model.add_prop([<state_ $name2>].into());)*
      $(model.add_prop([<compute_ $name3>].into());)*
      model
    }
  } } };
  (@$($tt:tt)*) => { compile_error!(concat!("macro_error model_apply!: @", stringify!($($tt)*))); };
}

#[macro_export]
macro_rules! model {
  (@stage0[], name: $name:ident $($tt:tt)*) => { model!(@stage0_impl[@name $name] $($tt)*); };
  (@stage0[] $($tt:tt)*) => { model!(@stage0_impl[@name Model] $($tt)*); };
  (@stage0_impl[@name $name:ident] $($tt:tt)*) => {
    // $crate::compute::paste_item! {
    //   #[allow(dead_code, non_snake_case, non_camel_case_types, unused_imports, unused_variables)]
    //   mod [<__model_ $name>] {
        model!(@stage1[@name Model] $($tt)*);
    //   }
    //   #[allow(unused_imports)]
    //   use [<__model_ $name>] as [<create_ $name>];
    // }
  };
  (@stage1 $r:tt, props: { $($ti:tt)* } $($tt:tt)*) => {
    model!(@stage1_loop $r @props{ }, props: {$($ti)*,} $($tt)*);
  };
  (@stage1 $r:tt $($tt:tt)*) => {
    model!(@stage1_impl $r @props{ } $($tt)*);
  };
  // TODO should have a issue about $(!)?
  (@stage1_loop $r:tt @props{ $($tr:tt)* }, props: { $name:ident:$ty:ty $(= $expr:expr)? $(=> $example:expr)?, $($ti:tt)* } $($tt:tt)*) => {
    model!(@stage1_loop $r @props{ $($tr)* $name():$ty $(= $expr)? $(=> $example)?, }, props: {$($ti)*} $($tt)*);
  };
  (@stage1_loop $r:tt @props{ $($tr:tt)* }, props: { $name:ident!:$ty:ty $(= $expr:expr)? $(=> $example:expr)?, $($ti:tt)* } $($tt:tt)*) => {
    model!(@stage1_loop $r @props{ $($tr)* $name(!):$ty $(= $expr)? $(=> $example)?, }, props: {$($ti)*} $($tt)*);
  };
  (@stage1_loop $r:tt @props{ $($tr:tt)* }, props: { $(,)? } $($tt:tt)*) => {
    model!(@stage1_impl $r @props{ $($tr)* } $($tt)*);
  };
  (@stage1_impl[$($r:tt)*] @props{ $($name:ident($($to:tt)?):$ty:ty $(= $expr:expr)?),* $(,)? } $($tt:tt)*) => {
    $($crate::prop_object!($name$($to)?:$ty $(= $expr)?);)*
    $crate::orbvue_apply!{
      model!(@stage2[$($r)* @props{ $($name:$ty {{<condition> {@uninit !} {} $($to)?}},)* }] $($tt)*);
    }
  };
  (@stage2 $r:tt, states: { $($t1:tt)* } $($tt:tt)*) => {
    model!(@stage2_impl $r @states{ $($t1)* } $($tt)*);
  };
  (@stage2 $r:tt $($tt:tt)*) => {
    model!(@stage2_impl $r @states{ } $($tt)*);
  };
  (@stage2_impl[$($r:tt)*] @states{ $($name:ident:$ty:ty $(= $expr:expr)?),* $(,)? } $($tt:tt)*) => {
    $($crate::state_object!($name:$ty $(= $expr)?);)*
    model!(@stage3[$($r)* @states{ $($name:$ty,)* }] $($tt)*);
  };
  (@stage3 $r:tt, compute: { $($t1:tt)* } $($tt:tt)*) => {
    model!(@stage3_impl $r @compute{ $($t1)* } $($tt)*);
  };
  (@stage3 $r:tt $($tt:tt)*) => {
    model!(@stage3_impl $r @compute{ } $($tt)*);
  };
  (@stage3_impl[$($r:tt)*] @compute{ $($name:ident [$($deps:ident: &$depty:ty),* $(,)?] -> $ty:ty { $($ttt:tt)* }),* $(,)? } $($tt:tt)*) => {
    $($crate::compute_object!($name [$($deps: &$depty),*] -> $ty { $($ttt)* });)*
    model!(@stage4[$($r)* @compute{ $($name:$ty [$($deps),*],)*}] $($tt)*);
  };
  (@stage4 $r:tt $(,)?) => { $crate::orbvue_apply!{
    model!(@stage_final {<model_sort> $r });
  } };
  (@stage_final [@name $name:ident $($r:tt)*]) => {$crate::compute::paste_item! {
    #[cfg(test)]
    mod test {
      use super::*;

      $crate::model_apply!(@test_struct [@name $name $($r)*]);

      $crate::model_apply!(@test_func [@name $name $($r)*]);
    }
    $crate::model_apply!(@build_model [@name $name $($r)*]);
  } };
  (@$($tt:tt)*) => { compile_error!(concat!("macro_error model!: @", stringify!($($tt)*))); };
  ($($tt:tt)*) => { model!(@stage0[], $($tt)*); };
}

#[cfg(test)]
mod model_test {
  mod m0 { model!{ props: {}, } }
  model!{ name: m1 }
  model!{ name: m2, props: {} }
  model!{ name: m3, states: {} }
  model!{ name: m4, compute: {} }
  model!{ name: m5, props: {}, states: {} }
  model!{
    name: model,
    props: {
      a: usize = 5,
      uninit!: (),
      init: (),
      s: String = "hello!".into(),
    },
    states: {
      current: usize = 3,
      s1: String,
    },
    compute: {
      char [vec: &Vec<char>, xxx: &usize,] -> Option<char> { vec.get(*xxx).cloned() },
      xxx [current: &usize,] -> usize { *current - 1 },
      vec [s: &String] -> Vec<char> { s.chars().collect() },
    },
  }

  use __model_model::*;
  #[test]
  fn test_prop_object() {
    use super::TypeId;
    assert_eq!(prop_a::NAME, "a");
    assert_eq!(prop_a::init(), 5);
    assert_eq!(prop_a::typeid(), TypeId::of::<usize>());
    assert_eq!(prop_a::deps(), vec![]);

    assert_eq!(prop_uninit::NAME, "uninit");
    assert_eq!(prop_uninit::typeid(), TypeId::of::<()>());
    assert_eq!(prop_uninit::deps(), vec![]);

    assert_eq!(prop_init::NAME, "init");
    prop_init::init();
    assert_eq!(prop_init::typeid(), TypeId::of::<()>());
    assert_eq!(prop_init::deps(), vec![]);

    assert_eq!(prop_s::NAME, "s");
    assert_eq!(prop_s::init(), "hello!");
    assert_eq!(prop_s::typeid(), TypeId::of::<String>());
    assert_eq!(prop_s::deps(), vec![]);
  }

  #[test]
  #[should_panic]
  fn test_uninit() {
    prop_uninit::init();
  }

  #[test]
  fn test_state_object() {
    use super::TypeId;
    assert_eq!(state_current::NAME, "current");
    assert_eq!(state_current::init(), 3);
    assert_eq!(state_current::typeid(), TypeId::of::<usize>());
    assert_eq!(state_current::deps(), vec![]);

    assert_eq!(state_s1::NAME, "s1");
    assert_eq!(state_s1::init(), "");
    assert_eq!(state_s1::typeid(), TypeId::of::<String>());
    assert_eq!(state_s1::deps(), vec![]);
  }

  #[test]
  fn test_compute_object() {
    use super::{ComputedKey, TypeId};
    assert_eq!(compute_xxx::NAME, "xxx");
    assert_eq!(compute_xxx::call(&5), 4);
    assert_eq!(compute_xxx::deps(), ComputedKey::from_vec(vec!["current"]));
    assert_eq!(compute_xxx::typeid(), TypeId::of::<usize>());

    assert_eq!(compute_char::NAME, "char");
    assert_eq!(compute_char::call(&"hello!".chars().collect(), &4), Some('o'));
    assert_eq!(compute_char::deps(), ComputedKey::from_vec(vec!["vec", "xxx"]));
    assert_eq!(compute_char::typeid(), TypeId::of::<Option<char>>());
  }
}

#[cfg(test)]
mod test {
  prop_object!(a: usize => 5);
  prop_object!(uninit!: ());
  prop_object!(init: ());
  prop_object!(s: String = "hello!".into());

  state_object!(current: usize);
  state_object!(s1: String = "hello1".into());

  compute_object!(xxx [current: &usize,] -> usize { *current - 1 });
  compute_object!(char [s: &Vec<char>, xxx: &usize,] -> Option<char> { s.get(*xxx).cloned() });

  #[test]
  fn test_prop_object() {
    use super::TypeId;
    assert_eq!(prop_a::NAME, "a");
    assert_eq!(prop_a::init(), 0);
    assert_eq!(prop_a::example(), 5);
    assert_eq!(prop_a::typeid(), TypeId::of::<usize>());
    assert_eq!(prop_a::deps(), vec![]);

    assert_eq!(prop_uninit::NAME, "uninit");
    assert_eq!(prop_uninit::typeid(), TypeId::of::<()>());
    assert_eq!(prop_uninit::deps(), vec![]);

    assert_eq!(prop_init::NAME, "init");
    prop_init::init();
    assert_eq!(prop_init::typeid(), TypeId::of::<()>());
    assert_eq!(prop_init::deps(), vec![]);

    assert_eq!(prop_s::NAME, "s");
    assert_eq!(prop_s::init(), "hello!");
    assert_eq!(prop_s::typeid(), TypeId::of::<String>());
    assert_eq!(prop_s::deps(), vec![]);
  }

  #[test]
  #[should_panic]
  fn test_uninit() {
    prop_uninit::init();
  }

  #[test]
  fn test_state_object() {
    use super::TypeId;
    assert_eq!(state_current::NAME, "current");
    assert_eq!(state_current::init(), 0);
    assert_eq!(state_current::typeid(), TypeId::of::<usize>());
    assert_eq!(state_current::deps(), vec![]);

    assert_eq!(state_s1::NAME, "s1");
    assert_eq!(state_s1::init(), "hello1");
    assert_eq!(state_s1::typeid(), TypeId::of::<String>());
    assert_eq!(state_s1::deps(), vec![]);
  }

  #[test]
  fn test_compute_object() {
    use super::{ComputedKey, TypeId};
    assert_eq!(compute_xxx::NAME, "xxx");
    assert_eq!(compute_xxx::call(&5), 4);
    assert_eq!(compute_xxx::deps(), ComputedKey::from_vec(vec!["current"]));
    assert_eq!(compute_xxx::typeid(), TypeId::of::<usize>());

    assert_eq!(compute_char::NAME, "char");
    assert_eq!(compute_char::call(&"hello!".chars().collect(), &4), Some('o'));
    assert_eq!(compute_char::deps(), ComputedKey::from_vec(vec!["s", "xxx"]));
    assert_eq!(compute_char::typeid(), TypeId::of::<Option<char>>());
  }

  #[derive(Default)]
  struct M;
  impl super::Vue for M {
    type Widget = W;
  }

  #[derive(Default)]
  struct W(Vec<((std::any::TypeId, String), Box<dyn std::fmt::Debug>)>);
  impl super::AddComponent for W {
    fn add_shared_component(mut self, typeid: std::any::TypeId, key: String, source_key: String, source_id: super::Entity) -> Self {
      self.0.push(((typeid, key), Box::new((source_key, source_id)))); self
    }
    fn add_component_box(mut self, key: String, value: super::ComponentBox) -> Self {
      let (typeid, value) = value.consume();
      self.0.push(((typeid, key), Box::new((typeid, value)))); self
    }
    fn add_component_value<C: super::Component + super::Debug>(mut self, key: String, value: C) -> Self {
      let typeid = std::any::TypeId::of::<C>();
      self.0.push(((typeid, key), Box::new(value))); self
    }
  }

  #[test]
  fn test_model() {
    use super::*;
    let mut model = BuildModel::<M>::default();
    model.add_prop(prop_a.into());
    model.add_prop(state_current.into());
    model.add_prop(state_s1.into());
    model.add_prop(compute_xxx.into());
    model.add_prop(prop_s.into());
    model.add_prop(compute_char.into());
    let w = model.build_template(W::default(), Entity::from(0));
    for i in w.0 {
      println!("{:?}", i);
    }
  }
}
