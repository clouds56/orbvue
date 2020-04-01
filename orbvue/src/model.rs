use std::any::TypeId;
use std::collections::BTreeMap;
use std::fmt::Debug;
use crate::vue::*;
use crate::compute::*;

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

  pub fn build_template(&self, mut this: M::Widget, id: Entity) -> M::Widget {
    for name in &self.order {
      this = self.props.get(name).unwrap().build_template(this, id)
    }
    this
  }
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
  ]) => { $crate::orbvue_apply!{
    #[allow(non_camel_case_types)]
    struct {<:ident> test_ $name _struct} {
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
  ]) => { $crate::orbvue_apply!{
    #[allow(non_snake_case, clippy::let_unit_value)]
    #[test]
    {<:condition>
      {#[should_panic] fn {<:ident> test_ $name _init__should_panic}()}
      {fn {<:ident> test_ $name _init}()}
      $($($uninit)?)*
    } {
      $(let $name1:$ty1 = {<:ident> prop_ $name1}::example();)*
      $(let $name2:$ty2 = {<:ident> state_ $name2}::init();)*
      $(let $name3:$ty3 = {<:ident> compute_ $name3}::call($(&$dep3),*);)*
    }
  } };
  (@build_model [
    @name $name:ident
    @props { $($name1:ident: $ty1:ty {$(@uninit $uninit:tt)?},)* }
    @states { $($name2:ident: $ty2:ty,)* }
    @compute { $($name3:ident:$ty3:ty [$($dep3:ident),*],)* }
    @sorted $tt:tt
    $(@init_panic $tpanic:tt)?
  ]) => { $crate::orbvue_apply!{
    pub struct $name {}
    impl $name {
      pub fn create<M: $crate::vue::Vue>() -> $crate::model::BuildModel<M> {
        #[allow(unused_mut)]
        let mut model = $crate::model::BuildModel::default();
        $(model.add_prop({<:ident> prop_ $name1}.into());)*
        $(model.add_prop({<:ident> state_ $name2}.into());)*
        $(model.add_prop({<:ident> compute_ $name3}.into());)*
        model
      }
    }
  } };
  (@$($tt:tt)*) => { compile_error!(concat!("macro_error model_apply!: @", stringify!($($tt)*))); };
}

#[macro_export]
macro_rules! model {
  (@stage0[], name: $name:ident $($tt:tt)*) => { $crate::macros::model!(@stage0_impl[@name $name] $($tt)*); };
  (@stage0[] $($tt:tt)*) => { $crate::macros::model!(@stage0_impl[@name Model] $($tt)*); };
  (@stage0_impl[@name $name:ident] $($tt:tt)*) => {
    $crate::orbvue_apply! {
      #[allow(dead_code, non_snake_case, non_camel_case_types, unused_imports, unused_variables)]
      mod {<:ident> __model_ $name} {
        use super::*;
        use $crate::macros::*;
        model!(@stage1[@name $name] $($tt)*);
      }
      #[allow(unused_imports)]
      use {<:ident> __model_ $name}::$name;
    }
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
    $($crate::macros::prop_object!($name$($to)?:$ty $(= $expr)?);)*
    $crate::orbvue_apply!{
      model!(@stage2[$($r)* @props{ $($name:$ty {{<:condition> {@uninit !} {} $($to)?}},)* }] $($tt)*);
    }
  };
  (@stage2 $r:tt, states: { $($t1:tt)* } $($tt:tt)*) => {
    model!(@stage2_impl $r @states{ $($t1)* } $($tt)*);
  };
  (@stage2 $r:tt $($tt:tt)*) => {
    model!(@stage2_impl $r @states{ } $($tt)*);
  };
  (@stage2_impl[$($r:tt)*] @states{ $($name:ident:$ty:ty $(= $expr:expr)?),* $(,)? } $($tt:tt)*) => {
    $($crate::macros::state_object!($name:$ty $(= $expr)?);)*
    model!(@stage3[$($r)* @states{ $($name:$ty,)* }] $($tt)*);
  };
  (@stage3 $r:tt, compute: { $($t1:tt)* } $($tt:tt)*) => {
    model!(@stage3_impl $r @compute{ $($t1)* } $($tt)*);
  };
  (@stage3 $r:tt $($tt:tt)*) => {
    model!(@stage3_impl $r @compute{ } $($tt)*);
  };
  (@stage3_impl[$($r:tt)*] @compute{ $($name:ident [$($deps:ident: &$depty:ty),* $(,)?] -> $ty:ty { $($ttt:tt)* }),* $(,)? } $($tt:tt)*) => {
    $($crate::macros::compute_object!($name [$($deps: &$depty),*] -> $ty { $($ttt)* });)*
    model!(@stage4[$($r)* @compute{ $($name:$ty [$($deps),*],)*}] $($tt)*);
  };
  (@stage4 $r:tt $(,)?) => { $crate::orbvue_apply!{
    model!(@stage_final {<:model_sort> $r });
  } };
  (@stage_final [@name $name:ident $($r:tt)*]) => {
    #[cfg(test)]
    mod test {
      use super::*;

      model_apply!(@test_struct [@name $name $($r)*]);

      model_apply!(@test_func [@name $name $($r)*]);
    }
    model_apply!(@build_model [@name $name $($r)*]);
  };
  (@$($tt:tt)*) => { compile_error!(concat!("macro_error model!: @", stringify!($($tt)*))); };
  ($($tt:tt)*) => { $crate::macros::model!(@stage0[], $($tt)*); };
}


#[cfg(test)]
pub(crate) mod test {
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

  #[derive(Default)]
  pub struct M;
  impl super::Vue for M {
    type Widget = W;
    fn create_model() -> super::BuildModel<Self> { unimplemented!() }
  }

  #[derive(Default)]
  pub struct W(pub Vec<((std::any::TypeId, String), Box<dyn std::fmt::Debug>)>);
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
    use super::test::{W, M};
    let model = model::create::<M>();
    let w = model.build_template(W::default(), Entity::from(0));
    for i in w.0 {
      println!("{:?}", i);
    }
  }
}
