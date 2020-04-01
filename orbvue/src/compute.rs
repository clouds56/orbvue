use std::fmt::Debug;
use std::any::TypeId;
use std::rc::Rc;
use crate::vue::*;

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
  (@stage_impl $name:ident: $ty:ty [@expr {$($expr:tt)*} @example {$($example:tt)*}]) => { $crate::orbvue_apply!{
    #[allow(non_camel_case_types)]
    pub struct {<:ident> prop_ $name};
    impl {<:ident> prop_ $name} {
      pub const NAME: &'static str = {<:stringify> $name};
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
    into_computed_prop!({<:ident> prop_ $name});
  } };
  ($($tt:tt)*) => { compile_error!(concat!("macro_error prop_object!: ", stringify!($($tt)*))); };
}
#[macro_export]
macro_rules! compute_object {
  ($name:ident [$($deps:ident: &$depty:ty),* $(,)?] -> $ty:ty { $($tt:tt)* }) => { $crate::orbvue_apply!{
    #[allow(non_camel_case_types)]
    pub struct {<:ident> compute_ $name};
    impl {<:ident> compute_ $name} {
      pub const NAME: &'static str = {<:stringify> $name};
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
          let $deps = ComputeHandler::get::<$depty>(store, ComputedKey({<:stringify> $deps}), id);
        )*
        let {<:ident> new_ $name} = Self::call($($deps),*);
        ComputeHandler::set(store, ComputedKey(Self::NAME), id, {<:ident> new_ $name}, Self::replace)
      }
      pub fn typeid() -> ::core::any::TypeId {
        ::core::any::TypeId::of::<$ty>()
      }
      pub fn handler() -> $crate::compute::ComputeBox {
        $crate::compute::ComputeHandler::from(Self::wrapper).into()
      }
      pub fn deps() -> Vec<$crate::compute::ComputedKey> {
        let deps: Vec<&'static str> = vec![$({<:stringify> $deps}),*];
        $crate::compute::ComputedKey::from_vec(deps)
      }
    }
    $crate::into_computed_prop!({<:ident> compute_ $name});
  } };
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

  #[test]
  fn test_model() {
    use super::*;
    use crate::model::{BuildModel, test::{W, M}};
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
