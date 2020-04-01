use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};

pub struct TraceRef<'a, T>(Box<dyn Deref<Target=T> + 'a>);
pub struct TraceMut<'a, T>(Box<dyn DerefMut<Target=T> + 'a>, Box<dyn FnMut() + 'a>);
impl<'a, T> TraceRef<'a, T> {
  fn new(p: impl Deref<Target=T> + 'a) -> Self {
    Self(Box::new(p))
  }
}

impl<T> std::ops::Deref for TraceRef<'_, T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T> std::ops::Deref for TraceMut<'_, T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T> std::ops::DerefMut for TraceMut<'_, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.1();
    &mut self.0
  }
}

impl<'a, T> TraceMut<'a, T> {
  fn new(p: impl DerefMut<Target=T> + 'a, f: impl FnMut() + 'a) -> Self {
    Self(Box::new(p), Box::new(f))
  }
}

pub trait Tracable {
  type Target;
  fn new(t: Self::Target) -> Self;
  fn get(&self) -> TraceRef<'_, Self::Target>;
  fn get_mut(&mut self) -> TraceMut<'_, Self::Target>;
  fn clean(&mut self);
  fn mess_up(&mut self);
  fn dirty(&self) -> bool;
}

#[derive(Default, Clone, Debug)]
pub struct Data<T> {
  data: T,
  dirty: u32,
}

impl<T> Tracable for Data<T> {
  type Target = T;
  fn new(t: T) -> Self { Self { data: t, dirty: 0 } }
  fn get(&self) -> TraceRef<'_, Self::Target> { TraceRef::new(&self.data) }
  fn get_mut(&mut self) -> TraceMut<'_, Self::Target> {
    let dirty = &mut self.dirty;
    TraceMut::new(&mut self.data, move || *dirty += 1)
   }
  fn clean(&mut self) { self.dirty = 0; }
  fn mess_up(&mut self) { self.dirty += 1; }
  fn dirty(&self) -> bool { self.dirty > 0 }
}
impl<T: 'static> Data<T> {
  pub fn downcast<U: 'static>(&self) -> Option<&Data<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { std::mem::transmute(self) })
    } else {
      None
    }
  }
  pub fn downcast_mut<U: 'static>(&mut self) -> Option<&mut Data<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { std::mem::transmute(self) })
    } else {
      None
    }
  }
}

#[test]
fn test_traced_data() {
  use std::collections::HashMap;
  let mut traced: HashMap<usize, Data<String>> = HashMap::new();
  assert_eq!(traced.entry(0).or_default().dirty(), false);
  assert_eq!(traced.get_mut(&0).unwrap().dirty(), false);
  traced.get_mut(&0).unwrap().get_mut().push_str("hello");
  assert_eq!(traced.get_mut(&0).unwrap().dirty(), true);
  traced.get_mut(&0).unwrap().clean();
  assert_eq!(traced.get(&0).unwrap().dirty(), false);
  assert_eq!(traced.get(&0).unwrap().get().as_str(), "hello");
  assert_eq!(traced.get_mut(&0).unwrap().get_mut().as_str(), "hello");
  assert_eq!(traced.get(&0).unwrap().dirty(), false);
  traced.get_mut(&0).unwrap().mess_up();
  assert_eq!(traced.get(&0).unwrap().dirty(), true);
  {
    use std::ops::DerefMut;
    traced.get_mut(&0).unwrap().clean();
    traced.get_mut(&0).unwrap().get_mut().deref_mut();
  }
  assert_eq!(traced.get(&0).unwrap().dirty(), true);
  // println!("{:?}", traced);
}

#[derive(Default, Clone, Debug)]
pub struct SharedData<T> {
  data: Rc<RefCell<T>>,
  dirty: u32,
}

impl<T> Tracable for SharedData<T> {
  type Target = T;
  fn new(t: T) -> Self { Self { data: Rc::new(RefCell::new(t)), dirty: 0 } }
  fn get(&self) -> TraceRef<'_, Self::Target> { TraceRef::new(self.data.borrow()) }
  fn get_mut(&mut self) -> TraceMut<'_, Self::Target> {
    let dirty = &mut self.dirty;
    TraceMut::new(self.data.borrow_mut(), move || *dirty += 1)
  }
  fn clean(&mut self) { self.dirty = 0; }
  fn mess_up(&mut self) { self.dirty += 1; }
  fn dirty(&self) -> bool { self.dirty > 0 }
}

impl<T: 'static> SharedData<T> {
  pub fn downcast<U: 'static>(&self) -> Option<&Data<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { std::mem::transmute(self) })
    } else {
      None
    }
  }
}

#[test]
fn test_traced_shared() {
  use std::collections::HashMap;
  let mut traced: HashMap<usize, SharedData<String>> = HashMap::new();
  assert_eq!(traced.entry(0).or_default().dirty(), false);
  assert_eq!(traced.get_mut(&0).unwrap().dirty(), false);
  traced.get_mut(&0).unwrap().get_mut().push_str("hello");
  assert_eq!(traced.get_mut(&0).unwrap().dirty(), true);
  traced.get_mut(&0).unwrap().clean();
  assert_eq!(traced.get(&0).unwrap().dirty(), false);
  assert_eq!(traced.get(&0).unwrap().get().as_str(), "hello");
  assert_eq!(traced.get_mut(&0).unwrap().get_mut().as_str(), "hello");
  assert_eq!(traced.get(&0).unwrap().dirty(), false);
  traced.get_mut(&0).unwrap().mess_up();
  assert_eq!(traced.get(&0).unwrap().dirty(), true);
  {
    use std::ops::DerefMut;
    traced.get_mut(&0).unwrap().clean();
    traced.get_mut(&0).unwrap().get_mut().deref_mut();
  }
  assert_eq!(traced.get(&0).unwrap().dirty(), true);
  // println!("{:?}", traced);
}

#[macro_export]
macro_rules! state_object {
  ($name:ident: $ty:ty) => {
    state_object!($name: $ty = Default::default());
  };
  ($name:ident: $ty:ty = $expr:expr) => { $crate::orbvue_apply!{
    #[allow(non_camel_case_types)]
    pub struct {<:ident> state_ $name};
    impl {<:ident> state_ $name} {
      pub const NAME: &'static str = {<:stringify> $name};
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
    into_computed_prop!({<:ident> state_ $name});
  } };
}

#[macro_export]
macro_rules! state {
  ($name:ident: {$($n:ident:$ty:ty),* $(,)?}) => {
    #[allow(dead_code)]
    pub struct $name {
      $(pub $n: $crate::state::Data<$ty>,)*
    }

    $crate::orbvue_apply! {
      impl $crate::state::TracedState for $name {
        fn get<T: 'static>(&self, name: &str) -> Option<$crate::state::TraceRef<'_, T>> {
          use $crate::state::Tracable;
          match name {
            $({<:stringify> $n} => Some(self.$n.downcast::<T>().expect(&format!("wrong type downcast {}", name)).get()),)*
            _ => None,
          }
        }
        fn get_mut<T: 'static>(&mut self, name: &str) -> Option<$crate::state::TraceMut<'_, T>> {
          use $crate::state::Tracable;
          match name {
            $({<:stringify> $n} => Some(self.$n.downcast_mut::<T>().expect(&format!("wrong type downcast {}", name)).get_mut()),)*
            _ => None,
          }
        }
      }
    }
    impl $name {
      #[allow(dead_code)]
      pub fn new($($n:$ty),*) -> Self {
        use $crate::state::Tracable;
        Self {
          $($n: $crate::state::Data::new($n),)*
        }
      }
    }
  };
}

pub trait TracedState {
  fn get<T: 'static>(&self, name: &str) -> Option<TraceRef<'_, T>>;
  fn get_mut<T: 'static>(&mut self, name: &str) -> Option<TraceMut<'_, T>>;
}

mod test_helper {
  crate::state!(State: {
    a: usize,
    b: String,
  });

  #[test]
  fn test_state_proxy() {
    use super::{Tracable, TracedState};
    let mut s = State::new(5, "b".to_string());
    *s.get_mut::<usize>("a").unwrap() += 1;
    assert_eq!(*s.get::<usize>("a").unwrap(), 6);
    assert!(s.a.dirty());
    assert!(s.get::<()>("c").is_none());
  }

  #[test]
  #[should_panic]
  fn test_state_proxy_panic() {
    use super::TracedState;
    let s = State::new(5, "b".to_string());
    s.get::<&str>("b");
  }
}
