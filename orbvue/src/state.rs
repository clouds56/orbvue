use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};

pub enum TraceCopy<T> {
  Raw(T),
  Rc(Rc<RefCell<T>>),
}
pub enum TraceRef<'a, T> {
  Raw(&'a T),
  Rc(Ref<'a, T>, &'a Rc<RefCell<T>>),
}
pub enum TraceMut<'a, T> {
  Raw(&'a mut T, &'a mut u32),
  Rc(RefMut<'a, T>, &'a Rc<RefCell<T>>, &'a mut u32),
}

impl<T> std::ops::Deref for TraceRef<'_, T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    match self {
      TraceRef::Raw(ptr) => ptr.deref(),
      TraceRef::Rc(ptr, _) => ptr.deref(),
    }
  }
}
impl<T> std::ops::Deref for TraceMut<'_, T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    match self {
      TraceMut::Raw(ptr, _) => ptr.deref(),
      TraceMut::Rc(ptr, _, _) => ptr.deref(),
    }
  }
}
impl<T> std::ops::DerefMut for TraceMut<'_, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    use std::ops::AddAssign;
    match self {
      TraceMut::Raw(ptr, dirty) => { dirty.add_assign(1); ptr.deref_mut() },
      TraceMut::Rc(ptr, _, dirty) => { dirty.add_assign(1); ptr.deref_mut() },
    }
  }
}
impl<'a, T> TraceRef<'a, T> {
  fn new_raw(p: &'a T) -> Self {
    TraceRef::Raw(p)
  }
  fn new_rc(r: &'a Rc<RefCell<T>>) -> Self {
    TraceRef::Rc(r.borrow(), r)
  }
  pub fn cloned(&self) -> TraceCopy<T> {
    match *self {
      TraceRef::Raw(_t) => unimplemented!(),
      TraceRef::Rc(_, t) => TraceCopy::Rc(t.clone()),
    }
  }
}
impl<'a, T: Clone> TraceRef<'a, T> {
  pub fn copy(&self) -> TraceCopy<T> {
    match *self {
      TraceRef::Raw(t) => TraceCopy::Raw(t.clone()),
      TraceRef::Rc(_, t) => TraceCopy::Rc(t.clone()),
    }
  }
}
impl<'a, T> TraceMut<'a, T> {
  fn new_raw(p: &'a mut T, dirty: &'a mut u32) -> Self {
    TraceMut::Raw(p, dirty)
  }
  fn new_rc(r: &'a Rc<RefCell<T>>, dirty: &'a mut u32) -> Self {
    TraceMut::Rc(r.borrow_mut(), r, dirty)
  }
  pub fn cloned(&self) -> TraceCopy<T> {
    match self {
      TraceMut::Raw(_t, _) => unimplemented!(),
      &TraceMut::Rc(_, t, _) => TraceCopy::Rc(t.clone()),
    }
  }
}
impl<'a, T: Clone> TraceMut<'a, T> {
  pub fn copy(&self) -> TraceCopy<T> {
    match self {
      TraceMut::Raw(t, _) => TraceCopy::Raw((*t).clone()),
      &TraceMut::Rc(_, t, _) => TraceCopy::Rc(t.clone()),
    }
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
  fn get(&self) -> TraceRef<'_, Self::Target> { TraceRef::new_raw(&self.data) }
  fn get_mut(&mut self) -> TraceMut<'_, Self::Target> {
    TraceMut::new_raw(&mut self.data, &mut self.dirty)
   }
  fn clean(&mut self) { self.dirty = 0; }
  fn mess_up(&mut self) { self.dirty += 1; }
  fn dirty(&self) -> bool { self.dirty > 0 }
}
impl<T: 'static> Data<T> {
  pub fn downcast<U: 'static>(&self) -> Option<&Data<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { &*(self as *const _ as *const _) })
    } else {
      None
    }
  }
  pub fn downcast_mut<U: 'static>(&mut self) -> Option<&mut Data<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { &mut *(self as *mut _ as *mut _) })
    } else {
      None
    }
  }
}
impl<T: Clone> Data<T> {
  pub fn from_copy(&mut self, r: TraceCopy<T>) {
    self.mess_up();
    match r {
      TraceCopy::Raw(t) => self.data = t,
      TraceCopy::Rc(t) => self.data = t.borrow().clone(),
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
  fn get(&self) -> TraceRef<'_, Self::Target> { TraceRef::new_rc(&self.data) }
  fn get_mut(&mut self) -> TraceMut<'_, Self::Target> {
    TraceMut::new_rc(&self.data, &mut self.dirty)
  }
  fn clean(&mut self) { self.dirty = 0; }
  fn mess_up(&mut self) { self.dirty += 1; }
  fn dirty(&self) -> bool { self.dirty > 0 }
}
impl<T> SharedData<T> {
  pub fn inner(&self) -> Rc<RefCell<T>> {
    self.data.clone()
  }
  pub fn from_copy(&mut self, r: TraceCopy<T>) {
    self.mess_up();
    match r {
      TraceCopy::Raw(t) => self.data = Rc::new(RefCell::new(t)),
      TraceCopy::Rc(t) => self.data = t,
    }
  }
}

impl<T: 'static> SharedData<T> {
  pub fn downcast<U: 'static>(&self) -> Option<&SharedData<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { &*(self as *const _ as *const _) })
    } else {
      None
    }
  }
  pub fn downcast_mut<U: 'static>(&mut self) -> Option<&mut SharedData<U>> {
    use std::any::TypeId;
    if TypeId::of::<T>() == TypeId::of::<U>() {
      Some(unsafe { &mut *(self as *mut _ as *mut _) })
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
            $({<:stringify> $n} => Some(self.$n.downcast::<T>().unwrap_or_else(|| panic!("wrong type downcast {}", name)).get()),)*
            _ => None,
          }
        }
        fn get_mut<T: 'static>(&mut self, name: &str) -> Option<$crate::state::TraceMut<'_, T>> {
          use $crate::state::Tracable;
          match name {
            $({<:stringify> $n} => Some(self.$n.downcast_mut::<T>().unwrap_or_else(|| panic!("wrong type downcast {}", name)).get_mut()),)*
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

#[cfg(test)]
mod test_shared {
  use super::*;
  struct State {
    a: Data<usize>,
    b: SharedData<String>,
  }

  impl TracedState for State {
    fn get<T: 'static>(&self, name: &str) -> Option<TraceRef<'_, T>> {
      match name {
        "a" => Some(self.a.downcast::<T>().unwrap_or_else(|| panic!("wrong type downcast {}", name)).get()),
        "b" => Some(self.b.downcast::<T>().unwrap_or_else(|| panic!("wrong type downcast {}", name)).get()),
        _ => None,
      }
    }
    fn get_mut<T: 'static>(&mut self, name: &str) -> Option<TraceMut<'_, T>> {
      match name {
        "a" => Some(self.a.downcast_mut::<T>().unwrap_or_else(|| panic!("wrong type downcast {}", name)).get_mut()),
        "b" => Some(self.b.downcast_mut::<T>().unwrap_or_else(|| panic!("wrong type downcast {}", name)).get_mut()),
        _ => None,
      }
    }
  }

  impl State {
    #[allow(dead_code)]
    pub fn new(a: usize, b: String) -> Self {
      Self {
        a: Data::new(a),
        b: SharedData::new(b),
      }
    }
  }

  #[test]
  fn test_state_proxy() {
    use super::{Tracable, TracedState};
    let mut s = State::new(5, "b".to_string());
    *s.get_mut::<usize>("a").unwrap() += 1;
    assert_eq!(*s.get::<usize>("a").unwrap(), 6);
    assert!(s.a.dirty());
    let b = s.b.inner();
    b.borrow_mut().push_str(".");
    assert_eq!(*s.get::<String>("b").unwrap(), "b.");
    assert!(s.get::<()>("c").is_none());
  }

  #[test]
  #[should_panic]
  fn test_state_proxy_panic() {
    use super::TracedState;
    let s = State::new(5, "b".to_string());
    s.get::<&str>("b");
  }

  #[test]
  fn test_state_ref_copy() {
    let a = Data::new(5);
    let mut b = Data::new(6);
    b.from_copy(a.get().copy());
    assert_eq!(*b.get(), 5);
    assert!(b.dirty());
    let mut b = SharedData::new(6);
    b.from_copy(a.get().copy());
    assert_eq!(*b.get(), 5);
    assert!(b.dirty());
    b.clean();
    let a = SharedData::new(6);
    b.from_copy(a.get().copy());
    assert_eq!(*b.get(), 6);
    assert!(b.dirty());
    let mut a = Data::new(5);
    a.from_copy(b.get().copy());
    assert_eq!(*a.get(), 6);
    assert!(a.dirty());
  }
}

#[cfg(test)]
mod test {
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
