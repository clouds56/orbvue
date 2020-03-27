use std::any::{Any, TypeId};

pub trait ComputedValue: Any {
  fn as_any(&self) -> &dyn Any;
}

pub trait Computable<T: ComputedValue> {
  fn is_dirty(&self) -> bool;
  fn get_value(&self) -> Option<&T>;
}

pub type Key = &'static str;

pub struct Computed {
  dirty: bool,
  upstream: Vec<Key>,
  downstream: Vec<Key>,
  value: Box<dyn ComputedValue>,
}

impl<T: ComputedValue> Computable<T> for Computed {
  fn is_dirty(&self) -> bool { self.dirty }
  fn get_value(&self) -> Option<&T> {
    if self.dirty {
      None
    } else {
      self.value.as_any().downcast_ref()
    }
  }
}

impl Computed {
  pub fn root<T: ComputedValue>(value: T) -> Computed {
    Computed { dirty: false, upstream: vec![], downstream: vec![], value: Box::new(value) }
  }
  pub fn type_id(this: &Self) -> TypeId {
    this.value.as_any().type_id()
  }
}

macro_rules! computed_value {
  (@shim {
    constr: [$($t1:tt)*],
  }, $name:ident $($t2:tt)*) => {
    impl<$($t1)*> $crate::compute::ComputedValue for $name $($t2)* {
      fn as_any(&self) -> &dyn ::core::any::Any { self }
    }
  };
  (@shim {
    constr: [$($t1:tt)*],
  }, $name:ty) => {
    impl<$($t1)*> $crate::compute::ComputedValue for $name {
      fn as_any(&self) -> &dyn ::core::any::Any { self }
    }
  };
  (@shim_test $name:ident $args:tt,) => {
    computed_value!(@shim_test $name $args, = ::core::default::Default::default());
  };
  (@shim_test $name:ident {
    constr: [$($t1:tt)*],
  }, = $($t2:tt)*) => {
    paste::item!{
      #[test]
      #[allow(non_snake_case)]
      fn [<test_computed_type_ $name>]() {
        use ::core::any::Any;
        let [<$name _x>]: $name<$($t1)*> = $($t2)*;
        let [<$name _x_typeid>] = [<$name _x>].type_id();
        let [<$name _y>] = $crate::compute::Computed::root([<$name _x>]);
        assert_eq!([<$name _x_typeid>], $crate::compute::Computed::type_id(&[<$name _y>]));
        assert!($crate::compute::Computable::<$name<$($t1)*>>::get_value(&[<$name _y>]).is_some());
      }
    }
  };
  (@impl_ $($t:tt)*) => {
    parse_generics_shim!{
      { constr },
      then computed_value!(@shim),
      $($t)*
    }
  };
  (@test $name:ident $($t:tt)*) => {
    parse_generics_shim!{
      { constr },
      then computed_value!(@shim_test $name),
      $($t)*
    }
  };
  (@$($t:tt)*) => {
    compile_error!(concat!("computed_value macro error: @", stringify!($($t)*)));
  };
  ($($t:tt)*) => {
    computed_value!(@impl_ $($t)*);
  };
}
computed_value!(());

#[cfg(test)]
mod test {
  struct Test<T: 'static>(T);
  #[derive(Default)]
  struct Test2(usize);
  computed_value!(<T: 'static> Test<T>);
  computed_value!(Test2);
  computed_value!(@test Test<usize> = Test(10));
  computed_value!(@test Test2);
  #[test]
  fn test_computed() {
    use super::*;
    #[derive(Debug, PartialEq)]
    struct V(usize);
    computed_value!(V);

    let x = Computed::root(V(10));
    dbg!(V(10).type_id());
    dbg!(x.value.as_any().type_id());
    dbg!(x.value.as_ref().type_id());
    dbg!(x.value.type_id());
    dbg!(Computed::type_id(&x));
    assert_eq!(Computed::type_id(&x), V(10).type_id());
    assert_eq!(x.get_value(), None::<&()>);
    assert_eq!(x.get_value(), Some(&V(10)));
  }
}

/// you would like to write
/// #[computed]
/// fn c(&self) -> usize {
///   self.a + self.b - 1
/// }
/// that is compiled to
/// fn compute_c(entity: Entity, store: &ComponentStore) -> ComputedValue {
///   let a: Computable = store.get::<Computed>(entity, "a");
///   let b: Computable = store.get::<Computed>(entity, "b");
///
/// }
mod doc {}
