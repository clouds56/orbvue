use std::collections::VecDeque;
use orbtk::prelude::*;

pub trait ComponentTrait: 'static {
  type Props;
  type State;
  type Slots;
  type Event;
}

// whole these Props, State, Slots, Event as a State in the scope of orbtk
#[derive(Default)]
pub struct Component<C: ComponentTrait> {
  // $props in template
  props: C::Props,
  // $state in template
  state: C::State,
  // v-on/@ in template
  slots: C::Slots,
  // action in document of orbtk
  events: VecDeque<C::Event>,
}

// this is exact the same as #[derive(AsAny)] output, while the derive not work with Generics
// see https://github.com/redox-os/orbtk/blob/develop/crates/api/src/widget/state.rs
// see https://github.com/redox-os/orbtk/blob/develop/crates/proc-macros/src/lib.rs
impl<C: ComponentTrait> AsAny for Component<C> {
  fn as_any(&self) -> &dyn Any { self }
  fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

#[macro_export]
macro_rules! component {
  (props: $name:ident {
    $($field:ident : $ty:ty = $expr:expr,)*
  }) => {
    pub struct $name {
      $($field:$ty,)*
    }
    impl ::core::default::Default for $name {
      fn default() -> Self {
        Self {
          $($field: $expr,)*
        }
      }
    }
    #[allow(dead_code)]
    impl $name {
      $(pub fn $field(&self) -> &$ty {
        &self.$field
      })*
    }
  };
}

#[cfg(test)]
mod test {
  component!{
    props: Prop {
      i: u32 = 0,
      j: String = "".into(),
    }
  }
}
