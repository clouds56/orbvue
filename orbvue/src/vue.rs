use std::fmt::Debug;
pub use orbtk::{StringComponentStore, Component, Entity, ComponentBox};

pub trait Vue {
  type Widget: AddComponent;
}

pub trait AddComponent: Sized {
  fn add_shared_component(self, typeid: std::any::TypeId, key: String, source_key: String, source_id: Entity) -> Self;
  fn add_component_box(self, key: String, value: ComponentBox) -> Self;
  fn add_component_value<C: Component + Debug>(self, key: String, value: C) -> Self {
    self.add_component_box(key, ComponentBox::new(value))
  }
}
