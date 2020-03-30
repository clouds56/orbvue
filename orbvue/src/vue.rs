use std::fmt::Debug;
use crate::compute::BuildModel;
pub use orbtk::{StringComponentStore, Component, Entity, SharedComponentBox, ComponentBox};

pub trait Vue: Sized {
  type Widget: AddComponent;
  fn create_model() -> BuildModel<Self>;
}

pub trait AddComponent: Sized {
  fn add_shared_component(self, typeid: std::any::TypeId, key: String, source_key: String, source_id: Entity) -> Self;
  fn add_component_box(self, key: String, value: ComponentBox) -> Self;
  fn add_component_value<C: Component + Debug>(self, key: String, value: C) -> Self {
    self.add_component_box(key, ComponentBox::new(value))
  }
}

#[macro_export]
macro_rules! vue {
  ($name:ident, $model:ty, $widget:ty) => {
    pub struct $name;
    impl $crate::vue::Vue for $name {
      type Widget = $widget;
      fn create_model() -> $crate::compute::BuildModel<Self> {
        <$model>::create()
      }
    }
    impl $crate::vue::AddComponent for $widget {
      fn add_shared_component(mut self, typeid: std::any::TypeId, key: String, source_key: String, source_id: $crate::vue::Entity) -> Self {
        self.shared_attached_properties.insert((key, source_key), $crate::vue::SharedComponentBox::new(typeid, source_id)); self
      }
      fn add_component_box(mut self, key: String, value: $crate::vue::ComponentBox) -> Self {
        self.attached_properties.insert(key, value); self
      }
    }
    impl $widget {
      pub fn build_model(mut self, id: $crate::vue::Entity) -> Self {
        use $crate::vue::Vue;
        let model = $name::create_model();
        model.build_template(self, id)
      }
    }
  };
}
