#[cfg(feature="derive")]
pub use orbvue_derive::{build_template, orbvue_apply};

pub mod compute;
pub mod event;
pub mod vue;
mod test;

pub mod macros {
  pub use crate::{into_computed_prop, prop_object, state_object, compute_object, model, model_apply};
}
