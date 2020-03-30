use std::any::Any;
use orbtk::{AsAny, Entity, StatesContext, State, Registry, Context};

pub enum ComputeAction<Action> {
  #[allow(dead_code)]
  Auto(&'static str),
  Custom(Action),
}

pub struct ComputeActionState<Action, State> {
  actions: std::collections::VecDeque<ComputeAction<Action>>,
  state: State,
}
impl<Action, State: Default> Default for ComputeActionState<Action, State> {
  fn default() -> Self {
    Self { actions: std::collections::VecDeque::new(), state: State::default() }
  }
}
pub type ComputeState<State> = ComputeActionState<<State as OnAction>::Action, State>;
pub type ComputeNoActionState<State> = ComputeActionState<ActionNever, State>;

impl<A: 'static, S: 'static> AsAny for ComputeActionState<A, S> {
  fn as_any(&self) -> &dyn Any { self }
  fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

impl<A, S> ComputeActionState<A, S> {
  pub fn inner_mut(&mut self) -> &mut S {
    &mut self.state
  }
  pub fn action(&mut self, action: A) {
    self.actions.push_back(ComputeAction::Custom(action))
  }
}

impl<A: 'static, S: State> State for ComputeActionState<A, S> where S: OnAction<Action=A> {
  fn update(&mut self, reg: &mut Registry, ctx: &mut Context<'_>) {
    while let Some(action) = self.actions.pop_front() {
      match action {
        ComputeAction::Auto(_) => unimplemented!("ComputeAction::Auto"),
        ComputeAction::Custom(a) => self.state.on_action(reg, ctx, a),
      }
    }
    self.state.update(reg, ctx)
  }
}

pub struct ActionNever(());
pub trait NoAction { }
impl<S: NoAction> OnAction for S {
  type Action = ActionNever;
  fn on_action(&mut self, _: &mut Registry, _: &mut Context<'_>, _: Self::Action) {
    unreachable!("action should never construct");
  }
}

pub trait OnAction {
  type Action: 'static;
  fn on_action(&mut self, reg: &mut Registry, ctx: &mut Context<'_>, action: Self::Action);
}

pub struct EventFn<State>(std::marker::PhantomData<State>);
impl<State: OnAction + 'static> EventFn<State> where State::Action: Clone {
  pub fn get_mut<'a>(states: &'a mut StatesContext, id: Entity) -> &'a mut ComputeActionState<State::Action, State> {
    states.get_mut(id)
  }
  #[allow(clippy::new_ret_no_self)]
  pub fn new<Event>(id: Entity, action: State::Action) -> impl for<'r, 's> Fn(&'r mut StatesContext<'s>, Event) -> bool {
    move |states, _| { Self::get_mut(states, id).action(action.clone()); true }
  }
  pub fn with<Event>(id: Entity, f: impl Fn(Event) -> State::Action) -> impl for<'r, 's> Fn(&'r mut StatesContext<'s>, Event) -> bool {
    move |states, event| { Self::get_mut(states, id).action(f(event)); true }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[derive(Default, AsAny)]
  struct S;
  impl State for S { }
  impl NoAction for S { }

  /// simulate &mut HashMap<Entity, Box<dyn State>>
  #[allow(clippy::borrowed_box)]
  struct Wrap<'a>(&'a mut Box<dyn State>);
  impl<'a> Wrap<'a> {
    fn get_mut<S: orbtk::Component>(&mut self) -> Option<&mut S> {
      self.0.as_any_mut().downcast_mut()
    }
  }
  struct EventFn<State>(std::marker::PhantomData<State>);
  impl<State: 'static> EventFn<State> {
    fn get_mut<'a, Action: 'static>(states: &'a mut Wrap) -> &'a mut ComputeActionState<Action, State> {
      states.get_mut().unwrap()
    }
    #[allow(clippy::new_ret_no_self)]
    fn new<Action: 'static + Clone>(action: Action) -> impl for<'r, 's> Fn(&'r mut Wrap<'s>) -> bool {
      move |states| { Self::get_mut(states).action(action.clone()); true }
    }
  }
  type E = EventFn<S>;

  #[test]
  fn test_typeid() {
    use std::any::TypeId;
    assert_ne!(TypeId::of::<ComputeActionState<(), S>>(), TypeId::of::<ComputeActionState<ActionNever, S>>());
    let s: ComputeActionState<ActionNever, S> = Default::default();
    let mut s: Box<dyn State> = Box::new(s);
    let mut s = Wrap(&mut s);
    assert!(s.get_mut::<ComputeActionState<(), S>>().is_none());
  }
  #[test]
  #[should_panic]
  fn test_typeid_panic() {
    let s: ComputeActionState<ActionNever, S> = Default::default();
    let mut s: Box<dyn State> = Box::new(s);
    let mut s = Wrap(&mut s);
    E::new(true)(&mut s);
  }
}
