use orbvue::build_template;

pub const TITLE: &str = "OrbTk - minimal example";
pub const WINDOW_SIZE: (f64, f64) = (420.0, 730.0);

build_template!{
<template name="MainView">
  <Stack orientation="horizontal">
    <!-- By injecting the id of the parent the text property -->
    <!-- is shared between the MainView and the TextBox. This -->
    <!-- means both references the same String16 object. -->
    <TextBox height=32.0 text:="$id" />
    <Button margin:="(8.0, 0.0, 0.0, 0.0)" text="Clear"
            on_click:="move |states, _| {states.get_mut::<MainViewState>($id).clear(); true}" />
  </Stack>
</template>

<script>
#[derive(Default, AsAny)]
pub struct MainViewState {
  clear: bool,
}

impl MainViewState {
  // Sets an action the state
  fn clear(&mut self) {
    self.clear = true;
  }
}

impl State for MainViewState {
  fn update(&mut self, _: &mut Registry, ctx: &mut Context<'_>) {
    if self.clear {
      // Clears the text property of MainView and because
      // of the sharing also the text of the TextBox.
      ctx.widget().set("text", String16::from(""));
      self.clear = false;
    }
  }
}

widget!(MainView<MainViewState> {
  text: String16
});

impl Template for MainView {
  #[orbvue::template]
  fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
    #[orbvue="expr"]
    self
  }
}
</script>
}
