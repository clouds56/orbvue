pub const TITLE: &str = "OrbTk - props example";
pub const WINDOW_SIZE: (f64, f64) = (420.0, 730.0);

orbvue::build_template!{
<template name="MainView">
  <Stack orientation="horizontal">
    <!-- By injecting the id of the parent the text property -->
    <!-- is shared between the MainView and the TextBox. This -->
    <!-- means both references the same String16 object. -->
    <TextBox height=32.0 text:="$id" />
    <Button margin:="(8.0, 0.0, 0.0, 0.0)" text="Clear"
            on_click:=r#"EventFn::with($id, |e| { println!("{:?}", e); })"# />
  </Stack>
</template>

<script>
orbvue::model!{
  name: MainModel,
  props: {
    greet: String16 = "hello".into(),
  },
  states: {
    text: String16
  },
  compute: {
    show_test [text: &String16, greet: &String16] -> String16 {
      format!("{}, {}!", greet, text).into()
    },
  }
}
orbvue::vue!(MainWidget, MainModel, MainView);

#[derive(Default, AsAny)]
struct MyState {
  clear: bool,
}

impl State for MyState {
  fn update(&mut self, _: &mut Registry, ctx: &mut Context<'_>) {
    if self.clear {
      // Clears the text property of MainView and because
      // of the sharing also the text of the TextBox.
      ctx.widget().set("text", String16::from(""));
      self.clear = false;
    }
  }
}

impl orbvue::event::OnAction for MyState {
  type Action = ();
  fn on_action(&mut self, _: &mut Registry, _: &mut Context<'_>, _: Self::Action) {
    self.clear = true
  }
}

type MainViewState = orbvue::event::ComputeState<MyState>;
type EventFn = orbvue::event::EventFn<MyState>;

widget!(MainView<MainViewState> {
  text: String16
});

impl Template for MainView {
  #[orbvue::template]
  fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
    #[orbvue="expr"]
    self.build_model(id)
  }
}
</script>
}
