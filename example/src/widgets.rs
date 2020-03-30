use orbvue::build_template;

pub const TITLE: &str = "OrbTk - widgets example";
pub const WINDOW_SIZE: (f64, f64) = (468.0, 730.0);

build_template!{
<template name="MainView" result="Button count: 0" counter=0 selected_indices:="HashSet::new()"
          list:=r#"vec![ "Item 1".to_string(), "Item 2".to_string(), "Item 3".to_string(), ]"# list_count=3
          selection_list:=r#"vec![ "Item 1".to_string(), "Item 2".to_string(), "Item 3".to_string(), "Item 4".to_string(), "Item 5".to_string(),
                                   "Item 6".to_string(), "Item 7".to_string(), "Item 8".to_string(), "Item 9".to_string(), "Item 10".to_string(), ]"#
          combo_box_list:=r#"vec![ "CB 1".to_string(), "CB 2".to_string(), "CB 3".to_string(), "CB 4".to_string(), "CB 5".to_string(),
                                   "CB 6".to_string(), "CB 7".to_string(), "CB 8".to_string(), "CB 9".to_string(), "CB 10".to_string(), ]"#
          selection_list_count=10 combo_box_list_count=10>
  <Grid margin=8.0 columns$="132.0 16.0 132.0 16.0 132.0">
    <Stack attach:="Grid::column(0)">
        <!-- Column 0 -->
        {{& create_header($ctx, "Buttons") }}
        <Button text="Button" margin:="(0.0, 8.0, 0.0, 0.0)" icon:="material_font_icons::CHECK_FONT_ICON" attach:="Grid::column(0)" attach:="Grid::row(1)"
                on_click:="move |states, _| { state($id, states).action(Action::IncrementCounter); true }" />
        <Button text="Primary" element="button" class="primary" margin:="(0.0, 8.0, 0.0, 0.0)" icon:="material_font_icons::CHECK_FONT_ICON"
                attach:="Grid::column(0)" attach:="Grid::row(2)" />
        <ToggleButton class="single_content" text="ToggleButton" margin:="(0.0, 8.0, 0.0, 0.0)" attach:="Grid::column(0)" attach:="Grid::row(3)" />
        <CheckBox text="CheckBox" margin:="(0.0, 8.0, 0.0, 0.0)" attach:="Grid::column(0)" attach:="Grid::row(4)" />
        <Switch margin:="(0.0, 8.0, 0.0, 0.0)" attach:="Grid::column(0)" attach:="Grid::row(5)" />
        <TextBlock margin:="(0.0, 8.0, 0.0, 0.0)" element="h1" id="value_text" text="0" horizontal_alignment="center" />
        <Slider on_changed:="move |states, entity| { state($id, states).action(Action::ValueChanged(entity)); }" />
    </Stack>
    <Stack attach:="Grid::column(2)">
      {{& create_header($ctx, "Text") }}
      <TextBlock class="body" text:=r#"("result", $id)"# margin:="(0.0, 8.0, 0.0, 0.0)" attach:="Grid::column(2)" attach:="Grid::row(1)" />
      <TextBox water_mark="TextBox..." text:=r#"("text_one", $id)"# margin:="(0.0, 8.0, 0.0, 0.0)" attach:="Grid::column(2)" attach:="Grid::row(2)"
                on_activate:="move |states, entity| { state($id, states).action(Action::EntryActivated(entity)); }"
                on_changed:="move |states, entity| { state($id, states).action(Action::EntryChanged(entity)); }" />
      <TextBox water_mark="TextBox..." text:=r#"("text_two", $id)"# margin:="(0.0, 8.0, 0.0, 0.0)" attach:="Grid::column(2)" attach:="Grid::row(2)"
                on_activate:="move |states, entity| { state($id, states).action(Action::EntryActivated(entity)); }"
                on_changed:="move |states, entity| { state($id, states).action(Action::EntryChanged(entity)); }" />
      <Button margin:="(0.0, 8.0, 0.0, 0.0)" class="single_content" text="clear text"
              on_click:="move |states, _| { state($id, states).action(Action::ClearText); true }" />
    </Stack>
    <Grid rows$="auto 32.0 16.0 204.0 auto 192.0 auto" columns$="* 4.0 *" attach:="Grid::column(4)">
      <TextBlock text="Items" element="text-block" class="h1" attach:="Grid::column(0)" attach:="Grid::column_span(3)" attach:="Grid::row(0)" />
      <ComboBox items_builder:=r#"move |bc, index| {
                let text = bc.get_widget($id).get::<Vec<String>>("combo_box_list")[index].clone();
                TextBlock::create().margin((0.0, 0.0, 0.0, 2.0)).vertical_alignment("center").text(text).build(bc) } "#
                selected_index=0 attach:="Grid::column(0)" attach:="Grid::column_span(3)" attach:="Grid::row(1)"
                margin:="(0.0, 8.0, 0.0, 0.0)" count:=r#"("combo_box_list_count", $id)"# />
      <ItemsWidget element="items-widget" id="items" attach:="Grid::column(0)" attach:="Grid::column_span(3)" attach:="Grid::row(3)"
                  padding:="(4.0, 4.0, 4.0, 2.0)" margin:="(0.0, 0.0, 0.0, 8.0)" count:=r#"("list_count", $id)"#
                  items_builder:=r#"move |bc, index| {
                  let text = bc.get_widget($id).get::<Vec<String>>("list")[index].clone();
                  Button::create().margin((0.0, 0.0, 0.0, 2.0)).text(text).build(bc) } "# />
      <Button element="button" class="single_content" id="remove-item-button" icon:="material_font_icons::MINUS_FONT_ICON"
              on_click:="move |states, _| { state($id, states).action(Action::RemoveItem); true }"
              min_width=0.0 attach:="Grid::column(0)" attach:="Grid::row(4)" />
      <Button element="button" class="single_content" id="add-item-button" icon:="material_font_icons::ADD_FONT_ICON"
              on_click:="move |states, _| { state($id, states).action(Action::AddItem); true }"
              min_width=0.0 attach:="Grid::column(2)" attach:="Grid::row(4)" />
      <ListView attach:="Grid::column(0)" attach:="Grid::column_span(3)" attach:="Grid::row(5)"
                selected_indices:="$id" margin:="(0.0, 16.0, 0.0, 8.0)" count:=r#"("selection_list_count", $id)"#
                items_builder:=r#"move |bc, index| {
                let text = bc.get_widget($id).get::<Vec<String>>("selection_list")[index].clone();
                TextBlock::create().margin((0.0, 0.0, 0.0, 2.0)).vertical_alignment("center").text(text).build(bc) }"# />
      <!-- todo: wrong text width???? -->
      <TextBlock element="text-block" id="selection" max_width=120.0 text="Selected:"
                attach:="Grid::column(0)" attach:="Grid::column_span(3)" attach:="Grid::row(6)" />
    </Grid>
  </Grid>
</template>

<script>
use std::collections::HashSet;

#[derive(Debug, Copy, Clone)]
enum Action {
  AddItem,
  ClearText,
  EntryActivated(Entity),
  EntryChanged(Entity),
  ValueChanged(Entity),
  IncrementCounter,
  RemoveItem,
}

#[derive(AsAny)]
struct MainViewState {
  action: Option<Action>,
}

impl Default for MainViewState {
  fn default() -> Self {
    MainViewState { action: None }
  }
}

impl MainViewState {
  fn action(&mut self, action: impl Into<Option<Action>>) {
    self.action = action.into();
  }
}

impl State for MainViewState {
  fn update(&mut self, _: &mut Registry, ctx: &mut Context<'_>) {
    if let Some(action) = self.action {
      match action {
        Action::AddItem => {
          let len = ctx.widget().get::<List>("list").len();
          if len < 5 {
            ctx.widget()
              .get_mut::<List>("list")
              .push(format!("Item {}", len + 1));
            ctx.child("items").set("count", len + 1);
            ctx.child("remove-item-button").set("enabled", true);

            if len == 4 {
              ctx.child("add-item-button").set("enabled", false);
            }
          }
        }
        Action::RemoveItem => {
          let len = ctx.widget().get::<List>("list").len();
          if len > 0 {
            ctx.widget().get_mut::<List>("list").remove(len - 1);
            ctx.child("items").set("count", len - 1);
            ctx.child("add-item-button").set("enabled", true);

            if len == 1 {
              ctx.child("remove-item-button").set("enabled", false);
            }
          }
        }
        Action::IncrementCounter => {
          *ctx.widget().get_mut::<usize>("counter") += 1;

          let counter = *ctx.widget().get::<usize>("counter");

          ctx.widget().set(
            "result",
            String16::from(format!("Button count: {}", counter)),
          );
        }
        Action::ClearText => {
          ctx.widget().set("text_one", String16::from(""));
          ctx.widget().set("text_two", String16::from(""));
        }
        Action::EntryActivated(entity) => {
          let mut widget = ctx.get_widget(entity);
          let text = widget.get_mut::<String16>("text");
          println!("submitting {}", text);
          text.clear();
        }
        Action::EntryChanged(entity) => {
          let widget = ctx.get_widget(entity);
          let text = widget.get::<String16>("text");
          println!("entry changed: {}", text);
        }
        Action::ValueChanged(entity) => {
          let value =
            ((*ctx.get_widget(entity).get::<f64>("value")).floor() as i32).to_string();
          ctx.child("value_text").set("text", String16::from(value));
        }
      }

      self.action = None;
    }
  }

  fn update_post_layout(&mut self, _: &mut Registry, ctx: &mut Context<'_>) {
    let mut selection_string = "Selected:".to_string();

    for index in &ctx.widget().get::<SelectedIndices>("selected_indices").0 {
      selection_string = format!("{} {}", selection_string, index);
    }

    ctx.child("selection")
      .set("text", String16::from(selection_string));
  }
}

fn create_header(ctx: &mut BuildContext, text: &str) -> Entity {
  TextBlock::create()
    .text(text)
    .element("text-block")
    .class("h1")
    .build(ctx)
}

type List = Vec<String>;

widget!(
  MainView<MainViewState> {
    selected_indices: SelectedIndices,
    counter: usize,
    list_count: usize,
    combo_box_list_count: usize,
    list: List,
    selection_list: List,
    combo_box_list: List,
    selection_list_count: usize,
    text_one: String16,
    text_two: String16,
    result: String16
  }
);

impl Template for MainView {
  #[orbvue::template]
  fn template(self, #[orbvue="id"] id: Entity, #[orbvue="ctx"] ctx: &mut BuildContext) -> Self {
    #[orbvue="expr"]
    self
  }
}

// helper to request MainViewState
fn state<'a>(id: Entity, states: &'a mut StatesContext) -> &'a mut MainViewState {
  states.get_mut(id)
}
</script>
}
