use orbvue::build_template;

pub const TITLE: &str = "OrbTk - Calculator example";
pub const WINDOW_SIZE: (f64, f64) = (212.0, 336.0);

build_template!{
<template name="MainView" width:="212.0" height:="336.0" text="">
  <Grid rows$="72.0 *">
    <Container padding:="8.0" element="container" class="header" attach:="Grid::row(0)">
      <Grid>
        <ScrollViewer scroll_viewer_mode:=r#"("custom", "disabled")"#>
          <TextBlock width:="0.0" height:="14.0" text="" element="text-block" id="input" vertical_alignment="start" />
        </ScrollViewer>
        <TextBlock text:="$id" element="text-block" vertical_alignment="end" horizontal_alignment="end" />
      </Grid>
    </Container>
    <Container padding:="8.0" element="container" class="content" attach:="Grid::row(1)">
      <Grid columns$="48.0 4.0 48.0 4.0 48.0 4.0 48.0 4.0 48.0" rows$="48.0 4.0 48.0 4.0 48.0 4.0 48.0 4.0 48.0">
        <!-- row 0 -->
        {{& generate_operation_button($ctx, $id, 'C', false, 0, 5, 0) }}
        {{& generate_operation_button($ctx, $id, '/', true, 6, 1, 0) }}
        <!-- row 2 -->
        {{& generate_digit_button($ctx, $id, '7', false, 0, 1, 2) }}
        {{& generate_digit_button($ctx, $id, '8', false, 2, 1, 2) }}
        {{& generate_digit_button($ctx, $id, '9', false, 4, 1, 2) }}
        {{& generate_operation_button($ctx, $id, '*', true, 6, 1, 2) }}
        <!-- row 4 -->
        {{& generate_digit_button($ctx, $id, '4', false, 0, 1, 4) }}
        {{& generate_digit_button($ctx, $id, '5', false, 2, 1, 4) }}
        {{& generate_digit_button($ctx, $id, '6', false, 4, 1, 4) }}
        {{& generate_operation_button($ctx, $id, '-', true, 6, 1, 4) }}
        <!-- row 6 -->
        {{& generate_digit_button($ctx, $id, '1', false, 0, 1, 6) }}
        {{& generate_digit_button($ctx, $id, '2', false, 2, 1, 6) }}
        {{& generate_digit_button($ctx, $id, '3', false, 4, 1, 6) }}
        {{& generate_operation_button($ctx, $id, '+', true, 6, 1, 6) }}
        <!-- row 8 -->
        {{& generate_digit_button($ctx, $id, '0', false, 0, 3, 8) }}
        {{& generate_digit_button($ctx, $id, '.', false, 4, 1, 8) }}
        {{& generate_operation_button($ctx, $id, '=', true, 6, 1, 8) }}
      </Grid>
    </Container>
  </Grid>
</template>

<script>
#[derive(Debug, Copy, Clone)]
enum Action {
  Digit(char),
  Operator(char),
}

#[derive(Default, AsAny)]
pub struct MainViewState {
  input: String,
  operator: Option<char>,
  left_side: Option<f64>,
  right_side: Option<f64>,
  action: Option<Action>,
}

impl MainViewState {
  fn action(&mut self, action: impl Into<Option<Action>>) {
    self.action = action.into();
  }

  fn calculate(&mut self, ctx: &mut Context) {
    let mut result = 0.0;
    if let Some(operator) = self.operator {
      if let Some(left_side) = self.left_side {
        if let Some(right_side) = self.right_side {
          match operator {
            '+' => {
              result = left_side + right_side;
            }
            '-' => {
              result = left_side - right_side;
            }
            '*' => {
              result = left_side * right_side;
            }
            '/' => {
              result = left_side / right_side;
            }
            _ => {}
          }
        }
      }
    }

    if result % 1.0 == 0.0 {
      ctx.widget()
        .set("text", String16::from(format!("{}", result)));
    } else {
      ctx.widget()
        .set("text", String16::from(format!("{:.8}", result)));
    }

    self.left_side = Some(result);
    self.right_side = None;
  }
}

impl State for MainViewState {
  fn update(&mut self, _: &mut Registry, ctx: &mut Context) {
    if let Some(action) = self.action {
      match action {
        Action::Digit(digit) => {
          self.input.push(digit);
          ctx.child("input").get_mut::<String16>("text").push(digit);
        }
        Action::Operator(operator) => match operator {
          'C' => {
            self.input.clear();
            self.left_side = None;
            self.operator = None;
            self.right_side = None;
            ctx.widget().get_mut::<String16>("text").clear();
            ctx.child("input").get_mut::<String16>("text").clear()
          }
          '=' => {
            self.right_side = Some(self.input.parse().unwrap_or(0.0));
            self.calculate(ctx);
            self.input.clear();
            self.left_side = None;
            self.operator = None;
            self.right_side = None;
            ctx.child("input").get_mut::<String16>("text").clear()
          }
          _ => {
            if self.input.is_empty() {
              return;
            }
            if self.left_side.is_none() {
              self.left_side = Some(self.input.parse().unwrap_or(0.0));
            } else {
              self.right_side = Some(self.input.parse().unwrap_or(0.0));
              self.calculate(ctx);
            }

            ctx.child("input")
              .get_mut::<String16>("text")
              .push(operator);
            self.input.clear();
            self.operator = Some(operator);
          }
        },
      }

      self.action = None;
    }
  }
}

fn generate_digit_button(
  ctx: &mut BuildContext,
  id: Entity,
  sight: char,
  primary: bool,
  column: usize,
  column_span: usize,
  row: usize,
) -> Entity {
  let mut button = Button::create()
    .class("single_content")
    .min_size(48.0, 48.0)
    .text(sight.to_string())
    .on_click(move |states, _| -> bool {
      states.get_mut::<MainViewState>(id).action(Action::Digit(sight));
      true
    })
    .attach(Grid::column(column))
    .attach(Grid::row(row))
    .attach(Grid::column_span(column_span));

  if primary {
    button = button.class("primary");
  }

  button.build(ctx)
}

fn generate_operation_button(
  ctx: &mut BuildContext,
  id: Entity,
  sight: char,
  primary: bool,
  column: usize,
  column_span: usize,
  row: usize,
) -> Entity {
  let mut button = Button::create()
    .class("single_content")
    .min_size(48.0, 48.0)
    .text(sight.to_string())
    .class("square")
    .on_click(move |states, _| -> bool {
      states.get_mut::<MainViewState>(id).action(Action::Operator(sight));
      true
    })
    .attach(Grid::column(column))
    .attach(Grid::column_span(column_span))
    .attach(Grid::row(row));

  if primary {
    button = button.class("primary");
  }

  button.build(ctx)
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
