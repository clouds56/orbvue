#[cfg(feature="derive")]
pub use orbvue_derive::build_template;

#[cfg(test)]
mod test {
  use super::*;
  use orbtk::prelude::*;

  fn generate_digit_button(ctx: &mut BuildContext, _id: Entity,
    sight: char, primary: bool, column: usize, column_span: usize, row: usize,
  ) -> Entity {
    let mut button = Button::create()
      .class("single_content")
      .min_size(48.0, 48.0)
      .text(sight.to_string())
      .on_click(|_, _| true)
      .attach(Grid::column(column))
      .attach(Grid::row(row))
      .attach(Grid::column_span(column_span));

    if primary {
      button = button.class("primary");
    }

    button.build(ctx)
  }

  fn generate_operation_button(ctx: &mut BuildContext, _id: Entity,
    sight: char, primary: bool, column: usize, column_span: usize, row: usize,
  ) -> Entity {
    let mut button = Button::create()
      .class("single_content")
      .min_size(48.0, 48.0)
      .text(sight.to_string())
      .class("square")
      .on_click(|_, _| true)
      .attach(Grid::column(column))
      .attach(Grid::column_span(column_span))
      .attach(Grid::row(row));

    if primary {
      button = button.class("primary");
    }

    button.build(ctx)
  }

  widget!(View { text: String16 });
  impl Template for View {
    build_template!{
      fn template(self, #[vue="id"] _id: Entity, #[vue="ctx"] ctx: &mut BuildContext) -> Self {
        #[vue(template)]
        self
      },
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
    }
  }
}
