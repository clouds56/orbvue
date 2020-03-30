use orbtk::prelude::*;
use orbtk::theme::DEFAULT_THEME_CSS;

pub mod calculator;
pub mod clear;
pub mod widgets;
pub mod props;

use props::*;

fn get_theme() -> ThemeValue {
    ThemeValue::create_from_css(DEFAULT_THEME_CSS)
        .build()
}

fn main() {
    Application::new()
        .window(|ctx| {
            Window::create()
                .title(TITLE)
                .position((100.0, 100.0))
                .size(WINDOW_SIZE.0, WINDOW_SIZE.1)
                .theme(get_theme())
                .child(MainView::create().build(ctx))
                .build(ctx)
        })
        .run();
}
