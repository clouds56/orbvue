use orbtk::prelude::*;
use orbtk::theme::DEFAULT_THEME_CSS;

mod main_view;
use main_view::*;

fn get_theme() -> ThemeValue {
    ThemeValue::create_from_css(DEFAULT_THEME_CSS)
        .build()
}

fn main() {
    Application::new()
        .window(|ctx| {
            Window::create()
                .title("OrbTk - Calculator example")
                .position((100.0, 100.0))
                .size(212.0, 336.0)
                .theme(get_theme())
                .child(MainView::create().build(ctx))
                .build(ctx)
        })
        .run();
}
