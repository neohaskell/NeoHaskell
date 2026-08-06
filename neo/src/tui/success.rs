use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    text::{Line, Span},
    widgets::{Paragraph, Widget},
};
use crate::theme::Theme;

pub struct SuccessDisplay<'a> {
    theme: &'a Theme,
    message: &'a str,
    frame: usize,
}

use miette::IntoDiagnostic;

impl<'a> SuccessDisplay<'a> {
    pub fn new(theme: &'a Theme, message: &'a str) -> Self {
        Self { theme, message, frame: 0 }
    }

    pub fn with_frame(mut self, frame: usize) -> Self {
        self.frame = frame;
        self
    }

    pub async fn show_one_shot(
        theme: &Theme,
        message: &str,
        terminal: &mut crate::output::DefaultTerminal,
    ) -> miette::Result<()> {
        for i in 0..25 {
            terminal.draw(|f| {
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(5), // Banner
                        Constraint::Min(0),     // Content
                    ])
                    .split(f.area());

                let banner = crate::tui::banner::Banner::new(theme, "NEO", "Success!").with_frame(i);
                f.render_widget(banner, chunks[0]);

                let success = SuccessDisplay::new(theme, message).with_frame(i);
                f.render_widget(success, chunks[1]);
            }).into_diagnostic()?;
            tokio::time::sleep(std::time::Duration::from_millis(80)).await;
        }
        Ok(())
    }
}

impl<'a> Widget for SuccessDisplay<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let _ = self.frame;
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Padding
                Constraint::Length(1), // Message
                Constraint::Min(0),
            ])
            .split(area);

        let success_line = Line::from(vec![
            Span::styled("✓ ", self.theme.style_success()),
            Span::styled(self.message, self.theme.style_success()),
        ]);

        Paragraph::new(success_line).render(chunks[1], buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_success_display_render() {
        let theme = Theme::neo();
        let widget = SuccessDisplay::new(&theme, "Build succeeded!");
        let area = Rect::new(0, 0, 80, 10);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);

        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("✓"));
        assert!(content.contains("Build succeeded!"));
        assert!(!content.contains("║ :)║"));
    }
}
