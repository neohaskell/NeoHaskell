use ratatui::{
    layout::Rect,
    style::Modifier,
    text::{Line, Span},
    widgets::{Paragraph, Widget},
    buffer::Buffer,
};
use crate::theme::Theme;

pub struct Prompt<'a> {
    pub theme: &'a Theme,
    pub label: &'a str,
    pub value: &'a str,
    pub placeholder: &'a str,
    pub cursor_visible: bool,
}

impl<'a> Prompt<'a> {
    pub fn new(theme: &'a Theme, label: &'a str, value: &'a str, placeholder: &'a str, cursor_visible: bool) -> Self {
        Self { theme, label, value, placeholder, cursor_visible }
    }
}

impl<'a> Widget for Prompt<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let label_span = Span::styled(format!("  {} ", self.label), self.theme.style_text().add_modifier(Modifier::BOLD));
        
        let value_span = if self.value.is_empty() {
            Span::styled(self.placeholder, self.theme.style_muted())
        } else {
            Span::styled(self.value, self.theme.style_accent())
        };

        let cursor_span = if self.cursor_visible {
            Span::styled("█", self.theme.style_primary())
        } else {
            Span::raw(" ")
        };

        let line = Line::from(vec![
            label_span,
            Span::raw("\n  ❯ "),
            value_span,
            cursor_span,
        ]);

        let p = Paragraph::new(line);
        p.render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_prompt_render() {
        let theme = Theme::neo();
        let widget = Prompt::new(&theme, "Test Label", "test value", "placeholder", true);
        let area = Rect::new(0, 0, 40, 4);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        // Check content
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("Test Label"));
        assert!(content.contains("test value"));
        assert!(content.contains("█"));
    }

    #[test]
    fn test_prompt_render_no_cursor() {
        let theme = Theme::neo();
        let widget = Prompt::new(&theme, "Label", "value", "placeholder", false);
        let area = Rect::new(0, 0, 40, 4);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(!content.contains("█"));
    }

    #[test]
    fn test_prompt_render_placeholder() {
        let theme = Theme::neo();
        let widget = Prompt::new(&theme, "Label", "", "my-placeholder", true);
        let area = Rect::new(0, 0, 40, 4);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("my-placeholder"));
    }
}
