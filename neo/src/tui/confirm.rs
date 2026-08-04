use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::Modifier,
    text::{Line, Span},
    widgets::Widget,
};
use crate::theme::Theme;

pub struct Confirm<'a> {
    theme: &'a Theme,
    prompt: &'a str,
    value: bool,
}

impl<'a> Confirm<'a> {
    pub fn new(theme: &'a Theme, prompt: &'a str, value: bool) -> Self {
        Self { theme, prompt, value }
    }
}

impl<'a> Widget for Confirm<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let y_style = if self.value {
            self.theme.style_accent().add_modifier(Modifier::BOLD)
        } else {
            self.theme.style_muted()
        };

        let n_style = if !self.value {
            self.theme.style_accent().add_modifier(Modifier::BOLD)
        } else {
            self.theme.style_muted()
        };

        // We use buf.set_line but we need to handle the newline manually or use Paragraph
        // Since Confirm is simple, let's use two lines
        
        let first_line = Line::from(vec![
            Span::styled(format!("  {} ", self.prompt), self.theme.style_text().add_modifier(Modifier::BOLD)),
            Span::styled("Y", y_style),
            Span::styled("/", self.theme.style_muted()),
            Span::styled("n", n_style),
        ]);
        
        let second_line = Line::from(vec![
            Span::styled("  ↵ Enter to confirm", self.theme.style_muted()),
        ]);

        buf.set_line(area.x, area.y, &first_line, area.width);
        buf.set_line(area.x, area.y + 1, &second_line, area.width);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_confirm_render() {
        let theme = Theme::neo();
        let widget = Confirm::new(&theme, "Proceed?", true);
        let area = Rect::new(0, 0, 40, 2);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("Proceed?"));
        assert!(content.contains("Y"));
        assert!(content.contains("n"));
        assert!(content.contains("Enter to confirm"));
    }
}
