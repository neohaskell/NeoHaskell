use ratatui::{
    layout::Rect,
    style::Modifier,
    text::{Line, Span},
    widgets::{Paragraph, Widget},
    buffer::Buffer,
};
use crate::theme::Theme;

pub struct Selection<'a> {
    pub theme: &'a Theme,
    pub label: &'a str,
    pub options: &'a [&'a str],
    pub selected_index: usize,
}

impl<'a> Selection<'a> {
    pub fn new(theme: &'a Theme, label: &'a str, options: &'a [&'a str], selected_index: usize) -> Self {
        Self { theme, label, options, selected_index }
    }
}

impl<'a> Widget for Selection<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let label_span = Span::styled(format!("  {} ", self.label), self.theme.style_text().add_modifier(Modifier::BOLD));
        
        let mut lines = vec![Line::from(label_span)];
        
        for (i, option) in self.options.iter().enumerate() {
            if i == self.selected_index {
                lines.push(Line::from(vec![
                    Span::styled("  ❯ ", self.theme.style_primary()),
                    Span::styled(*option, self.theme.style_accent().add_modifier(Modifier::BOLD)),
                ]));
            } else {
                lines.push(Line::from(vec![
                    Span::raw("    "),
                    Span::styled(*option, self.theme.style_muted()),
                ]));
            }
        }

        let p = Paragraph::new(lines);
        p.render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_selection_render() {
        let theme = Theme::neo();
        let options = vec!["Option 1", "Option 2"];
        let widget = Selection::new(&theme, "Test Label", &options, 0);
        let area = Rect::new(0, 0, 40, 5);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("Test Label"));
        assert!(content.contains("Option 1"));
        assert!(content.contains("Option 2"));
        assert!(content.contains("❯"));
    }

    #[test]
    fn test_selection_render_second_option() {
        let theme = Theme::neo();
        let options = vec!["Option A", "Option B"];
        let widget = Selection::new(&theme, "Label", &options, 1);
        let area = Rect::new(0, 0, 40, 5);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("Option A"));
        assert!(content.contains("Option B"));
        assert!(content.contains("❯"));
    }
}
