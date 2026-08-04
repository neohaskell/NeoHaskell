use ratatui::{
    layout::Alignment,
    style::{Modifier, Style},
    widgets::{Block, BorderType, Borders, Paragraph},
};
use crate::theme::Theme;

pub struct Footer<'a> {
    theme: &'a Theme,
    version: &'a str,
    update_available: Option<&'a str>,
}

impl<'a> Footer<'a> {
    pub fn new(theme: &'a Theme, version: &'a str, update_available: Option<&'a str>) -> Self {
        Self {
            theme,
            version,
            update_available,
        }
    }
}

impl<'a> ratatui::widgets::Widget for Footer<'a> {
    fn render(self, area: ratatui::layout::Rect, buf: &mut ratatui::buffer::Buffer) {
        let text = if let Some(new_version) = self.update_available {
            format!(" Neo v{} → v{} available! | Ctrl+C to quit ", self.version, new_version)
        } else {
            format!(" Neo v{} | Ctrl+C to quit ", self.version)
        };

        let style = if self.update_available.is_some() {
            Style::default().fg(self.theme.accent).add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(self.theme.muted)
        };

        let paragraph = Paragraph::new(text)
            .style(style)
            .alignment(Alignment::Center)
            .block(
                Block::default()
                    .borders(Borders::TOP)
                    .border_type(BorderType::Plain)
                    .border_style(Style::default().fg(self.theme.muted)),
            );

        paragraph.render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use ratatui::widgets::Widget;

    #[test]
    fn test_footer_render() {
        let theme = Theme::neo();
        let widget = Footer::new(&theme, "0.1.0", None);
        let area = Rect::new(0, 0, 40, 2);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("v0.1.0"));
        assert!(content.contains("Ctrl+C to quit"));
    }

    #[test]
    fn test_footer_render_update() {
        let theme = Theme::neo();
        let widget = Footer::new(&theme, "0.1.0", Some("0.2.0"));
        let area = Rect::new(0, 0, 60, 2);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("v0.1.0"));
        assert!(content.contains("v0.2.0"));
        assert!(content.contains("available!"));
    }
}
