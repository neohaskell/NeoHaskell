use ratatui::{
    buffer::Buffer,
    layout::Rect,
    text::Span,
    widgets::Widget,
};
use crate::theme::Theme;

pub struct Spinner<'a> {
    theme: &'a Theme,
    frames: &'static [&'static str],
    current_frame: usize,
    label: Option<&'a str>,
}

impl<'a> Spinner<'a> {
    pub const FRAMES: &'static [&'static str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

    pub fn new(theme: &'a Theme, current_frame: usize) -> Self {
        Self {
            theme,
            frames: Self::FRAMES,
            current_frame,
            label: None,
        }
    }

    pub fn with_label(mut self, label: &'a str) -> Self {
        self.label = Some(label);
        self
    }
}

impl<'a> Widget for Spinner<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let frame = self.frames[self.current_frame % self.frames.len()];
        let span = Span::styled(frame, self.theme.style_primary());
        
        buf.set_span(area.x, area.y, &span, area.width);

        if let Some(label) = self.label {
            if area.width > 2 {
                let label_span = Span::styled(format!(" {}", label), self.theme.style_text());
                buf.set_span(area.x + span.width() as u16, area.y, &label_span, area.width - span.width() as u16);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_spinner_render() {
        let theme = Theme::neo();
        let widget = Spinner::new(&theme, 0).with_label("Loading...");
        let area = Rect::new(0, 0, 20, 1);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains(Spinner::FRAMES[0]));
        assert!(content.contains("Loading..."));
    }

    #[test]
    fn test_spinner_animation() {
        let theme = Theme::neo();
        let area = Rect::new(0, 0, 1, 1);
        
        let widget0 = Spinner::new(&theme, 0);
        let mut buf0 = Buffer::empty(area);
        widget0.render(area, &mut buf0);
        
        let widget1 = Spinner::new(&theme, 1);
        let mut buf1 = Buffer::empty(area);
        widget1.render(area, &mut buf1);
        
        assert_ne!(buf0.content()[0].symbol(), buf1.content()[0].symbol());
        assert_eq!(buf0.content()[0].symbol(), Spinner::FRAMES[0]);
        assert_eq!(buf1.content()[0].symbol(), Spinner::FRAMES[1]);
    }
}
