use ratatui::{
    buffer::Buffer,
    layout::Rect,
    widgets::{Gauge, Widget},
};
use crate::theme::Theme;

pub struct ProgressBar<'a> {
    theme: &'a Theme,
    progress: f64, // 0.0 to 1.0
    label: Option<&'a str>,
}

impl<'a> ProgressBar<'a> {
    pub fn new(theme: &'a Theme, progress: f64) -> Self {
        Self {
            theme,
            progress,
            label: None,
        }
    }

    pub fn with_label(mut self, label: &'a str) -> Self {
        self.label = Some(label);
        self
    }
}

impl<'a> Widget for ProgressBar<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut gauge = Gauge::default()
            .gauge_style(self.theme.style_primary())
            .style(self.theme.style_muted())
            .ratio(self.progress.clamp(0.0, 1.0));
            
        if let Some(label) = self.label {
            gauge = gauge.label(label);
        }

        gauge.render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_progress_bar_render() {
        let theme = Theme::neo();
        let widget = ProgressBar::new(&theme, 0.5).with_label("50%");
        let area = Rect::new(0, 0, 20, 1);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);
        
        // Check that something was rendered
        let non_empty = buf.content().iter().filter(|c| c.symbol() != " ").count();
        assert!(non_empty > 0);
        
        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("50%"));
    }
}
