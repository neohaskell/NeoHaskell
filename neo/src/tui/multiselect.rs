use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::Modifier,
    text::{Line, Span},
    widgets::{Paragraph, Widget},
};
use crate::theme::Theme;

/// A checkbox multi-select list (Tier B1, inline — no box drawing).
///
/// `cursor` is the highlighted row; `checked` parallels `options`.
/// `descriptions` (optional, parallel) renders a muted hint after each option.
pub struct MultiSelect<'a> {
    pub theme: &'a Theme,
    pub label: &'a str,
    pub options: &'a [&'a str],
    pub descriptions: &'a [&'a str],
    pub checked: &'a [bool],
    pub cursor: usize,
}

impl<'a> MultiSelect<'a> {
    pub fn new(
        theme: &'a Theme,
        label: &'a str,
        options: &'a [&'a str],
        descriptions: &'a [&'a str],
        checked: &'a [bool],
        cursor: usize,
    ) -> Self {
        Self { theme, label, options, descriptions, checked, cursor }
    }
}

impl<'a> Widget for MultiSelect<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut lines = vec![Line::from(Span::styled(
            format!("  {} ", self.label),
            self.theme.style_text().add_modifier(Modifier::BOLD),
        ))];

        for (i, option) in self.options.iter().enumerate() {
            let is_cursor = i == self.cursor;
            let is_checked = self.checked.get(i).copied().unwrap_or(false);

            let cursor_span = if is_cursor {
                Span::styled("  ❯ ", self.theme.style_primary())
            } else {
                Span::raw("    ")
            };
            let checkbox = if is_checked { "[x] " } else { "[ ] " };
            let checkbox_span = Span::styled(
                checkbox,
                if is_checked { self.theme.style_accent() } else { self.theme.style_muted() },
            );
            let name_style = if is_cursor {
                self.theme.style_accent().add_modifier(Modifier::BOLD)
            } else if is_checked {
                self.theme.style_text()
            } else {
                self.theme.style_muted()
            };

            let mut spans = vec![cursor_span, checkbox_span, Span::styled(*option, name_style)];
            if let Some(desc) = self.descriptions.get(i) {
                if !desc.is_empty() {
                    spans.push(Span::styled(format!("  — {}", desc), self.theme.style_muted()));
                }
            }
            lines.push(Line::from(spans));
        }

        lines.push(Line::from(Span::styled(
            "  Space toggles · Enter confirms · Esc cancels",
            self.theme.style_muted(),
        )));

        Paragraph::new(lines).render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;

    #[test]
    fn test_multiselect_render() {
        let theme = Theme::neo();
        let options = vec!["claude", "cursor"];
        let descriptions = vec![".claude/skills", ".cursor/rules"];
        let checked = vec![true, false];
        let widget = MultiSelect::new(&theme, "Select tools:", &options, &descriptions, &checked, 0);
        let area = Rect::new(0, 0, 60, 6);
        let mut buf = Buffer::empty(area);
        widget.render(area, &mut buf);

        let content = buf.content().iter().map(|c| c.symbol()).collect::<String>();
        assert!(content.contains("Select tools:"));
        assert!(content.contains("claude"));
        assert!(content.contains("cursor"));
        assert!(content.contains("[x]"));
        assert!(content.contains("[ ]"));
        assert!(content.contains("❯"));
        // Tier B1 inline widgets must not draw frames.
        assert!(!content.contains("╔"));
    }
}
