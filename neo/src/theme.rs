use ratatui::style::{Color, Modifier, Style};

#[derive(Clone)]
pub struct Theme {
    pub primary: Color,
    pub success: Color,
    pub error: Color,
    #[allow(dead_code)]
    pub warning: Color,
    #[allow(dead_code)]
    pub info: Color,
    pub muted: Color,
    pub text: Color,
    #[allow(dead_code)]
    pub bg: Color,
    pub accent: Color,
}

impl Theme {
    pub fn neo() -> Self {
        Self {
            primary: Color::from_u32(0x0050E0D0), // Teal
            success: Color::from_u32(0x0066D96E), // Green
            error: Color::from_u32(0x00FF6B6B),   // Soft red
            warning: Color::from_u32(0x00FFD93D), // Amber
            info: Color::from_u32(0x006BC5F0),    // Sky blue
            muted: Color::from_u32(0x006C757D),   // Gray
            text: Color::from_u32(0x00F8F9FA),    // Near-white
            bg: Color::Reset,
            accent: Color::from_u32(0x00BB86FC),  // Purple
        }
    }

    pub fn style_error(&self) -> Style {
        Style::default().fg(self.error).add_modifier(Modifier::BOLD)
    }

    pub fn style_success(&self) -> Style {
        Style::default().fg(self.success).add_modifier(Modifier::BOLD)
    }

    #[allow(dead_code)]
    pub fn style_warning(&self) -> Style {
        Style::default().fg(self.warning)
    }

    pub fn style_primary(&self) -> Style {
        Style::default().fg(self.primary)
    }

    pub fn style_text(&self) -> Style {
        Style::default().fg(self.text)
    }

    pub fn style_muted(&self) -> Style {
        Style::default().fg(self.muted)
    }

    pub fn style_accent(&self) -> Style {
        Style::default().fg(self.accent)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neo_theme_colors() {
        let theme = Theme::neo();
        assert_eq!(theme.primary, Color::from_u32(0x0050E0D0));
        assert_eq!(theme.success, Color::from_u32(0x0066D96E));
        assert_eq!(theme.error, Color::from_u32(0x00FF6B6B));
    }

    #[test]
    fn test_theme_styles() {
        let theme = Theme::neo();
        
        let error_style = theme.style_error();
        assert_eq!(error_style.fg, Some(theme.error));
        assert!(error_style.add_modifier.contains(Modifier::BOLD));

        let success_style = theme.style_success();
        assert_eq!(success_style.fg, Some(theme.success));
        assert!(success_style.add_modifier.contains(Modifier::BOLD));

        assert_eq!(theme.style_primary().fg, Some(theme.primary));
        assert_eq!(theme.style_accent().fg, Some(theme.accent));
        assert_eq!(theme.style_muted().fg, Some(theme.muted));
        assert_eq!(theme.style_text().fg, Some(theme.text));
        assert_eq!(theme.style_warning().fg, Some(theme.warning));
    }
}
