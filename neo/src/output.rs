pub use ratatui::DefaultTerminal;

pub enum OutputMode {
    Interactive,
    Ci,
}

impl OutputMode {
    pub fn is_ci(&self) -> bool {
        matches!(self, Self::Ci)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_output_mode_is_ci() {
        assert!(OutputMode::Ci.is_ci());
    }
}
