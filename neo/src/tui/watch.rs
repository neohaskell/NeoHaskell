use ratatui::{
    layout::{Constraint, Direction, Layout},
    style::Modifier,
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
    crossterm::event::{Event, KeyCode, KeyModifiers},
};
use crate::theme::Theme;
use crate::app::{State, Action};
use std::time::SystemTime;

use crate::tui::spinner::Spinner;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WatchStatus {
    Running,
    Success,
    Error,
}

pub struct WatchState {
    pub theme: Theme,
    pub status: WatchStatus,
    pub output: Vec<String>,
    pub last_updated: SystemTime,
    pub command_name: String,
    #[allow(dead_code)]
    pub should_quit: bool,
    pub frame: usize,
}

impl WatchState {
    pub fn new(theme: Theme, command_name: String) -> Self {
        Self {
            theme,
            status: WatchStatus::Running,
            output: Vec::new(),
            last_updated: SystemTime::now(),
            command_name,
            should_quit: false,
            frame: 0,
        }
    }

    pub fn set_status(&mut self, status: WatchStatus, output: Vec<String>) {
        self.status = status;
        self.output = output;
        self.last_updated = SystemTime::now();
    }
}

impl State for WatchState {
    type Output = ();

    fn view(&self, frame: &mut Frame) {
        let area = frame.area();
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3), // Header
                Constraint::Min(0),      // Content
                Constraint::Length(1), // Footer
            ])
            .split(area);

        // Header
        let header_block = Block::default()
            .borders(Borders::BOTTOM)
            .border_style(self.theme.style_muted());
        let header_text = format!(" neo {} --watch", self.command_name);
        let header = Paragraph::new(Line::from(vec![
            Span::styled(header_text, self.theme.style_primary().add_modifier(Modifier::BOLD)),
        ])).block(header_block);
        frame.render_widget(header, chunks[0]);

        // Content
        let content_area = chunks[1];
        match self.status {
            WatchStatus::Running => {
                let spinner = Spinner::new(&self.theme, self.frame).with_label("Running...");
                frame.render_widget(spinner, content_area);
            }
            WatchStatus::Success => {
                let p = Paragraph::new(vec![
                    Line::from(""),
                    Line::from(vec![
                        Span::styled("  ✓ ", self.theme.style_success().add_modifier(Modifier::BOLD)),
                        Span::styled("All good! No errors.", self.theme.style_success()),
                    ]),
                ]);
                frame.render_widget(p, content_area);
            }
            WatchStatus::Error => {
                let mut lines = vec![Line::from("")];
                for line in &self.output {
                    lines.push(Line::from(line.as_str()));
                }
                let p = Paragraph::new(lines);
                frame.render_widget(p, content_area);
            }
        }

        // Footer
        let footer_area = chunks[2];
        let status_text = match self.status {
            WatchStatus::Running => "Running",
            WatchStatus::Success => "Success",
            WatchStatus::Error => "Error",
        };
        
        let footer_text = format!(" Watching... (Ctrl+C to stop) | Status: {} ", status_text);
        let footer = Paragraph::new(Line::from(vec![
            Span::styled(footer_text, self.theme.style_muted()),
        ]));
        frame.render_widget(footer, footer_area);
    }

    fn update(&mut self, event: Event) -> miette::Result<Action<Self::Output>> {
        if let Event::Key(key) = event {
            if key.code == KeyCode::Char('c') && key.modifiers.contains(KeyModifiers::CONTROL) {
                return Ok(Action::Quit(()));
            }
            if key.code == KeyCode::Char('q') {
                return Ok(Action::Quit(()));
            }
        }
        Ok(Action::Continue)
    }

    fn tick(&mut self) {
        self.frame += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::crossterm::event::{KeyEvent, KeyModifiers};

    #[test]
    fn test_watch_state_init() {
        let theme = Theme::neo();
        let state = WatchState::new(theme, "build".to_string());
        assert_eq!(state.status, WatchStatus::Running);
        assert_eq!(state.command_name, "build");
        assert!(!state.should_quit);
    }

    #[test]
    fn test_watch_state_set_status() {
        let theme = Theme::neo();
        let mut state = WatchState::new(theme, "run".to_string());
        let output = vec!["All good".to_string()];
        state.set_status(WatchStatus::Success, output.clone());
        assert_eq!(state.status, WatchStatus::Success);
        assert_eq!(state.output, output);
    }

    #[test]
    fn test_watch_state_update_quit() {
        let theme = Theme::neo();
        let mut state = WatchState::new(theme, "test".to_string());
        
        // Test Ctrl+C
        let event = Event::Key(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
        let action = state.update(event).unwrap();
        assert!(matches!(action, Action::Quit(())));

        // Test 'q'
        let event = Event::Key(KeyEvent::new(KeyCode::Char('q'), KeyModifiers::NONE));
        let action = state.update(event).unwrap();
        assert!(matches!(action, Action::Quit(())));
    }
}
