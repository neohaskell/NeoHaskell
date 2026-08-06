use crate::output::OutputMode;
use crate::config::NeoConfig;
use crate::subprocess::ghci::GhciSession;
use crate::tui::watch::{WatchState, WatchStatus};
use crate::app::{Action, State};
use crate::theme::Theme;
use ratatui::crossterm::event::{self};
use std::time::Duration;
use notify::{Watcher, RecursiveMode};
use tokio::sync::mpsc;
use miette::IntoDiagnostic;

pub async fn run_watch(command_name: &str, output_mode: &mut OutputMode) -> miette::Result<()> {
    let mut terminal = match output_mode {
        OutputMode::Interactive => {
            ratatui::crossterm::terminal::enable_raw_mode().into_diagnostic()?;
            ratatui::crossterm::execute!(std::io::stdout(), ratatui::crossterm::terminal::EnterAlternateScreen).into_diagnostic()?;
            let backend = ratatui::backend::CrosstermBackend::new(std::io::stdout());
            ratatui::Terminal::new(backend).into_diagnostic()?
        },
        OutputMode::Ci => miette::bail!("Watch mode is not supported in CI mode"),
    };

    let theme = Theme::neo();
    let mut state = WatchState::new(theme, command_name.to_string());
    
    // Start GHCi session
    let mut ghci = GhciSession::start().await?;
    
    // Setup file watcher
    let (tx, mut rx) = mpsc::channel(100);
    let mut watcher = notify::RecommendedWatcher::new(move |res| {
        let _ = tx.blocking_send(res);
    }, notify::Config::default()).into_diagnostic()?;
    
    watcher.watch(std::path::Path::new("src"), RecursiveMode::Recursive).into_diagnostic()?;

    loop {
        terminal.draw(|frame| state.view(frame)).into_diagnostic()?;

        // Handle events
        tokio::select! {
            // File events
            Some(res) = rx.recv() => {
                match res {
                    Ok(event) => {
                        if event.kind.is_modify() {
                            state.status = WatchStatus::Running;
                            terminal.draw(|frame| state.view(frame)).into_diagnostic()?;
                            
                            // Reconcile before reload
                            let config = NeoConfig::load("neo.json")?;
                            crate::reconcile::run(".", &config).await?;
                            
                            match ghci.reload().await {
                                Ok(output) => {
                                    let has_errors = output.iter().any(|l| l.contains("error:"));
                                    if has_errors {
                                        state.set_status(WatchStatus::Error, output);
                                    } else {
                                        state.set_status(WatchStatus::Success, output);
                                    }
                                }
                                Err(e) => {
                                    state.set_status(WatchStatus::Error, vec![e.to_string()]);
                                }
                            }
                        }
                    }
                    Err(e) => {
                        state.set_status(WatchStatus::Error, vec![format!("Watcher error: {}", e)]);
                    }
                }
            }
            // Terminal events
            _ = tokio::time::sleep(Duration::from_millis(80)) => {
                if event::poll(Duration::from_millis(0)).into_diagnostic()? {
                    let event = event::read().into_diagnostic()?;
                    match state.update(event)? {
                        Action::Continue => {}
                        Action::Quit(_) => break,
                    }
                }
                state.tick();
            }
        }
    }

    ghci.stop().await?;
    
    if matches!(output_mode, OutputMode::Interactive) {
        ratatui::crossterm::execute!(std::io::stdout(), ratatui::crossterm::terminal::LeaveAlternateScreen).into_diagnostic()?;
        ratatui::crossterm::terminal::disable_raw_mode().into_diagnostic()?;
    }
    
    Ok(())
}
