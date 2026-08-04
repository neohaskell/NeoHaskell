use std::path::PathBuf;
use crossterm::event::{self, Event, KeyCode};
use miette::{IntoDiagnostic, Result};
use ratatui::{
    layout::{Constraint, Direction, Layout},
};

use crate::cli::{LockArgs, LockSubcommand};
use crate::errors::NeoError;
use crate::output::OutputMode;
use crate::lock::{LockManifest, discover_domain_files, fuzzy_match, find_locked_violations, LOCK_MANIFEST};
use crate::theme::Theme;
use crate::tui::selection::Selection;
use crate::tui::banner::Banner;
use crate::git;

use crate::tui::confirm::Confirm;
use crate::tui::success::SuccessDisplay;

pub async fn run(args: LockArgs, output_mode: &mut OutputMode) -> Result<()> {
    match args.subcommand {
        Some(LockSubcommand::Install) => {
            git::install_lock_hook(std::env::current_dir().into_diagnostic()?.as_path())?;
            match output_mode {
                OutputMode::Ci => println!("Lock hook installed"),
                OutputMode::Interactive => {
                    let mut terminal = ratatui::Terminal::with_options(
                        ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                        ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(10) }
                    ).into_diagnostic()?;
                    let theme = Theme::neo();
                    terminal.draw(|f| {
                        let chunks = Layout::default()
                            .direction(Direction::Vertical)
                            .constraints([Constraint::Length(8), Constraint::Min(0)])
                            .split(f.area());

                        let banner = Banner::new(&theme, "LOCK", "Hook Installed");
                        f.render_widget(banner, chunks[0]);

                        let success = SuccessDisplay::new(&theme, "Git pre-commit lock hook installed successfully.");
                        f.render_widget(success, chunks[1]);
                    }).into_diagnostic()?;
                    tokio::time::sleep(std::time::Duration::from_secs(2)).await;
                }
            }
            Ok(())
        }
        Some(LockSubcommand::Check) => {
            check_locked_files(output_mode).await
        }
        None => {
            if args.all {
                lock_all(output_mode).await
            } else if let Some(query) = args.search {
                lock_fuzzy(query, output_mode).await
            } else {
                lock_all(output_mode).await
            }
        }
    }
}

/// Verify that no locked file has been modified in the current working tree.
///
/// "Modified" means any of: staged for commit, unstaged in the working tree,
/// or untracked. Returns `Ok(())` when the lock manifest is missing/empty or
/// when no locked file shows up in `git status --porcelain`. On violation,
/// returns `Err(NeoError::LockViolation)` with a help block that lists every
/// offending path and the three concrete fix recipes (revert, unlock, skip).
///
/// Used by both the git pre-commit hook (`neo lock check`) and the pre-build
/// gate in `neo build`.
pub async fn check_locked_files(output_mode: &OutputMode) -> Result<()> {
    let _ = output_mode;
    let violations = find_locked_violations()?;
    if violations.is_empty() {
        return Ok(());
    }
    Err(NeoError::lock_violation(violations).into())
}

async fn lock_all(output_mode: &mut OutputMode) -> Result<()> {
    let files = discover_domain_files()?;
    if files.is_empty() {
        match output_mode {
            OutputMode::Ci => println!("No domain files found to lock."),
            OutputMode::Interactive => {
                let mut terminal = ratatui::Terminal::with_options(
                    ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                    ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(10) }
                ).into_diagnostic()?;
                let theme = Theme::neo();
                terminal.draw(|f| {
                    let banner = Banner::new(&theme, "LOCK", "No Domain Files Found");
                    f.render_widget(banner, f.area());
                }).into_diagnostic()?;
            }
        }
        return Ok(());
    }

    if files.len() > 1 {
        if matches!(output_mode, OutputMode::Interactive) {
            ratatui::crossterm::terminal::enable_raw_mode().into_diagnostic()?;
            let mut terminal = ratatui::Terminal::with_options(
                ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(10) }
            ).into_diagnostic()?;
            let prompt = format!("Lock all {} discovered domain files?", files.len());
            let res = confirm_tui(&mut terminal, &prompt).await;
            ratatui::crossterm::terminal::disable_raw_mode().into_diagnostic()?;
            if !res? {
                return Ok(());
            }
        }
    }

    let mut manifest = LockManifest::load()?;
    for file in &files {
        manifest.add(file.to_string_lossy().to_string());
    }
    manifest.save()?;

    stage_and_commit_lock(&files, output_mode).await?;

    Ok(())
}

async fn lock_fuzzy(query: String, output_mode: &mut OutputMode) -> Result<()> {
    let all_files = discover_domain_files()?;
    let matches = fuzzy_match(&query, &all_files);

    if matches.is_empty() {
        match output_mode {
            OutputMode::Ci => println!("No matches found for '{}'", query),
            OutputMode::Interactive => {
                let mut terminal = ratatui::Terminal::with_options(
                    ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                    ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(10) }
                ).into_diagnostic()?;
                let theme = Theme::neo();
                let subtitle = format!("No matches for '{}'", query);
                terminal.draw(|f| {
                    let banner = Banner::new(&theme, "LOCK", &subtitle);
                    f.render_widget(banner, f.area());
                }).into_diagnostic()?;
            }
        }
        return Ok(());
    }

    if matches.len() == 1 {
        let mut manifest = LockManifest::load()?;
        manifest.add(matches[0].to_string_lossy().to_string());
        manifest.save()?;
        stage_and_commit_lock(&matches, output_mode).await?;
        return Ok(());
    }

    match output_mode {
        OutputMode::Ci => {
            println!("Multiple matches found. Please be more specific or use --all.");
            for m in matches {
                println!("  - {}", m.to_string_lossy());
            }
        }
        OutputMode::Interactive => {
            ratatui::crossterm::terminal::enable_raw_mode().into_diagnostic()?;
            let mut terminal = ratatui::Terminal::with_options(
                ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(15) }
            ).into_diagnostic()?;
            let selected_file = select_file_tui(&mut terminal, matches).await;
            ratatui::crossterm::terminal::disable_raw_mode().into_diagnostic()?;
            if let Some(file) = selected_file? {
                let mut manifest = LockManifest::load()?;
                manifest.add(file.to_string_lossy().to_string());
                manifest.save()?;
                stage_and_commit_lock(&[file], output_mode).await?;
            }
        }
    }

    Ok(())
}

async fn confirm_tui(terminal: &mut crate::output::DefaultTerminal, prompt: &str) -> Result<bool> {
    let theme = Theme::neo();
    let mut value = true;

    loop {
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(8), Constraint::Min(0)])
                .split(f.area());

            let banner = Banner::new(&theme, "LOCK", "Confirmation Required");
            f.render_widget(banner, chunks[0]);

            let confirm = Confirm::new(&theme, prompt, value);
            f.render_widget(confirm, chunks[1]);
        }).into_diagnostic()?;

        if event::poll(std::time::Duration::from_millis(100)).into_diagnostic()? {
            if let Event::Key(key) = event::read().into_diagnostic()? {
                match key.code {
                    KeyCode::Left | KeyCode::Right | KeyCode::Char('h') | KeyCode::Char('l') => {
                        value = !value;
                    }
                    KeyCode::Char('y') | KeyCode::Char('Y') => value = true,
                    KeyCode::Char('n') | KeyCode::Char('N') => value = false,
                    KeyCode::Enter => return Ok(value),
                    KeyCode::Esc => return Ok(false),
                    _ => {}
                }
            }
        }
    }
}

async fn select_file_tui(terminal: &mut crate::output::DefaultTerminal, matches: Vec<PathBuf>) -> Result<Option<PathBuf>> {
    let theme = Theme::neo();
    let options: Vec<String> = matches.iter().map(|p| p.to_string_lossy().to_string()).collect();
    let options_str: Vec<&str> = options.iter().map(|s| s.as_str()).collect();
    let mut selected_index = 0;

    loop {
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(8), Constraint::Min(0)])
                .split(f.area());

            let banner = Banner::new(&theme, "LOCK", "Select file to lock");
            f.render_widget(banner, chunks[0]);

            let selection = Selection::new(&theme, "Matches:", &options_str, selected_index);
            f.render_widget(selection, chunks[1]);
        }).into_diagnostic()?;

        if event::poll(std::time::Duration::from_millis(100)).into_diagnostic()? {
            if let Event::Key(key) = event::read().into_diagnostic()? {
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => return Ok(None),
                    KeyCode::Up => {
                        if selected_index > 0 {
                            selected_index -= 1;
                        }
                    }
                    KeyCode::Down => {
                        if selected_index < options.len() - 1 {
                            selected_index += 1;
                        }
                    }
                    KeyCode::Enter => return Ok(Some(matches[selected_index].clone())),
                    _ => {}
                }
            }
        }
    }
}

async fn stage_and_commit_lock(files: &[PathBuf], output_mode: &mut OutputMode) -> Result<()> {
    let mut args = vec!["add", LOCK_MANIFEST];
    let file_strs: Vec<String> = files.iter().map(|p| p.to_string_lossy().to_string()).collect();
    for f in &file_strs {
        args.push(f);
    }

    std::process::Command::new("git")
        .args(&args)
        .output()
        .into_diagnostic()?;

    let commit_msg = if files.len() == 1 {
        format!("lock: {}", files[0].to_string_lossy())
    } else {
        format!("lock: {} files", files.len())
    };

    std::process::Command::new("git")
        .args(["commit", "-m", &commit_msg])
        .output()
        .into_diagnostic()?;

    match output_mode {
        OutputMode::Ci => println!("Locked and committed: {}", commit_msg),
        OutputMode::Interactive => {
            let mut terminal = ratatui::Terminal::with_options(
                ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(10) }
            ).into_diagnostic()?;
            let theme = Theme::neo();
            terminal.draw(|f| {
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Length(8), Constraint::Min(0)])
                    .split(f.area());

                let banner = Banner::new(&theme, "LOCK", "Completed");
                f.render_widget(banner, chunks[0]);

                let success = SuccessDisplay::new(&theme, &commit_msg);
                f.render_widget(success, chunks[1]);
            }).into_diagnostic()?;
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        }
    }

    Ok(())
}
