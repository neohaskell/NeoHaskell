mod app;
mod cli;
mod commands;
mod errlog;
mod errors;
mod ide;
mod inspect;
mod interpret;
mod output;
mod config;
mod prereqs;
mod theme;
mod tui;
mod lock;
mod network;
mod git;
mod reconcile;
mod skills;
mod subprocess;
mod test_utils;

use clap::Parser;
use cli::Cli;
use output::OutputMode;

fn detect_terminal_width() -> usize {
    use std::io::IsTerminal;
    if std::io::stderr().is_terminal() {
        ratatui::crossterm::terminal::size()
            .map(|(cols, _)| (cols as usize).max(80))
            .unwrap_or(120)
    } else {
        // Piped / non-TTY (CI logs, test runners, `2>&1 | …`): use a wide
        // width so consumers can do their own wrapping and substring matchers
        // are not derailed by mid-phrase line breaks.
        200
    }
}

#[tokio::main]
async fn main() -> miette::Result<()> {
    let stderr_is_tty = {
        use std::io::IsTerminal;
        std::io::stderr().is_terminal()
    };
    miette::set_hook(Box::new(move |_| {
        Box::new(
            miette::GraphicalReportHandler::new()
                // OSC-8 hyperlinks only when stderr is a real terminal — piped
                // output should not contain raw `\x1b]8;…` escape sentinels.
                .with_links(stderr_is_tty)
                .with_context_lines(2)
                .with_width(detect_terminal_width()),
        )
    }))?;

    let cli = Cli::parse();
    
    // Detect CI environment
    let is_ci = cli.ci || std::env::var("CI").is_ok();
    
    let mut output_mode = if is_ci {
        OutputMode::Ci
    } else {
        // Check terminal size
        if let Ok((cols, _)) = ratatui::crossterm::terminal::size() {
            if cols < 60 {
                println!("\n  {} Terminal is too narrow ({} columns).", 
                    ratatui::style::Stylize::bold(ratatui::style::Stylize::yellow("⚠")),
                    cols
                );
                println!("  NeoCLI looks best in terminals at least 60 columns wide.\n");
            }
        }
        OutputMode::Interactive
    };
    
    let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
    let update_status_clone = update_status.clone();
    
    // Background update check (non-blocking)
    let _update_handle = tokio::spawn(async move {
        if let Ok(Some(latest_version)) = network::check_for_updates().await {
            let mut status = update_status_clone.lock().unwrap();
            *status = Some(latest_version);
        }
    });

    let result = app::dispatch(cli.command, &mut output_mode, update_status.clone()).await;

    if let Err(_e) = &result {
        if matches!(output_mode, OutputMode::Interactive) {
            // Just use miette's default formatting which is already nice
            // We don't need to spin up a terminal just for the error box
            // that disappears after 3 seconds.
            // miette::set_hook handles the pretty printing automatically
            // when main returns Err(e).
        }
    }

    // Show update notice if available at the end as well (for non-interactive or short-lived commands)
    let final_update_status = update_status.lock().unwrap();
    if let Some(latest_version) = &*final_update_status {
        if is_ci {
            println!("\n[info] A new version of NeoCLI is available: v{}", latest_version);
        } else {
            // Only print if not already shown in TUI or for consistency
            println!("\n  NeoCLI v{} is available! Run `neo update` to install.", latest_version);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use miette::{GraphicalReportHandler, GraphicalTheme};

    fn render(diag: &dyn miette::Diagnostic, handler: GraphicalReportHandler) -> String {
        let mut buf = String::new();
        handler.render_report(&mut buf, diag).unwrap();
        buf
    }

    #[test]
    fn graphical_handler_emits_unicode_and_color() {
        let diag = crate::errors::NeoError::NoWorkspace;
        let handler = GraphicalReportHandler::new_themed(GraphicalTheme::unicode())
            .with_links(true)
            .with_width(120);
        let rendered = render(&diag, handler);
        assert!(rendered.contains("×"), "missing unicode error glyph: {}", rendered);
        assert!(rendered.contains("help:"), "missing help block: {}", rendered);
        assert!(rendered.contains("\x1b["), "missing ANSI color codes: {:?}", rendered);
    }

    #[test]
    fn no_color_theme_strips_ansi_keeps_unicode() {
        let diag = crate::errors::NeoError::NoWorkspace;
        let handler = GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor())
            .with_links(false)
            .with_urls(false)
            .with_width(120);
        let rendered = render(&diag, handler);
        assert!(!rendered.contains("\x1b["), "unexpected ANSI in nocolor theme: {:?}", rendered);
        assert!(rendered.contains("×"), "unicode glyph missing in nocolor: {}", rendered);
    }

    #[test]
    fn none_theme_strips_ansi_and_unicode() {
        let diag = crate::errors::NeoError::NoWorkspace;
        let handler = GraphicalReportHandler::new_themed(GraphicalTheme::none())
            .with_links(false)
            .with_urls(false)
            .with_width(120);
        let rendered = render(&diag, handler);
        assert!(!rendered.contains("\x1b["), "unexpected ANSI in none theme: {:?}", rendered);
    }

    #[test]
    fn narratable_handler_not_installed() {
        // Grep guard against regressing to the plain-text screen-reader handler.
        // We assemble the forbidden substring at runtime so this very assertion
        // does not falsely trip itself.
        let src = include_str!("main.rs");
        let forbidden = format!("Box::new({}::{}HandlerNew", "miette", "Narratable")
            .replace("HandlerNew", "Handler::new()");
        assert!(
            !src.contains(&forbidden),
            "the plain-text screen-reader handler is being installed by the miette hook — \
             this regresses the rendering. Use GraphicalReportHandler instead."
        );
    }

    #[test]
    fn graphical_handler_is_installed() {
        let src = include_str!("main.rs");
        assert!(
            src.contains("GraphicalReportHandler::new()"),
            "GraphicalReportHandler::new() is not installed in the miette hook"
        );
    }
}
