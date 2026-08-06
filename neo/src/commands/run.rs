use crate::output::OutputMode;
use crate::prereqs;
use crate::config::NeoConfig;
use crate::subprocess::nix;
use crate::commands::watch_common;
use miette::IntoDiagnostic;

pub async fn run(watch: bool, output_mode: &mut OutputMode) -> miette::Result<()> {
    prereqs::require_nix().await?;
    prereqs::warn_direnv(output_mode).await;
    
    let config = NeoConfig::load("neo.json")?;
    
    if output_mode.is_ci() {
        println!("[info] Reconciling project artifacts...");
    }
    crate::reconcile::run(".", &config).await?;
    
    if watch {
        watch_common::run_watch("run", output_mode).await?;
    } else {
        let start = std::time::Instant::now();
        if output_mode.is_ci() {
            println!("[info] Running project...");
        }
        nix::run(output_mode).await?;
        let duration = start.elapsed();
        
        if output_mode.is_ci() {
            println!("[ok] Project finished in {:.1}s", duration.as_secs_f64());
        } else {
            let theme = crate::theme::Theme::neo();
            let mut terminal = ratatui::Terminal::with_options(
                ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(13) }
            ).into_diagnostic()?;
            let msg = format!("Project finished in {:.1}s", duration.as_secs_f64());
            crate::tui::success::SuccessDisplay::show_one_shot(&theme, &msg, &mut terminal).await?;
        }
    }
    
    Ok(())
}
