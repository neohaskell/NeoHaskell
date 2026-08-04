use crate::output::OutputMode;
use crate::prereqs;
use crate::config::NeoConfig;
use crate::commands::watch_common;
use crate::subprocess::hurl;
use crate::subprocess::nix;
use std::time::Duration;
use tokio::time::sleep;
use crate::tui::spinner::Spinner;
use crate::tui::progress::ProgressBar;
use crate::theme::Theme;
use ratatui::layout::{Layout, Direction, Constraint};
use miette::IntoDiagnostic;

/// The port the generated app binds by default (and the port the bundled Hurl
/// scenarios target). Kept in sync with the starter's `Config` default.
const APP_PORT: u16 = 8080;

/// Poll the running app until it accepts an HTTP request, so Hurl tests don't race
/// a still-booting server.
///
/// Replaces a fixed 2-second sleep that was too short for a cold, freshly-built
/// binary — the source of spurious "connection refused" Hurl failures. Any HTTP
/// response (even a 404) proves the server is up and routing, which is all Hurl
/// needs; a connection error means "not ready yet — retry".
async fn wait_for_app_ready() -> miette::Result<()> {
    let url = format!("http://127.0.0.1:{APP_PORT}/");
    const TIMEOUT: Duration = Duration::from_secs(60);
    const INTERVAL: Duration = Duration::from_millis(250);

    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(2))
        .build()
        .into_diagnostic()?;

    let start = std::time::Instant::now();
    loop {
        if client.get(&url).send().await.is_ok() {
            return Ok(());
        }
        if start.elapsed() >= TIMEOUT {
            return Err(crate::errors::NeoError::SubprocessFailed {
                operation: "waiting for the app to become ready before running Hurl tests".to_string(),
                cause: format!(
                    "the app did not respond on {url} within {}s of `cabal run all` starting",
                    TIMEOUT.as_secs()
                ),
                fix: format!(
                    "Run `neo run` in another terminal and confirm the app boots and prints `Starting WebTransport server on port {APP_PORT}`. If it crashes on startup, fix that error first (a bad `Config`/`.env`, a failed DB connection, or a panic in `App.hs`). If your app serves a non-default port, the bundled Hurl scenarios under `tests/` must target that port too."
                ),
            }.into());
        }
        sleep(INTERVAL).await;
    }
}

pub async fn run(watch: bool, output_mode: &mut OutputMode) -> miette::Result<()> {
    prereqs::require_nix().await?;
    prereqs::require_git().await?;
    prereqs::warn_direnv(output_mode).await;
    
    let config = NeoConfig::load("neo.json")?;
    
    if output_mode.is_ci() {
        println!("[info] Reconciling project artifacts...");
    }
    crate::reconcile::run(".", &config).await?;
    
    if watch {
        watch_common::run_watch("test", output_mode).await?;
    } else {
        if output_mode.is_ci() {
            println!("[info] Running unit tests...");
        }
        
        nix::test(output_mode).await?;

        if output_mode.is_ci() {
            println!("[ok] Unit tests passed");
        }

        // Integration tests (Hurl)
        let hurl_tests = hurl::discover_tests(None).await?;
        if !hurl_tests.is_empty() {
            if output_mode.is_ci() {
                println!("[info] Running {} Hurl integration tests...", hurl_tests.len());
            }

            // Start the app in the background
            let app_child = nix::spawn_app().await?;

            // Poll until the app is actually serving before firing Hurl requests —
            // a fixed sleep raced the (cold, freshly-built) server and produced
            // spurious connection-refused failures.
            if output_mode.is_ci() {
                println!("[info] Waiting for the app to become ready on port {}...", APP_PORT);
            }
            if let Err(e) = wait_for_app_ready().await {
                // Don't leak the spawned app if it never came up.
                nix::kill_app(app_child).await;
                return Err(e);
            }

            let mut passed = 0;
            let mut failed = 0;
            let start_time = std::time::Instant::now();
            let total_tests = hurl_tests.len();
            let theme = Theme::neo();
            let mut frame = 0;

            let mut terminal: Option<ratatui::Terminal<ratatui::backend::CrosstermBackend<std::io::Stdout>>> = None;

            for (i, test_path) in hurl_tests.iter().enumerate() {
                if matches!(output_mode, OutputMode::Interactive) {
                    if terminal.is_none() {
                        ratatui::crossterm::terminal::enable_raw_mode().unwrap();
                        let backend = ratatui::backend::CrosstermBackend::new(std::io::stdout());
                        terminal = Some(ratatui::Terminal::with_options(
                            backend,
                            ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(3) }
                        ).unwrap());
                        ratatui::crossterm::terminal::disable_raw_mode().unwrap();
                    }
                    if let Some(t) = &mut terminal {
                        t.draw(|f| {
                        let chunks = Layout::default()
                            .direction(Direction::Vertical)
                            .constraints([
                                Constraint::Length(1), // Spinner
                                Constraint::Length(1), // Progress bar
                                Constraint::Min(0),
                            ])
                            .split(f.area());

                        let label = format!("Running: {}", test_path.display());
                        let spinner = Spinner::new(&theme, frame).with_label(&label);
                        f.render_widget(spinner, chunks[0]);

                        let progress = i as f64 / total_tests as f64;
                        let bar_label = format!("Test {}/{}", i + 1, total_tests);
                        let bar = ProgressBar::new(&theme, progress)
                            .with_label(&bar_label);
                        f.render_widget(bar, chunks[1]);
                    }).ok();
                    }
                    frame += 1;
                }

                match hurl::run_test(test_path, output_mode).await {
                    Ok(result) => {
                        if result.success {
                            passed += 1;
                            if output_mode.is_ci() {
                                println!("[ok] {} passed ({:?})", test_path.display(), result.duration);
                            }
                        } else {
                            failed += 1;
                            if output_mode.is_ci() {
                                println!("[fail] {} failed", test_path.display());
                            }
                        }
                    }
                    Err(e) => {
                        failed += 1;
                        if output_mode.is_ci() {
                            // Print the diagnostic compactly; don't double up — the caller will
                            // also see the final `cabal test all failed: …` aggregate below.
                            eprintln!("[fail] {}: {}", test_path.display(), e);
                        }
                    }
                }
            }

            // Tear down the app and every descendant nix develop / bash / cabal / app spawned.
            nix::kill_app(app_child).await;

            let total_duration = start_time.elapsed();

            if output_mode.is_ci() {
                println!("\nTest Summary:");
                println!("  Passed:   {}", passed);
                println!("  Failed:   {}", failed);
                println!("  Duration: {:?}", total_duration);
            } else if matches!(output_mode, OutputMode::Interactive) {
                let mut terminal = ratatui::Terminal::with_options(
                    ratatui::backend::CrosstermBackend::new(std::io::stdout()),
                    ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(13) }
                ).into_diagnostic()?;
                
                if failed == 0 {
                    let msg = format!("All tests passed! ({} passed in {:?})", passed, total_duration);
                    crate::tui::success::SuccessDisplay::show_one_shot(&theme, &msg, &mut terminal).await?;
                } else {
                    terminal.draw(|f| {
                        use ratatui::widgets::Paragraph;
                        let summary = format!(
                            "Tests FAILED: {} passed, {} failed\nDuration: {:?}",
                            passed, failed, total_duration
                        );
                        f.render_widget(Paragraph::new(summary).style(theme.style_error()), f.area());
                    }).ok();
                    tokio::time::sleep(Duration::from_secs(3)).await;
                }
            }

            if failed > 0 {
                return Err(crate::errors::NeoError::SubprocessFailed {
                    operation: "running Hurl integration tests".to_string(),
                    cause: format!("{} of {} tests failed", failed, passed + failed),
                    fix: "Inspect the failing test output above to identify which assertions failed. Common causes: (a) the server under test is not running — start it with `neo run` in another terminal, (b) request fixtures are stale — update the `.hurl` files in `tests/` to match the current API, (c) the response shape changed — update the assertions.".to_string(),
                }.into());
            }
        } else {
            if output_mode.is_ci() {
                println!("[info] No Hurl integration tests found in tests/");
            }
        }
    }
    
    Ok(())
}
