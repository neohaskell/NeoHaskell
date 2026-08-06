use crate::errors::NeoError;
use crate::ide::methods::register_all;
use crate::ide::registry::MethodRegistry;
use crate::ide::transport::LocalTransport;
use crate::ide::workspace::Workspace;
use crate::ide::AppState;
use crate::output::OutputMode;
use axum::{
    http::{header, StatusCode, Uri},
    response::{IntoResponse, Response},
    routing::get,
    Router,
};
use crossterm::style::Stylize;
use rust_embed::RustEmbed;
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;

/// All files under `assets/ide/dist/` are embedded into the binary at
/// compile time in release builds. In debug builds `rust-embed` reads from
/// disk on each request (live reload during frontend iteration).
#[derive(RustEmbed)]
#[folder = "assets/ide/dist/"]
struct IdeAssets;

pub async fn run(host: IpAddr, port: u16, output_mode: &mut OutputMode) -> miette::Result<()> {
    init_tracing();

    let addr = SocketAddr::new(host, port);

    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .map_err(|e| NeoError::IdeBind { host, port, source: e })?;

    let bound = listener
        .local_addr()
        .map_err(|e| NeoError::IdeBind { host, port, source: e })?;

    let browseable = browseable_url(host, bound);
    let bind_hint = host
        .is_unspecified()
        .then_some("reachable from any interface on this host");

    print_startup(output_mode, &browseable, &bound.to_string(), bind_hint);

    // Mint the workspace from the cwd once at server boot — every WS
    // connection inherits the same `Arc<Workspace>` via the `LocalTransport`.
    // Per the design-panel "Replit mistake" rule: workspace state is
    // explicit, not implicit-in-cwd-at-handler-call-time.
    let cwd = std::env::current_dir().map_err(|e| NeoError::IdeServe { source: e })?;
    let workspace = Workspace::from_root(&cwd)?;
    let workspace_root = workspace.root.clone();
    let transport = Arc::new(LocalTransport::new(workspace));
    let registry = register_all(MethodRegistry::new());

    // Background source watcher: on every `src/` change, re-sync
    // `event-model.json` from the parsed source and broadcast so any open IDE
    // client re-reads the model. Watching `src/` (not `event-model.json`) avoids
    // a feedback loop with autosave.
    let (model_changed_tx, _) = tokio::sync::broadcast::channel::<()>(16);
    spawn_source_watcher(workspace_root, model_changed_tx.clone());

    let state = AppState { registry, transport, model_changed_tx };

    let app = Router::new()
        .route("/ws", get(crate::ide::server::ws_upgrade))
        .with_state(state)
        .fallback(serve_asset);
    axum::serve(listener, app)
        .with_graceful_shutdown(async {
            let _ = tokio::signal::ctrl_c().await;
        })
        .await
        .map_err(|e| NeoError::IdeServe { source: e })?;

    print_shutdown(output_mode);
    Ok(())
}

/// Build the URL a human can click. When the bind host is unspecified
/// (`0.0.0.0` / `::`), `http://0.0.0.0:PORT` is not browseable — substitute
/// the matching loopback address (`127.0.0.1` for v4, `[::1]` for v6) so
/// the URL we print is always clickable.
fn browseable_url(host: IpAddr, bound: SocketAddr) -> String {
    if host.is_unspecified() {
        match host {
            IpAddr::V4(_) => format!("http://127.0.0.1:{}", bound.port()),
            IpAddr::V6(_) => format!("http://[::1]:{}", bound.port()),
        }
    } else {
        // SocketAddr's Display brackets v6 addresses, so this is correct for both.
        format!("http://{bound}")
    }
}

fn print_startup(output_mode: &OutputMode, url: &str, bind: &str, bind_hint: Option<&str>) {
    if output_mode.is_ci() {
        println!("[info] Neo IDE starting");
        println!("[info]   url   {url}");
        match bind_hint {
            Some(hint) => println!("[info]   bind  {bind}  ({hint})"),
            None => println!("[info]   bind  {bind}"),
        }
        println!("[info]   logs  stderr (set RUST_LOG=neo=debug for verbose)");
        println!("[info] press Ctrl+C to stop the server");
    } else {
        // Interactive: plain `println!` with crossterm SGR styling. No ratatui
        // viewport, no raw-mode terminal takeover — the command is a long-
        // running server that has no business stealing the user's terminal.
        // The output stays scrollable, redirect-friendly, and visible even
        // when the harness only allocates a partial TTY.
        println!();
        println!("  {}", "Neo IDE is running".cyan().bold());
        println!();
        println!("  Open this in your browser:");
        println!("      {}", url.cyan().bold().underlined());
        match bind_hint {
            Some(hint) => println!("\n  Bound on {} ({})", bind, hint.dark_grey()),
            None => println!("\n  Bound on {}", bind.dark_grey()),
        }
        println!();
        println!("  Press {} to stop the server.", "Ctrl+C".bold());
        println!(
            "  {}",
            "Server logs stream to stderr. Set RUST_LOG=neo=debug for verbose output."
                .dark_grey()
        );
        println!();
    }
}

/// Initialise the tracing subscriber for `neo ide`. Sends events to stderr
/// so stdout stays clean for the URL banner and other user-facing output.
///
/// Default filter: `neo=info,warn` — every `tracing::info!` from our own
/// modules surfaces, plus warnings/errors from deps. Override with
/// `RUST_LOG=neo=debug` (or any standard `tracing-subscriber` filter spec)
/// to get more detail, e.g. per-WS-message + raw `claude -p` line-by-line.
///
/// Idempotent — `try_init` returns `Err` if a subscriber is already set,
/// which we silently swallow so tests and other callers don't double-init.
fn init_tracing() {
    use std::io::IsTerminal;
    use tracing_subscriber::EnvFilter;
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("neo=info,warn"));
    let use_ansi = std::io::stderr().is_terminal();
    let _ = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(true)
        .with_ansi(use_ansi)
        .with_writer(std::io::stderr)
        .try_init();
}

/// Spawn a detached task that watches `<root>/src` and re-runs the code→model
/// sync on every change, broadcasting on `changed_tx` whenever the sync actually
/// rewrote `event-model.json` (so connected IDE clients re-read it). Best-effort:
/// if the watcher can't start, log and disable live sync rather than fail boot.
fn spawn_source_watcher(root: std::path::PathBuf, changed_tx: crate::ide::ModelChangedTx) {
    use notify::{RecursiveMode, Watcher};

    tokio::spawn(async move {
        let src_dir = root.join("src");
        // Bridge the notify callback (sync world) into async via an mpsc.
        let (tx, mut rx) = tokio::sync::mpsc::channel::<()>(16);
        let mut watcher = match notify::RecommendedWatcher::new(
            move |res: notify::Result<notify::Event>| {
                if let Ok(ev) = res {
                    if ev.kind.is_modify() || ev.kind.is_create() || ev.kind.is_remove() {
                        let _ = tx.blocking_send(());
                    }
                }
            },
            notify::Config::default(),
        ) {
            Ok(w) => w,
            Err(e) => {
                tracing::warn!(error = %e, "ide: source watcher init failed; live code→model sync disabled");
                return;
            }
        };
        if let Err(e) = watcher.watch(&src_dir, RecursiveMode::Recursive) {
            tracing::warn!(
                error = %e, dir = %src_dir.display(),
                "ide: cannot watch src/; live code→model sync disabled",
            );
            return;
        }
        tracing::info!(dir = %src_dir.display(), "ide: watching source for code→model sync");
        // Hold `watcher` alive for the task's lifetime — dropping it stops events.
        let _watcher = watcher;
        while rx.recv().await.is_some() {
            // Coalesce a burst of filesystem events into a single sync.
            while rx.try_recv().is_ok() {}
            match crate::ide::sync::sync_event_model(&root) {
                Ok(outcome) if outcome.applied > 0 => {
                    tracing::info!(
                        applied = outcome.applied,
                        fields = outcome.fields_updated,
                        full_heal = outcome.ran_full_heal,
                        "ide: code→model sync applied; notifying clients",
                    );
                    let _ = changed_tx.send(());
                }
                Ok(_) => {}
                Err(e) => tracing::warn!(error = %e, "ide: code→model sync failed"),
            }
        }
    });
}

fn print_shutdown(output_mode: &OutputMode) {
    if output_mode.is_ci() {
        println!("[ok] Neo IDE stopped");
    } else {
        println!();
        println!("  {} {}", "✓".green().bold(), "Neo IDE stopped".green().bold());
        println!();
    }
}

async fn serve_asset(uri: Uri) -> Response {
    serve_path(uri.path())
}

fn serve_path(req_path: &str) -> Response {
    let stripped = req_path.trim_start_matches('/');
    let lookup = if stripped.is_empty() { "index.html" } else { stripped };

    match IdeAssets::get(lookup) {
        Some(file) => {
            let mime = file.metadata.mimetype().to_string();
            let cache_control = cache_control_for(lookup);
            (
                [
                    (header::CONTENT_TYPE, mime),
                    (header::CACHE_CONTROL, cache_control.to_string()),
                ],
                file.data.into_owned(),
            )
                .into_response()
        }
        None => match IdeAssets::get("index.html") {
            Some(file) => (
                [
                    (header::CONTENT_TYPE, "text/html; charset=utf-8".to_string()),
                    // SPA fallback IS index.html — must revalidate or users
                    // get a stale shell pointing at deleted bundle hashes.
                    (header::CACHE_CONTROL, "no-cache, must-revalidate".to_string()),
                ],
                file.data.into_owned(),
            )
                .into_response(),
            None => (StatusCode::NOT_FOUND, "IDE assets missing").into_response(),
        },
    }
}

/// Cache policy per asset.
///
/// - `index.html` and any other un-fingerprinted path → `no-cache,
///   must-revalidate`. The browser must ask the server every reload —
///   otherwise it keeps serving a stale shell that points at bundle
///   filenames Vite has since renamed, and the user sees the old UI
///   (or a blank page) after every rebuild.
/// - Vite's hashed bundles under `assets/*` (e.g. `assets/index-CLyk1qxs.js`)
///   → `public, max-age=31536000, immutable`. The hash in the filename
///   IS the cache buster; the file at that exact URL never changes.
fn cache_control_for(asset_path: &str) -> &'static str {
    if asset_path == "index.html"
        || asset_path == "favicon.svg"
        || !asset_path.starts_with("assets/")
    {
        "no-cache, must-revalidate"
    } else {
        "public, max-age=31536000, immutable"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::to_bytes;
    use axum::http::StatusCode;
    use std::net::Ipv4Addr;

    async fn body_string(resp: Response) -> (StatusCode, String, String) {
        let status = resp.status();
        let content_type = resp
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("")
            .to_string();
        let bytes = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
        (status, content_type, String::from_utf8_lossy(&bytes).to_string())
    }

    fn cache_control(resp: &Response) -> String {
        resp.headers()
            .get(header::CACHE_CONTROL)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("")
            .to_string()
    }

    #[test]
    fn cache_control_for_html_revalidates() {
        // Regression: without `no-cache` on index.html the browser hangs
        // on to a stale shell pointing at deleted bundle hashes after a
        // rebuild, so reloads look like "nothing changed". Memory entry
        // `project_ide_asset_cache_headers.md` covers the why.
        assert_eq!(
            cache_control_for("index.html"),
            "no-cache, must-revalidate"
        );
    }

    #[test]
    fn cache_control_for_hashed_bundles_is_immutable() {
        // Vite mangles hashes into the filename, so a given URL never
        // changes content. Long, immutable cache is correct here.
        assert_eq!(
            cache_control_for("assets/index-CLyk1qxs.js"),
            "public, max-age=31536000, immutable"
        );
        assert_eq!(
            cache_control_for("assets/index-deadbeef.css"),
            "public, max-age=31536000, immutable"
        );
    }

    #[test]
    fn cache_control_for_favicon_revalidates() {
        // Favicons are NOT hashed by Vite — must revalidate or they go
        // stale forever after a swap.
        assert_eq!(
            cache_control_for("favicon.svg"),
            "no-cache, must-revalidate"
        );
    }

    #[tokio::test]
    async fn serve_path_sets_no_cache_on_index_html() {
        let resp = serve_path("/");
        assert_eq!(cache_control(&resp), "no-cache, must-revalidate");
        let resp_named = serve_path("/index.html");
        assert_eq!(cache_control(&resp_named), "no-cache, must-revalidate");
    }

    #[tokio::test]
    async fn serve_path_sets_immutable_on_assets() {
        // Pick a JS bundle from the embedded dist. Build hash changes
        // every time the frontend recompiles, so look it up dynamically.
        let bundle = IdeAssets::iter()
            .find(|p| p.starts_with("assets/") && p.ends_with(".js"))
            .expect("dist should contain a hashed JS bundle");
        let resp = serve_path(&format!("/{bundle}"));
        assert_eq!(cache_control(&resp), "public, max-age=31536000, immutable");
    }

    #[tokio::test]
    async fn serve_path_root_returns_index_html_with_html_mime() {
        let resp = serve_path("/");
        let (status, content_type, body) = body_string(resp).await;
        assert_eq!(status, StatusCode::OK);
        assert!(content_type.starts_with("text/html"), "bad MIME: {content_type}");
        assert!(body.contains("id=\"root\""), "body missing React mount point: {body}");
    }

    #[tokio::test]
    async fn serve_path_unknown_path_falls_back_to_index_html() {
        // SPA fallback: any unknown route serves index.html so the client
        // router can take over.
        let resp = serve_path("/some/spa/route");
        let (status, content_type, body) = body_string(resp).await;
        assert_eq!(status, StatusCode::OK);
        assert!(content_type.starts_with("text/html"), "bad MIME: {content_type}");
        assert!(
            body.contains("id=\"root\""),
            "SPA fallback should serve index.html with React mount point: {body}"
        );
    }

    #[tokio::test]
    async fn serve_path_strips_leading_slash() {
        // Lookup must work whether the path comes in as "/index.html" or "index.html".
        let resp = serve_path("/index.html");
        let (status, _, body) = body_string(resp).await;
        assert_eq!(status, StatusCode::OK);
        assert!(body.contains("id=\"root\""));
    }

    #[test]
    fn browseable_url_swaps_unspecified_v4_for_loopback() {
        let url = browseable_url(
            IpAddr::V4(Ipv4Addr::UNSPECIFIED),
            SocketAddr::new(IpAddr::V4(Ipv4Addr::UNSPECIFIED), 2323),
        );
        assert_eq!(url, "http://127.0.0.1:2323");
    }

    #[test]
    fn browseable_url_swaps_unspecified_v6_for_loopback() {
        let url = browseable_url(
            IpAddr::V6(std::net::Ipv6Addr::UNSPECIFIED),
            SocketAddr::new(IpAddr::V6(std::net::Ipv6Addr::UNSPECIFIED), 2323),
        );
        assert_eq!(url, "http://[::1]:2323");
    }

    #[test]
    fn browseable_url_preserves_specific_v4_host() {
        let url = browseable_url(
            IpAddr::V4(Ipv4Addr::LOCALHOST),
            SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 2323),
        );
        assert_eq!(url, "http://127.0.0.1:2323");
    }

    #[test]
    fn browseable_url_brackets_specific_v6_host() {
        // For v6, SocketAddr's Display brackets the address, so the printed URL is browseable.
        let url = browseable_url(
            IpAddr::V6(std::net::Ipv6Addr::LOCALHOST),
            SocketAddr::new(IpAddr::V6(std::net::Ipv6Addr::LOCALHOST), 9000),
        );
        assert_eq!(url, "http://[::1]:9000");
    }
}
