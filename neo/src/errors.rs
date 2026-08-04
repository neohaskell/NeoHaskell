use std::path::PathBuf;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum NeoError {
    #[error("No `neo.json` found in the current directory")]
    #[diagnostic(
        code(neo::no_workspace),
        url(docsrs),
        help("Run `neo new <name>` to create a project, or `cd` into an existing project directory that contains a `neo.json`.")
    )]
    NoWorkspace,

    #[error("Failed to parse `neo.json`: {reason}")]
    #[diagnostic(
        code(neo::invalid_config),
        url(docsrs),
        help("Expected JSON object syntax — `\"key\": value` pairs separated by commas, no trailing comma before `}}`. Open `neo.json` at the line and column underlined above, fix the syntax (e.g. remove a trailing comma, close a missing brace/quote, end an unterminated string), save, then re-run.")
    )]
    InvalidConfig {
        reason: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("syntax error here")]
        bad_bit: SourceSpan,
    },

    #[error("Directory `{name}` already exists in the current directory")]
    #[diagnostic(
        code(neo::dir_exists),
        url(docsrs),
        help("Pick a different project name (e.g. `neo new {name}-2`), or remove the existing directory first with `rm -rf {name}` and re-run `neo new {name}`.")
    )]
    DirectoryExists { name: String },

    #[error("Nix is required but was not found on PATH")]
    #[diagnostic(
        code(neo::nix_missing),
        url("https://nixos.org/download"),
        help("Install Nix from https://nixos.org/download (Determinate Systems installer recommended on macOS), then open a new shell and re-run.")
    )]
    NixNotFound,

    #[error("Git is required but was not found on PATH")]
    #[diagnostic(
        code(neo::git_missing),
        url("https://git-scm.com/downloads"),
        help("Install Git from https://git-scm.com/downloads (or via your OS package manager, e.g. `brew install git`), then open a new shell and re-run.")
    )]
    GitNotFound,

    #[error("Failed to fetch `{url}` over the network: {source}")]
    #[diagnostic(
        code(neo::network),
        url(docsrs),
        help("Check your internet connection (try `curl -I {url}`). If you intentionally want to skip network I/O (tests, offline dev), set `NEO_SKIP_NETWORK=1` — `neo` will use a local stub instead of downloading the starter template.")
    )]
    NetworkError {
        url: String,
        #[source]
        source: reqwest::Error,
    },

    #[error("I/O error while {operation} `{path}`: {source}")]
    #[diagnostic(
        code(neo::io_error),
        url(docsrs),
        help("Check that the path exists and that you have read/write permission. Run `ls -la {path}` to inspect, `df -h` to check disk space. If the parent directory does not exist, create it with `mkdir -p $(dirname {path})`.")
    )]
    IoErrorAt {
        operation: String,
        path: String,
        #[source]
        source: std::io::Error,
    },

    #[error("git {subcommand} failed: {reason}")]
    #[diagnostic(
        code(neo::git_error),
        url(docsrs),
        help("{fix}")
    )]
    GitError {
        subcommand: String,
        reason: String,
        fix: String,
    },

    #[error("Failed to render template `{template}`: {reason}")]
    #[diagnostic(
        code(neo::template_error),
        url(docsrs),
        help("This is an internal templating error in `neo`. Re-run with `RUST_BACKTRACE=1`. If it reproduces on a fresh `neo new` checkout, file a bug at https://github.com/NeoHaskell/neocli/issues with the full backtrace — the bundled `{template}` template should not fail to render with valid input.")
    )]
    TemplateError { template: String, reason: String },

    #[error("{operation} failed: {cause}")]
    #[diagnostic(
        code(neo::subprocess),
        url(docsrs),
        help("{fix}")
    )]
    SubprocessFailed {
        operation: String,
        cause: String,
        fix: String,
    },

    #[error("{operation} failed — `neo` could not extract an actionable cause from the child output.\n\nLast meaningful line from the child:\n  {tail}\n\n--- full child output (stdout + stderr, in capture order) ---\n{full_output}\n--- end of child output ---")]
    #[diagnostic(
        code(neo::subprocess_raw),
        url(docsrs),
        help("We didn't recognise this failure mode.\n\nThe full failure has been appended to your local log:\n  {log_path}\n\nThis file is the central backlog of every unrecognized error `neo` has hit on this machine — each line is one JSON record (operation, tail, full output, timestamp, cwd). When you have a moment, open an issue so we can add a fix recipe:\n  https://github.com/neohaskell/neo/issues/new?template=uninterpreted-subprocess-error.md\nYou can paste one record per issue, or attach the file and let us batch them.")
    )]
    SubprocessRaw {
        operation: String,
        tail: String,
        full_output: String,
        log_path: String,
    },

    #[error("Invalid dependency `{key}` = `{value}`: {reason}")]
    #[diagnostic(
        code(neo::invalid_dep),
        url(docsrs),
        help("Dependency values use npm-style semver (e.g. `^1.2.3`, `~2.0`, `*`). Use prefix `hackage:`, `git:`, `github:`, or `file:` for explicit sources. Example valid entries in `neo.json`: `\"text\": \"^2.0\"`, `\"hackage:aeson\": \"^2.1\"`, `\"github:owner/repo\": \"git:#main\"`, `\"file:../sibling\": \"file:../sibling\"`.")
    )]
    InvalidDependency {
        key: String,
        value: String,
        reason: String,
        #[source_code]
        src: Option<NamedSource<String>>,
        #[label("from this entry")]
        span: Option<SourceSpan>,
    },

    #[error("Build refused: {count} locked file(s) violate the lock")]
    #[diagnostic(
        code(neo::lock_violation),
        url(docsrs),
        help("{help_text}")
    )]
    LockViolation {
        count: usize,
        help_text: String,
    },

    #[error("Binding `neo ide` HTTP server to {host}:{port} failed: {source}")]
    #[diagnostic(
        code(neo::ide::bind),
        url(docsrs),
        help("Address {host}:{port} is unavailable. Common causes and concrete fixes:\n  \
              1. Port {port} is already in use. Find the PID and stop it:\n     \
                 - macOS: `lsof -iTCP:{port} -sTCP:LISTEN`\n     \
                 - Linux: `ss -ltnp 'sport = :{port}'`\n  \
              2. Pick a different free port. Re-run with `neo ide --port <FREE_PORT>` (any integer in 1024..=65535 that nothing else is listening on — e.g. `neo ide --port 2324`). Ports below 1024 require root; do not use them.\n  \
              3. The host {host} is not assigned to any interface on this machine. To bind every interface, re-run with `neo ide --host 0.0.0.0`. To bind loopback only, re-run with `neo ide --host 127.0.0.1`. Pass a literal IP, not a hostname.")
    )]
    IdeBind {
        host: std::net::IpAddr,
        port: u16,
        #[source]
        source: std::io::Error,
    },

    #[error("`neo ide` HTTP server crashed: {source}")]
    #[diagnostic(
        code(neo::ide::serve),
        url(docsrs),
        help("The embedded axum server returned an unexpected I/O error mid-flight. This is a bug in `neo`. \
              Re-run with `--verbose` to capture details, then file an issue at \
              https://github.com/NeoHaskell/neo/issues with the full output.")
    )]
    IdeServe {
        #[source]
        source: std::io::Error,
    },

    #[error("Healing `event-model.json` aborted: `claude` is not on PATH")]
    #[diagnostic(
        code(neo::ide::healing::claude_missing),
        url(docsrs),
        help("Install Claude Code: `npm install -g @anthropic-ai/claude-code` (requires Node 18+). After install, run `which claude` to confirm it is on PATH; if it still is not, open a new shell or check your PATH order. Healing shells out to the same `claude` CLI you use interactively — there is no fallback. Once `which claude` prints a path, click Heal again in the IDE.")
    )]
    HealingClaudeMissing,

    #[error("Healing `event-model.json` via `claude -p` failed: {reason}")]
    #[diagnostic(
        code(neo::ide::healing::failed),
        url(docsrs),
        help("`claude -p` did not complete successfully. Last lines of its stderr:\n\n{stderr_tail}\n\nCommon causes and concrete fixes:\n  1. API key missing or invalid — run `claude login` (or `export ANTHROPIC_API_KEY=...`) and click Heal again.\n  2. Rate-limited — wait ~60 seconds and click Heal again.\n  3. Network error — verify connectivity with `curl -I https://api.anthropic.com`, then retry.\n  4. Timed out — the model file may be too large or the prompt too complex. Open `event-model.json` and shrink it (delete obviously-broken nodes), then click Heal.\n  5. Crash inside `claude` — re-run `claude -p \"hello\"` from a terminal in this workspace to confirm the CLI itself works. If it crashes there too, file a bug with the Claude Code team.")
    )]
    HealingFailed {
        reason: String,
        stderr_tail: String,
    },
}

impl NeoError {
    pub fn io_at(operation: impl Into<String>, path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        NeoError::IoErrorAt {
            operation: operation.into(),
            path: path.into().display().to_string(),
            source,
        }
    }

    /// Build a `SubprocessRaw` and, as a side-effect, append the failure to
    /// the centralized `unrecognized-errors.jsonl` log under `$NEO_HOME` (or
    /// `$HOME/.neo` if unset). The returned variant carries the resolved log
    /// path so the help text can point the user at it. If logging fails (no
    /// HOME, disk full, perms), the field reads as a fallback hint rather
    /// than a real path — the error itself is still surfaced normally.
    /// Build a `LockViolation` from the list of locked-and-touched paths
    /// (modify, rename, or delete — all surface here via `parse_porcelain`).
    ///
    /// The help text is a railguard for coding-agent LLMs: it teaches the
    /// event-sourcing immutability rule and the only correct fix (write a
    /// new sibling file with a `V`-bumped suffix and leave the locked file
    /// byte-identical). It deliberately does NOT mention
    /// `neo lock --remove` or `neo build --skip-lock-check` — those exist
    /// for humans who already understand the model and find them in
    /// `--help`; advertising them in the railguard would defeat its
    /// purpose.
    pub fn lock_violation(paths: Vec<String>) -> Self {
        let count = paths.len();
        let paths_block = paths
            .iter()
            .map(|p| format!("  - `{}`", p))
            .collect::<Vec<_>>()
            .join("\n");
        let first = paths.first().map(String::as_str).unwrap_or("<path>");
        let (first_next_path, first_type, first_next_type) = derive_next_version(first);
        let restore_args = paths
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>()
            .join(" ");

        let intentional = if count == 1 {
            format!(
                "  2. If you do need new behavior: do NOT modify, rename, or delete the locked file. Create a new sibling file with a `V`-bumped suffix and write the new behavior from scratch there:\n       \
                 - Create `{first_next_path}`\n       \
                 - Define type `{first_next_type}` inside it with the new shape\n       \
                 - Leave `{first}` byte-identical to the locked version\n     \
                 Naming is exact: PascalCase type, matching filename, suffix is `V` immediately followed by an integer (no `_v2`, no `.V2`, no `Version2`). If the next number is taken (e.g. `V2` already exists), bump again to `V3`, `V4`, etc."
            )
        } else {
            format!(
                "  2. If you do need new behavior: do NOT modify, rename, or delete any of the locked files above. For each path, create a new sibling file with a `V`-bumped suffix and write the new behavior from scratch there. Naming is exact: PascalCase type, matching filename, suffix is `V` immediately followed by an integer (no `_v2`, no `.V2`, no `Version2`). If the next number is taken (e.g. `V2` already exists), bump again to `V3`, `V4`, etc.\n\n     \
                 Worked example for `{first}` (type `{first_type}`):\n       \
                 - Create `{first_next_path}`\n       \
                 - Define type `{first_next_type}` inside it with the new shape\n       \
                 - Leave `{first}` byte-identical to the locked version"
            )
        };

        let help_text = format!(
            "Pre-build lock check on `.locked-files` aborted. Locked files have been modified, renamed, or deleted (staged, unstaged, or untracked):\n\
             {paths_block}\n\n\
             Why this matters:\n  \
             NeoHaskell projects are event-sourced. Commands, events, projections, and deciders define the wire shapes that persisted event logs decode against. Every deployed instance replays its log on start. Editing, renaming, or deleting a locked file silently breaks replay on every deployed node — a class of bug that is invisible in tests and catastrophic in production. The lock exists to prevent that.\n\n\
             Do exactly one of these:\n\n  \
             1. If the edit was unintentional: discard with `git restore -- {restore_args}` so the path(s) are byte-identical to the locked version.\n\n\
             {intentional}"
        );
        NeoError::LockViolation { count, help_text }
    }

    pub fn subprocess_raw(
        operation: impl Into<String>,
        tail: impl Into<String>,
        full_output: impl Into<String>,
    ) -> Self {
        let operation = operation.into();
        let tail = tail.into();
        let full_output = full_output.into();
        let log_path = match crate::errlog::log_unrecognized(&operation, &tail, &full_output) {
            Some(p) => p.display().to_string(),
            None => "(could not write to `~/.neo/unrecognized-errors.jsonl` — set `NEO_HOME` to a writable directory if you want this persisted)".to_string(),
        };
        NeoError::SubprocessRaw { operation, tail, full_output, log_path }
    }
}

/// Derive `(next-version path, original type, next-version type)` from a
/// locked Haskell file path. Used to render a concrete worked example in
/// `LockViolation` help.
///
/// Examples:
///
///   src/Foo/Bar.hs     -> (src/Foo/BarV2.hs,  Bar,    BarV2)
///   src/Foo/BarV2.hs   -> (src/Foo/BarV3.hs,  BarV2,  BarV3)
///   src/Foo/BarV99.hs  -> (src/Foo/BarV100.hs, BarV99, BarV100)
///   Bar                -> (BarV2, Bar, BarV2)
///
/// Detects an existing `V<digits>` tail so the example never suggests
/// `BarV2V2` for an already-versioned locked file.
fn derive_next_version(path: &str) -> (String, String, String) {
    let (dir, file_name) = match path.rfind('/') {
        Some(i) => (&path[..=i], &path[i + 1..]),
        None => ("", path),
    };
    let (stem, ext) = match file_name.rfind('.') {
        Some(i) => (&file_name[..i], &file_name[i..]),
        None => (file_name, ""),
    };

    let bytes = stem.as_bytes();
    let mut digit_start = bytes.len();
    while digit_start > 0 && bytes[digit_start - 1].is_ascii_digit() {
        digit_start -= 1;
    }
    let next_stem = if digit_start < bytes.len()
        && digit_start > 0
        && bytes[digit_start - 1] == b'V'
    {
        let n: u64 = stem[digit_start..].parse().unwrap_or(1);
        format!("{}{}", &stem[..digit_start], n + 1)
    } else {
        format!("{stem}V2")
    };

    let next_path = format!("{dir}{next_stem}{ext}");
    (next_path, stem.to_string(), next_stem)
}

/// Find the byte-offset span of `"<key>"` inside a JSON-like source.
///
/// Returns the span covering the quoted key (including the surrounding `"`).
/// Returns `None` if the literal substring `"<key>"` does not occur.
///
/// Limitation: first match wins. If the same key name appears earlier in the
/// file inside a string value, that earlier occurrence is what gets pointed at.
/// Acceptable for `neo.json` where keys are typically distinctive.
pub fn key_span(content: &str, key: &str) -> Option<SourceSpan> {
    let needle = format!("\"{}\"", key);
    let offset = content.find(&needle)?;
    Some(SourceSpan::new(offset.into(), needle.len()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use miette::{GraphicalReportHandler, GraphicalTheme};

    fn render(diag: &dyn Diagnostic) -> String {
        let mut buf = String::new();
        GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor())
            .with_width(120)
            .with_links(false)
            .with_urls(false)
            .render_report(&mut buf, diag)
            .unwrap();
        buf
    }

    fn render_with_urls(diag: &dyn Diagnostic, links: bool, urls: bool) -> String {
        let mut buf = String::new();
        GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor())
            .with_width(120)
            .with_links(links)
            .with_urls(urls)
            .render_report(&mut buf, diag)
            .unwrap();
        buf
    }

    fn stub_invalid_config() -> NeoError {
        NeoError::InvalidConfig {
            reason: "unexpected comma".to_string(),
            src: NamedSource::new("neo.json", "x".to_string()),
            bad_bit: SourceSpan::new(0usize.into(), 1usize),
        }
    }

    fn stub_invalid_dep() -> NeoError {
        NeoError::InvalidDependency {
            key: "k".to_string(),
            value: "v".to_string(),
            reason: "r".to_string(),
            src: None,
            span: None,
        }
    }

    #[test]
    fn test_error_messages() {
        let err = NeoError::NoWorkspace;
        assert!(err.to_string().contains("No `neo.json` found"));

        let err = NeoError::DirectoryExists { name: "test".to_string() };
        assert!(err.to_string().contains("Directory `test` already exists"));

        let err = stub_invalid_config();
        assert!(err.to_string().contains("Failed to parse `neo.json`"));
        assert!(err.to_string().contains("unexpected comma"));

        let err = NeoError::NixNotFound;
        assert!(err.to_string().contains("Nix is required"));

        let err = NeoError::SubprocessFailed {
            operation: "cabal build".to_string(),
            cause: "package `foo` not found".to_string(),
            fix: "edit `neo.json` and add a source prefix".to_string(),
        };
        let rendered = err.to_string();
        assert!(rendered.contains("cabal build failed"));
        assert!(rendered.contains("package `foo` not found"));

        let err = NeoError::SubprocessRaw {
            operation: "nix develop".to_string(),
            tail: "(no output)".to_string(),
            full_output: "(no output)".to_string(),
            log_path: "/tmp/test/unrecognized-errors.jsonl".to_string(),
        };
        let rendered = err.to_string();
        assert!(rendered.contains("nix develop failed"), "rendered: {}", rendered);
        assert!(rendered.contains("could not extract an actionable cause"), "rendered: {}", rendered);

        let err = NeoError::GitError {
            subcommand: "ls-remote".to_string(),
            reason: "repository not found".to_string(),
            fix: "check the URL".to_string(),
        };
        assert!(err.to_string().contains("git ls-remote failed"));
        assert!(err.to_string().contains("repository not found"));
    }

    #[test]
    fn invalid_config_help_no_longer_repeats_line_col() {
        // The line/col now live in the snippet block; the help text should explain
        // how to fix without re-stating coordinates already shown above.
        let err = stub_invalid_config();
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("underlined above"), "help: {}", help);
        assert!(help.contains("re-run"), "help: {}", help);
    }

    #[test]
    fn test_network_error_mentions_offline_env_var() {
        let err = NeoError::NetworkError {
            url: "https://example.invalid".to_string(),
            source: reqwest::Client::new()
                .get("not a url")
                .build()
                .unwrap_err(),
        };
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("NEO_SKIP_NETWORK=1"), "help missing env var: {}", help);
    }

    #[test]
    fn test_io_error_carries_path() {
        let err = NeoError::io_at(
            "writing `neo.json`".to_string(),
            std::path::PathBuf::from("/tmp/x/neo.json"),
            std::io::Error::new(std::io::ErrorKind::PermissionDenied, "no perm"),
        );
        let display = err.to_string();
        assert!(display.contains("writing `neo.json`"), "missing op: {}", display);
        assert!(display.contains("/tmp/x/neo.json"), "missing path: {}", display);
        assert!(display.contains("no perm"), "missing source: {}", display);

        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("ls -la"), "help missing ls -la: {}", help);
    }

    #[test]
    fn test_subprocess_failed_carries_fix() {
        let err = NeoError::SubprocessFailed {
            operation: "cabal build".to_string(),
            cause: "X".to_string(),
            fix: "Y".to_string(),
        };
        let display = err.to_string();
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(display.contains("cabal build failed: X"), "bad display: {}", display);
        assert_eq!(help, "Y", "bad help: {}", help);
    }

    // ---------------- SubprocessRaw: full output + GH issue link ----------------

    fn stub_subprocess_raw() -> NeoError {
        NeoError::SubprocessRaw {
            operation: "`cabal build all`".to_string(),
            tail: "Error: [Cabal-7125]".to_string(),
            full_output: "alpha\nbravo\ncharlie αβγ".to_string(),
            log_path: "/tmp/test-home/.neo/unrecognized-errors.jsonl".to_string(),
        }
    }

    #[test]
    fn subprocess_raw_renders_full_output_inline() {
        let err = stub_subprocess_raw();
        let rendered = err.to_string();
        assert!(rendered.contains("alpha"), "missing alpha: {}", rendered);
        assert!(rendered.contains("bravo"), "missing bravo: {}", rendered);
        assert!(rendered.contains("charlie αβγ"), "missing unicode line: {}", rendered);
        assert!(rendered.contains("--- full child output"), "missing opening fence: {}", rendered);
        assert!(rendered.contains("--- end of child output ---"), "missing closing fence: {}", rendered);
    }

    #[test]
    fn subprocess_raw_renders_no_output_placeholder() {
        let err = NeoError::SubprocessRaw {
            operation: "`cabal build all`".to_string(),
            tail: "(no output)".to_string(),
            full_output: "(no output)".to_string(),
            log_path: "/tmp/log.jsonl".to_string(),
        };
        let rendered = err.to_string();
        // Placeholder appears between the fences, not just in the tail.
        let opening = rendered.find("--- full child output").expect("opening fence");
        let closing = rendered.find("--- end of child output ---").expect("closing fence");
        let between = &rendered[opening..closing];
        assert!(between.contains("(no output)"), "placeholder not between fences: {}", between);
    }

    #[test]
    fn subprocess_raw_help_mentions_log_path_field() {
        // Help text must surface the JSONL log path the constructor writes to,
        // so the user knows where to find the persisted record for filing an issue.
        let err = NeoError::SubprocessRaw {
            operation: "op".to_string(),
            tail: "t".to_string(),
            full_output: "f".to_string(),
            log_path: "/home/u/.neo/unrecognized-errors.jsonl".to_string(),
        };
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(
            help.contains("/home/u/.neo/unrecognized-errors.jsonl"),
            "help missing log path: {}",
            help
        );
        assert!(help.contains("appended"), "help should say the failure was appended: {}", help);
    }

    #[test]
    fn subprocess_raw_constructor_writes_jsonl_under_neo_home() {
        // Integration between `NeoError::subprocess_raw` and `errlog`: the
        // constructor must call into the logger so every call site gets
        // persistence for free.
        let dir = tempfile::tempdir().unwrap();
        let prev = std::env::var("NEO_HOME").ok();
        // SAFETY: cargo test runs this in the same process; the cost is that
        // parallel tests touching NEO_HOME may race. We accept that — the
        // intent is to prove the constructor invokes the logger.
        unsafe { std::env::set_var("NEO_HOME", dir.path()); }

        let err = NeoError::subprocess_raw("op-x", "tail-x", "full-x\nmulti-line");
        let log_file = dir.path().join("unrecognized-errors.jsonl");
        assert!(log_file.exists(), "constructor should have written the log file");

        let content = std::fs::read_to_string(&log_file).unwrap();
        assert!(content.contains("op-x"), "log missing operation: {}", content);
        assert!(content.contains("tail-x"), "log missing tail: {}", content);
        // Verify the variant carries the resolved path (not the fallback message)
        match &err {
            NeoError::SubprocessRaw { log_path, .. } => {
                assert_eq!(log_path, &log_file.display().to_string());
            }
            _ => panic!("expected SubprocessRaw"),
        }

        unsafe {
            match prev {
                Some(v) => std::env::set_var("NEO_HOME", v),
                None => std::env::remove_var("NEO_HOME"),
            }
        }
    }

    #[test]
    fn subprocess_raw_help_points_at_gh_template() {
        let err = stub_subprocess_raw();
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(
            help.contains("github.com/neohaskell/neo/issues/new"),
            "help missing GH issue URL: {}",
            help
        );
        assert!(
            help.contains("template=uninterpreted-subprocess-error.md"),
            "help missing template query param: {}",
            help
        );
    }

    #[test]
    fn subprocess_raw_help_does_not_mention_interpret_rs() {
        // Regression: the old help told end users to edit `src/subprocess/interpret.rs`.
        // That guidance belongs in the GH issue template (for contributors), not in
        // the error a user installing the released binary sees.
        let err = stub_subprocess_raw();
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(!help.contains("interpret.rs"), "help still mentions interpret.rs: {}", help);
        assert!(!help.contains("interpret_cabal"), "help still mentions interpret_cabal: {}", help);
    }

    #[test]
    fn subprocess_raw_help_does_not_say_scroll_up() {
        // Regression: full output is now inline, so "scroll up" is misleading.
        let err = stub_subprocess_raw();
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(
            !help.to_lowercase().contains("scroll up"),
            "help still says 'scroll up': {}",
            help
        );
    }

    #[test]
    fn subprocess_raw_tail_still_renders() {
        let err = stub_subprocess_raw();
        let rendered = err.to_string();
        assert!(
            rendered.contains("Last meaningful line from the child:"),
            "missing tail headline: {}",
            rendered
        );
        assert!(rendered.contains("Error: [Cabal-7125]"), "missing tail content: {}", rendered);
    }

    // ---------------- Tier 2a: url() on every variant ----------------

    fn all_variants() -> Vec<NeoError> {
        vec![
            NeoError::NoWorkspace,
            stub_invalid_config(),
            NeoError::DirectoryExists { name: "x".to_string() },
            NeoError::NixNotFound,
            NeoError::GitNotFound,
            NeoError::NetworkError {
                url: "u".to_string(),
                source: reqwest::Client::new().get("not a url").build().unwrap_err(),
            },
            NeoError::IoErrorAt {
                operation: "o".to_string(),
                path: "p".to_string(),
                source: std::io::Error::other("io"),
            },
            NeoError::GitError {
                subcommand: "g".to_string(),
                reason: "r".to_string(),
                fix: "f".to_string(),
            },
            NeoError::TemplateError { template: "t".to_string(), reason: "r".to_string() },
            NeoError::SubprocessFailed { operation: "o".to_string(), cause: "c".to_string(), fix: "f".to_string() },
            NeoError::SubprocessRaw { operation: "o".to_string(), tail: "t".to_string(), full_output: "stdout line\nstderr line".to_string(), log_path: "/tmp/log.jsonl".to_string() },
            stub_invalid_dep(),
            NeoError::lock_violation(vec!["src/Commands/Foo.hs".to_string()]),
            NeoError::IdeBind {
                host: std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST),
                port: 2323,
                source: std::io::Error::new(std::io::ErrorKind::AddrInUse, "address in use"),
            },
            NeoError::IdeServe {
                source: std::io::Error::other("server crashed"),
            },
            NeoError::HealingClaudeMissing,
            NeoError::HealingFailed {
                reason: "exit code 1".to_string(),
                stderr_tail: "panic at the disco".to_string(),
            },
        ]
    }

    // ---------------- LockViolation: help acts as an LLM railguard ----------------
    //
    // The help text must (a) teach the event-sourcing immutability rule,
    // (b) point at the V-bump recipe with a worked example, and (c) refuse
    // to advertise the escape hatches (`neo lock --remove`, `--skip-lock-check`).
    // A coding-agent LLM reading the error must conclude "create a new
    // sibling file with a V-bumped suffix", not "unlock and edit".

    #[test]
    fn lock_violation_help_explains_event_sourcing_immutability() {
        let err = NeoError::lock_violation(vec!["src/Commands/User.hs".to_string()]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("event-sourced"), "missing event-sourcing explainer: {}", help);
        assert!(help.contains("replay"), "missing replay rationale: {}", help);
    }

    #[test]
    fn lock_violation_help_includes_versioned_worked_example() {
        let err = NeoError::lock_violation(vec![
            "src/Starter/Counter/Commands/IncrementCounter.hs".to_string(),
        ]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        // Path bumped to V2 in the same directory.
        assert!(
            help.contains("src/Starter/Counter/Commands/IncrementCounterV2.hs"),
            "missing V2 path in worked example: {}",
            help,
        );
        // Type name bumped to V2.
        assert!(help.contains("IncrementCounterV2"), "missing V2 type: {}", help);
        // Original file must be called out as untouched.
        assert!(
            help.contains("byte-identical"),
            "missing 'byte-identical' instruction: {}",
            help,
        );
        // Naming-convention guardrail (anti-hallucination on suffix shape).
        assert!(help.contains("`_v2`"), "missing forbidden-naming list: {}", help);
    }

    #[test]
    fn lock_violation_help_drops_unlock_and_skip_flag() {
        // The forbidden phrases — these are the railguard's reason to exist.
        let err = NeoError::lock_violation(vec![
            "src/Commands/A.hs".to_string(),
            "src/Events/B.hs".to_string(),
        ]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(!help.contains("neo lock --remove"), "unlock recipe leaked: {}", help);
        assert!(!help.contains("--skip-lock-check"), "skip flag leaked: {}", help);
        assert!(!help.contains("git checkout --"), "old revert recipe leaked: {}", help);
        // And no soft signals to look for the escape hatches.
        assert!(
            !help.to_lowercase().contains("not recommended"),
            "should not editorialize about an escape hatch we don't mention: {}",
            help,
        );
    }

    #[test]
    fn lock_violation_help_uses_git_restore_for_unintentional_branch() {
        let err = NeoError::lock_violation(vec!["src/Commands/A.hs".to_string()]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(
            help.contains("git restore -- src/Commands/A.hs"),
            "missing concrete git restore recipe: {}",
            help,
        );
    }

    #[test]
    fn lock_violation_help_quotes_every_offending_path() {
        let err = NeoError::lock_violation(vec![
            "src/Commands/A.hs".to_string(),
            "src/Events/B.hs".to_string(),
        ]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("`src/Commands/A.hs`"), "missing path A in help: {}", help);
        assert!(help.contains("`src/Events/B.hs`"), "missing path B in help: {}", help);
    }

    #[test]
    fn lock_violation_headline_carries_count() {
        let err = NeoError::lock_violation(vec![
            "a.hs".to_string(),
            "b.hs".to_string(),
            "c.hs".to_string(),
        ]);
        let rendered = err.to_string();
        assert!(rendered.contains("Build refused"), "missing op: {}", rendered);
        assert!(rendered.contains("3 locked file(s)"), "missing count: {}", rendered);
    }

    #[test]
    fn lock_violation_help_names_pre_build_check() {
        // The "operation" component of the rustc-style error contract.
        let err = NeoError::lock_violation(vec!["x.hs".to_string()]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("Pre-build lock check"), "missing op label: {}", help);
        assert!(help.contains(".locked-files"), "missing manifest name: {}", help);
    }

    #[test]
    fn lock_violation_help_single_file_has_no_for_each_abstraction() {
        // N=1: the worked example IS the recipe — no "for each path" wrapper.
        let err = NeoError::lock_violation(vec!["src/Commands/A.hs".to_string()]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(
            !help.to_lowercase().contains("for each path"),
            "single-file help should not abstract over a list: {}",
            help,
        );
    }

    #[test]
    fn lock_violation_help_multi_file_shares_explainer_and_one_example() {
        // N>1: shared "for each path" recipe, ONE worked example anchored to
        // the first path (not repeated per file — token economy +
        // hallucination reduction).
        let err = NeoError::lock_violation(vec![
            "src/Commands/Alpha.hs".to_string(),
            "src/Events/Beta.hs".to_string(),
            "src/Queries/Gamma.hs".to_string(),
        ]);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("For each path"), "missing shared recipe phrase: {}", help);
        // Worked example must use the first path (Alpha), not Beta or Gamma.
        assert!(
            help.contains("Worked example for `src/Commands/Alpha.hs`"),
            "worked example missing or not on first path: {}",
            help,
        );
        assert!(help.contains("src/Commands/AlphaV2.hs"), "missing AlphaV2 path: {}", help);
        // Beta and Gamma must NOT have their own worked-example blocks.
        assert!(
            !help.contains("Worked example for `src/Events/Beta.hs`"),
            "per-file example leaked for Beta: {}",
            help,
        );
        assert!(
            !help.contains("Worked example for `src/Queries/Gamma.hs`"),
            "per-file example leaked for Gamma: {}",
            help,
        );
        // git restore line lists ALL paths inline so the LLM can copy-paste.
        assert!(
            help.contains("git restore -- src/Commands/Alpha.hs src/Events/Beta.hs src/Queries/Gamma.hs"),
            "git restore should enumerate all paths: {}",
            help,
        );
    }

    #[test]
    fn lock_violation_empty_paths_does_not_panic() {
        let err = NeoError::lock_violation(vec![]);
        let _ = err.to_string();
        let _ = err.help().map(|h| h.to_string());
    }

    // ---------------- derive_next_version helper ----------------

    #[test]
    fn derive_next_version_appends_v2_to_plain_haskell_file() {
        let (next_path, ty, next_ty) = derive_next_version("src/Foo/Bar.hs");
        assert_eq!(next_path, "src/Foo/BarV2.hs");
        assert_eq!(ty, "Bar");
        assert_eq!(next_ty, "BarV2");
    }

    #[test]
    fn derive_next_version_bumps_existing_v_suffix() {
        // Already-versioned locked file (the project evolved through V2):
        // the example must point at V3, not BarV2V2.
        let (next_path, ty, next_ty) = derive_next_version("src/Foo/BarV2.hs");
        assert_eq!(next_path, "src/Foo/BarV3.hs");
        assert_eq!(ty, "BarV2");
        assert_eq!(next_ty, "BarV3");
    }

    #[test]
    fn derive_next_version_bumps_large_v_suffix() {
        let (next_path, _, next_ty) = derive_next_version("src/Foo/BarV99.hs");
        assert_eq!(next_path, "src/Foo/BarV100.hs");
        assert_eq!(next_ty, "BarV100");
    }

    #[test]
    fn derive_next_version_treats_lowercase_v_as_no_version_suffix() {
        // `v2` is NOT the project convention — treat as plain stem and
        // append V2, yielding `Barv2V2`. This is correct: the user/LLM
        // should adopt the canonical PascalCase `V` form.
        let (next_path, _, next_ty) = derive_next_version("src/Foo/Barv2.hs");
        assert_eq!(next_path, "src/Foo/Barv2V2.hs");
        assert_eq!(next_ty, "Barv2V2");
    }

    #[test]
    fn derive_next_version_handles_file_with_no_directory() {
        let (next_path, ty, next_ty) = derive_next_version("Bar.hs");
        assert_eq!(next_path, "BarV2.hs");
        assert_eq!(ty, "Bar");
        assert_eq!(next_ty, "BarV2");
    }

    #[test]
    fn derive_next_version_handles_file_with_no_extension() {
        let (next_path, ty, next_ty) = derive_next_version("Bar");
        assert_eq!(next_path, "BarV2");
        assert_eq!(ty, "Bar");
        assert_eq!(next_ty, "BarV2");
    }

    #[test]
    fn every_variant_has_url() {
        for v in all_variants() {
            let url = v.url().map(|u| u.to_string());
            assert!(
                url.is_some() && !url.as_deref().unwrap().is_empty(),
                "variant {:?} has no url()",
                v
            );
        }
    }

    #[test]
    fn url_renders_as_link_when_links_on() {
        let rendered = render_with_urls(&NeoError::NoWorkspace, true, true);
        assert!(rendered.contains("\x1b]8;"), "expected OSC-8 sentinel in: {:?}", rendered);
    }

    #[test]
    fn url_omitted_when_with_urls_false() {
        let rendered = render_with_urls(&NeoError::NoWorkspace, false, false);
        assert!(!rendered.contains("\x1b]8;"), "OSC-8 should be absent: {:?}", rendered);
    }

    // ---------------- Tier 2b: InvalidConfig source span ----------------

    fn invalid_config_with_content(content: &str, line: usize, col: usize, reason: &str) -> NeoError {
        let offset = miette::SourceOffset::from_location(content, line, col);
        NeoError::InvalidConfig {
            reason: reason.to_string(),
            src: NamedSource::new("neo.json", content.to_string()),
            bad_bit: SourceSpan::new(offset, 1usize),
        }
    }

    #[test]
    fn invalid_config_renders_with_caret_block() {
        // Trailing comma on line 3 col 18.
        let content = "{\n  \"name\": \"x\",\n  \"author\": \"y\",,\n}";
        let err = invalid_config_with_content(content, 3, 18, "trailing comma");
        let rendered = render(&err);
        assert!(rendered.contains("Failed to parse `neo.json`"), "headline missing: {}", rendered);
        assert!(rendered.contains("trailing comma"), "reason missing: {}", rendered);
        assert!(rendered.contains("syntax error here"), "label missing: {}", rendered);
        // Snippet block opens with either unicode `╭` or ASCII `,-` depending on theme.
        // We picked unicode_nocolor in `render()`, so this should be unicode.
        assert!(
            rendered.contains("╭") || rendered.contains(",-"),
            "snippet block open missing: {}",
            rendered
        );
    }

    #[test]
    fn invalid_config_renders_deterministically() {
        let err = stub_invalid_config();
        assert_eq!(render(&err), render(&err));
    }

    #[test]
    fn invalid_config_with_unicode_content_does_not_panic() {
        let content = "{\n  \"name\": \"日本語\",,\n}";
        let err = invalid_config_with_content(content, 2, 22, "trailing comma after unicode");
        let _ = render(&err); // must not panic
    }

    #[test]
    fn invalid_config_at_eof_does_not_panic() {
        let content = "{ \"name\": ";
        let err = invalid_config_with_content(content, 1, 11, "unexpected end of input");
        let _ = render(&err);
    }

    #[test]
    fn invalid_config_empty_content_does_not_panic() {
        let err = invalid_config_with_content("", 1, 1, "empty input");
        let _ = render(&err);
    }

    // ---------------- Tier 2c: key_span helper ----------------

    #[test]
    fn key_span_finds_quoted_key() {
        let content = "{\"foo\": 1, \"bar\": 2}";
        let span = key_span(content, "bar").expect("should find bar");
        assert_eq!(span.offset(), 11);
        assert_eq!(span.len(), 5); // "bar" with surrounding quotes = 5 bytes
    }

    #[test]
    fn key_span_returns_none_when_missing() {
        assert!(key_span("{\"x\": 1}", "nope").is_none());
    }

    #[test]
    fn key_span_first_match_wins() {
        // The key `a` appears twice — once as a key, once as a value. We get the first.
        let content = "{\"a\":\"a\"}";
        let span = key_span(content, "a").expect("should find a");
        assert_eq!(span.offset(), 1, "should point at the first occurrence");
    }

    #[test]
    fn key_span_handles_unicode_key() {
        let content = "{\"日本語\": 1}";
        let span = key_span(content, "日本語").expect("should find unicode key");
        // 1 byte for `{`, then 1 byte for opening `"`, span starts there
        assert_eq!(span.offset(), 1);
    }

    // ---------------- Tier 2c: InvalidDependency rendering ----------------

    #[test]
    fn invalid_dep_renders_with_span_when_source_attached() {
        let content = "{\n  \"dependencies\": {\n    \"foo\": \"^9.9.9\"\n  }\n}";
        let err = NeoError::InvalidDependency {
            key: "foo".to_string(),
            value: "^9.9.9".to_string(),
            reason: "package `foo` not found in the NeoPackages registry".to_string(),
            src: Some(NamedSource::new("neo.json", content.to_string())),
            span: key_span(content, "foo"),
        };
        let rendered = render(&err);
        assert!(rendered.contains("Invalid dependency `foo`"), "headline: {}", rendered);
        assert!(rendered.contains("from this entry"), "label: {}", rendered);
        assert!(rendered.contains("neo.json"), "filename: {}", rendered);
    }

    // ---------------- IdeBind / IdeServe: actionable help ----------------

    #[test]
    fn ide_bind_help_names_addr_and_concrete_fix() {
        let err = NeoError::IdeBind {
            host: std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST),
            port: 2323,
            source: std::io::Error::new(std::io::ErrorKind::AddrInUse, "address in use"),
        };
        let display = err.to_string();
        // Headline must name the exact host:port the user asked for, not a hardcoded one.
        assert!(display.contains("127.0.0.1:2323"), "missing op + addr: {}", display);
        assert!(display.contains("address in use"), "missing source cause: {}", display);

        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        // Must mention both the host and port the user actually passed.
        assert!(help.contains("2323"), "help missing port: {}", help);
        assert!(help.contains("127.0.0.1"), "help missing host: {}", help);
        // Must give a concrete fix recipe — the `--port` flag form.
        assert!(help.contains("neo ide --port"), "help missing --port recipe: {}", help);
        // Must mention `--host` so users who hit an unassigned-interface error see the fix.
        assert!(help.contains("--host"), "help missing --host recipe: {}", help);
        // Must give a probe command for finding what's bound.
        assert!(help.contains("lsof"), "help missing lsof probe: {}", help);
    }

    #[test]
    fn ide_bind_help_reflects_user_supplied_host() {
        // When the user passes `--host 0.0.0.0` and bind fails, the headline + help
        // must echo `0.0.0.0`, not silently report `127.0.0.1`.
        let err = NeoError::IdeBind {
            host: std::net::IpAddr::V4(std::net::Ipv4Addr::UNSPECIFIED),
            port: 9000,
            source: std::io::Error::new(std::io::ErrorKind::AddrInUse, "address in use"),
        };
        let display = err.to_string();
        assert!(display.contains("0.0.0.0:9000"), "headline must echo host:port: {}", display);
        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("0.0.0.0"), "help must echo user-supplied host: {}", help);
    }

    #[test]
    fn ide_serve_help_points_at_bug_tracker() {
        let err = NeoError::IdeServe {
            source: std::io::Error::other("unexpected EOF"),
        };
        let display = err.to_string();
        assert!(display.contains("HTTP server crashed"), "missing op label: {}", display);
        assert!(display.contains("unexpected EOF"), "missing source cause: {}", display);

        let help = err.help().map(|h| h.to_string()).unwrap_or_default();
        assert!(help.contains("github.com/NeoHaskell/neo/issues"), "help missing issue link: {}", help);
    }

    #[test]
    fn invalid_dep_renders_without_span_when_no_source() {
        let err = NeoError::InvalidDependency {
            key: "foo".to_string(),
            value: "^9.9.9".to_string(),
            reason: "r".to_string(),
            src: None,
            span: None,
        };
        let rendered = render(&err);
        assert!(rendered.contains("Invalid dependency `foo`"), "headline: {}", rendered);
        // No snippet block (no source attached).
        assert!(!rendered.contains("from this entry"), "label should not appear: {}", rendered);
    }
}
