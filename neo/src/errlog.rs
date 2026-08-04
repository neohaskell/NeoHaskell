//! Centralized, append-only log of *unrecognized* subprocess failures.
//!
//! When `neo` returns a `NeoError::SubprocessRaw` — meaning none of the
//! `interpret_*` patterns in `subprocess::interpret` matched the child output
//! — we append one JSON object per line to:
//!
//!     $NEO_HOME/unrecognized-errors.jsonl     (if `NEO_HOME` is set)
//!     $HOME/.neo/unrecognized-errors.jsonl    (fallback)
//!
//! The file is the user's local backlog: every unrecognized failure they hit
//! is preserved verbatim so they can later open issues (one per record, or
//! batched) without having to scroll back through terminal scrollback.
//!
//! Failures here are intentionally silent — logging must never break the
//! actual command. The returned `Option<PathBuf>` reports the file path on
//! success so callers can surface it in the error help text.
//!
//! ## JSONL record schema
//!
//! ```json
//! {"epoch_secs": <u64>, "neo_version": "<str>", "cwd": "<str>",
//!  "operation": "<str>", "tail": "<str>", "full_output": "<str>"}
//! ```
//!
//! Fields are populated best-effort: if `current_dir()` fails (e.g. the cwd
//! was removed), `cwd` is the empty string. `epoch_secs` is seconds since the
//! Unix epoch, UTC; we avoid a chrono/time dep so the binary stays slim and
//! the consumer (issue-generator) can format timestamps however it wants.

use serde::Serialize;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

const LOG_FILE_NAME: &str = "unrecognized-errors.jsonl";

#[derive(Debug, Serialize)]
struct Record<'a> {
    epoch_secs: u64,
    neo_version: &'a str,
    cwd: String,
    operation: &'a str,
    tail: &'a str,
    full_output: &'a str,
}

/// Resolve the directory that holds neo's per-user state.
///
/// Precedence: `$NEO_HOME` > `$HOME/.neo`. Returns `None` when neither is set
/// (rare — typically only in stripped-down CI containers).
pub fn log_dir() -> Option<PathBuf> {
    if let Ok(neo_home) = std::env::var("NEO_HOME") {
        if !neo_home.is_empty() {
            return Some(PathBuf::from(neo_home));
        }
    }
    let home = std::env::var("HOME").ok()?;
    if home.is_empty() {
        return None;
    }
    Some(PathBuf::from(home).join(".neo"))
}

/// Full path to the unrecognized-errors JSONL file (regardless of whether it
/// exists yet).
pub fn unrecognized_errors_path() -> Option<PathBuf> {
    log_dir().map(|d| d.join(LOG_FILE_NAME))
}

/// Append one unrecognized-error record. Resolves the destination via
/// [`unrecognized_errors_path`] and swallows all I/O errors (logging must
/// never break the user's command). Returns the file path on success so the
/// caller can surface it in the error help text.
pub fn log_unrecognized(operation: &str, tail: &str, full_output: &str) -> Option<PathBuf> {
    let path = unrecognized_errors_path()?;
    match write_record(&path, operation, tail, full_output) {
        Ok(()) => Some(path),
        Err(_) => None,
    }
}

/// Path-explicit append, exposed for tests so they don't have to mutate
/// process-global env vars.
pub fn write_record(
    path: &Path,
    operation: &str,
    tail: &str,
    full_output: &str,
) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let record = Record {
        epoch_secs: SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0),
        neo_version: env!("CARGO_PKG_VERSION"),
        cwd: std::env::current_dir()
            .map(|p| p.display().to_string())
            .unwrap_or_default(),
        operation,
        tail,
        full_output,
    };
    let line = serde_json::to_string(&record)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
    let mut f = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)?;
    f.write_all(line.as_bytes())?;
    f.write_all(b"\n")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn parse_lines(path: &Path) -> Vec<serde_json::Value> {
        std::fs::read_to_string(path)
            .unwrap()
            .lines()
            .filter(|l| !l.is_empty())
            .map(|l| serde_json::from_str::<serde_json::Value>(l).expect("each line is valid JSON"))
            .collect()
    }

    #[test]
    fn write_record_creates_parent_dir_and_appends_one_line() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("nested/sub/unrecognized-errors.jsonl");
        write_record(&path, "`cabal build all`", "Error: foo", "stdout\nstderr").unwrap();

        let lines = parse_lines(&path);
        assert_eq!(lines.len(), 1);
        let r = &lines[0];
        assert_eq!(r["operation"], "`cabal build all`");
        assert_eq!(r["tail"], "Error: foo");
        assert_eq!(r["full_output"], "stdout\nstderr");
        assert_eq!(r["neo_version"], env!("CARGO_PKG_VERSION"));
        assert!(r["epoch_secs"].as_u64().unwrap() > 0);
    }

    #[test]
    fn write_record_appends_does_not_overwrite() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("log.jsonl");
        write_record(&path, "op-1", "t-1", "f-1").unwrap();
        write_record(&path, "op-2", "t-2", "f-2").unwrap();
        write_record(&path, "op-3", "t-3", "f-3").unwrap();

        let lines = parse_lines(&path);
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0]["operation"], "op-1");
        assert_eq!(lines[1]["operation"], "op-2");
        assert_eq!(lines[2]["operation"], "op-3");
    }

    #[test]
    fn write_record_preserves_newlines_inside_full_output() {
        // JSONL must remain one-record-per-line — embedded `\n` in full_output
        // gets serialized as the escape sequence `\n` inside the JSON string,
        // not a literal newline that would split the record.
        let dir = tempdir().unwrap();
        let path = dir.path().join("log.jsonl");
        write_record(&path, "op", "tail", "line1\nline2\nline3").unwrap();

        let raw = std::fs::read_to_string(&path).unwrap();
        assert_eq!(raw.lines().count(), 1, "must be exactly one JSONL line, got:\n{}", raw);
        let lines = parse_lines(&path);
        assert_eq!(lines[0]["full_output"], "line1\nline2\nline3");
    }

    #[test]
    fn write_record_preserves_unicode_verbatim() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("log.jsonl");
        write_record(&path, "op 日本語", "tail αβγ", "full 🚀").unwrap();
        let lines = parse_lines(&path);
        assert_eq!(lines[0]["operation"], "op 日本語");
        assert_eq!(lines[0]["tail"], "tail αβγ");
        assert_eq!(lines[0]["full_output"], "full 🚀");
    }

    // ---------------- path resolution ----------------
    //
    // These tests mutate process-global env vars. They run on a single thread
    // (cargo test serializes them under #[serial_test] if used; here we just
    // accept the constraint that PATH-resolution tests share state and clean up).

    /// Take an exclusive lock for env-mutating tests in this module.
    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
        LOCK.lock().unwrap_or_else(|e| e.into_inner())
    }

    #[test]
    fn log_dir_prefers_neo_home_over_home() {
        let _guard = env_lock();
        let prev_neo_home = std::env::var("NEO_HOME").ok();
        let prev_home = std::env::var("HOME").ok();

        // SAFETY: env vars are process-global, but `env_lock()` ensures no
        // other env-mutating tests in this module run concurrently.
        unsafe {
            std::env::set_var("NEO_HOME", "/tmp/neo-home-override");
            std::env::set_var("HOME", "/tmp/some-user-home");
        }
        assert_eq!(log_dir(), Some(PathBuf::from("/tmp/neo-home-override")));
        assert_eq!(
            unrecognized_errors_path(),
            Some(PathBuf::from("/tmp/neo-home-override/unrecognized-errors.jsonl"))
        );

        unsafe { restore_env("NEO_HOME", prev_neo_home); restore_env("HOME", prev_home); }
    }

    #[test]
    fn log_dir_falls_back_to_home_dot_neo() {
        let _guard = env_lock();
        let prev_neo_home = std::env::var("NEO_HOME").ok();
        let prev_home = std::env::var("HOME").ok();

        unsafe {
            std::env::remove_var("NEO_HOME");
            std::env::set_var("HOME", "/tmp/user-home");
        }
        assert_eq!(log_dir(), Some(PathBuf::from("/tmp/user-home/.neo")));
        assert_eq!(
            unrecognized_errors_path(),
            Some(PathBuf::from("/tmp/user-home/.neo/unrecognized-errors.jsonl"))
        );

        unsafe { restore_env("NEO_HOME", prev_neo_home); restore_env("HOME", prev_home); }
    }

    #[test]
    fn log_dir_returns_none_when_neither_env_set() {
        let _guard = env_lock();
        let prev_neo_home = std::env::var("NEO_HOME").ok();
        let prev_home = std::env::var("HOME").ok();

        unsafe {
            std::env::remove_var("NEO_HOME");
            std::env::remove_var("HOME");
        }
        assert_eq!(log_dir(), None);

        unsafe { restore_env("NEO_HOME", prev_neo_home); restore_env("HOME", prev_home); }
    }

    #[test]
    fn log_dir_treats_empty_neo_home_as_unset() {
        let _guard = env_lock();
        let prev_neo_home = std::env::var("NEO_HOME").ok();
        let prev_home = std::env::var("HOME").ok();

        unsafe {
            std::env::set_var("NEO_HOME", "");
            std::env::set_var("HOME", "/tmp/h");
        }
        assert_eq!(log_dir(), Some(PathBuf::from("/tmp/h/.neo")));

        unsafe { restore_env("NEO_HOME", prev_neo_home); restore_env("HOME", prev_home); }
    }

    #[test]
    fn log_unrecognized_writes_under_neo_home_override() {
        let _guard = env_lock();
        let prev_neo_home = std::env::var("NEO_HOME").ok();
        let dir = tempdir().unwrap();

        unsafe {
            std::env::set_var("NEO_HOME", dir.path());
        }
        let path = log_unrecognized("op", "tail", "full").expect("should return path on success");
        assert_eq!(path, dir.path().join(LOG_FILE_NAME));
        assert!(path.exists());
        let lines = parse_lines(&path);
        assert_eq!(lines.len(), 1);

        unsafe { restore_env("NEO_HOME", prev_neo_home); }
    }

    unsafe fn restore_env(key: &str, prev: Option<String>) {
        match prev {
            Some(v) => unsafe { std::env::set_var(key, v) },
            None => unsafe { std::env::remove_var(key) },
        }
    }
}
