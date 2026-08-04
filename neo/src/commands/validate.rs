//! `neo validate` — lint `event-model.json` against the embedded JSON Schema
//! and referential-integrity rules, from the CLI, without launching `neo ide`.
//!
//! Read-only: this command NEVER modifies the file. It reuses the exact
//! validator engine the IDE uses (`crate::ide::validate`) via the shared
//! `read_and_validate` helper, so the CLI and the IDE can never disagree on
//! whether a model is valid / absent / invalid / malformed.
//!
//! Exit-code contract (see `build_report` / `status_code`):
//!   0 = valid · 1 = IO/tool failure · 2 = invalid · 3 = malformed JSON · 4 = absent.
//!
//! LOAD-BEARING: codes 2/3/4 are emitted with `std::process::exit` AFTER flushing
//! stdout+stderr, NOT by returning `Err`. `main()` maps every `miette` `Err` to
//! exit code 1, so a bad MODEL (2/3/4) must never travel the `Err` path — that
//! would erase the invalid-vs-malformed-vs-absent distinction and collide with a
//! genuine IO failure. `Err`/`NeoError::io_at` (→ exit 1) is reserved for real IO
//! failure (unreadable file, permission denied, path is a directory).

use std::io::Write;
use std::path::{Path, PathBuf};

use crate::errors::NeoError;
use crate::ide::methods::read_event_model::{read_and_validate, EVENT_MODEL_FILENAME};
use crate::ide::validate::ValidationOutcome;

/// The lines + exit code a validation run produces. Split out of `run` so the
/// `outcome → (output, code)` mapping is a PURE function unit tests can assert on
/// directly — the real `run` ends in `process::exit`, which would tear down a
/// test process, so exit codes themselves are only observable at the
/// integration/e2e layer.
#[derive(Debug, PartialEq, Eq)]
pub struct ValidateReport {
    pub stdout: Vec<String>,
    pub stderr: Vec<String>,
    pub code: i32,
}

/// Resolve the target file, validate it, print the report, and exit with the
/// contract's code. Returns `Err` (→ miette → exit 1) only on genuine IO failure.
pub fn run(path: Option<PathBuf>, json: bool) -> miette::Result<()> {
    let target = match path {
        Some(p) => p,
        None => {
            let cwd = std::env::current_dir().map_err(|e| {
                NeoError::io_at(
                    "resolving current working directory for `neo validate`",
                    PathBuf::from("."),
                    e,
                )
            })?;
            cwd.join(EVENT_MODEL_FILENAME)
        }
    };

    // A missing file is NOT an IO failure here — it maps to `NotFound` → exit 4.
    // Only genuine IO failure (permissions, path is a directory) becomes `Err` → exit 1.
    let (_content, outcome) = read_and_validate(&target)?;
    let report = build_report(&outcome, &target, json);

    {
        let mut out = std::io::stdout().lock();
        for line in &report.stdout {
            let _ = writeln!(out, "{line}");
        }
        let _ = out.flush();
    }
    {
        let mut err = std::io::stderr().lock();
        for line in &report.stderr {
            let _ = writeln!(err, "{line}");
        }
        let _ = err.flush();
    }

    // MUST be process::exit, never `Err` — see the module docs.
    std::process::exit(report.code)
}

/// Pure mapping from a `ValidationOutcome` to the lines to print and the exit code.
fn build_report(outcome: &ValidationOutcome, path: &Path, json: bool) -> ValidateReport {
    let p = path.display();

    if json {
        // Machine mode: stdout is EXACTLY the serialized outcome, stderr empty, so
        // `neo validate --json | jq` sees pure JSON and the exit code still mirrors
        // the status. No `[prefix]` lines leak in.
        let payload = serde_json::to_string_pretty(outcome).unwrap_or_else(|e| {
            format!("{{\"status\":\"serializationError\",\"error\":\"{e}\"}}")
        });
        return ValidateReport {
            stdout: vec![payload],
            stderr: Vec::new(),
            code: status_code(outcome),
        };
    }

    match outcome {
        ValidationOutcome::Valid => ValidateReport {
            stdout: vec![
                format!("[info] validating {p}"),
                "[ok] event-model.json is valid".to_string(),
            ],
            stderr: Vec::new(),
            code: 0,
        },
        ValidationOutcome::Invalid { errors } => {
            let mut stdout = vec![format!("[info] validating {p}")];
            for e in errors {
                // Messages are already written to be actionable by a tiny LLM — print
                // them verbatim, never reworded or truncated. Empty pointer = whole doc.
                let loc = if e.pointer.is_empty() {
                    "<root>"
                } else {
                    e.pointer.as_str()
                };
                stdout.push(format!("[error] {loc}: {}", e.message));
            }
            stdout.push(format!(
                "[fail] {} validation error(s) in {p} — fix the lines above, then re-run `neo validate`.",
                errors.len()
            ));
            ValidateReport {
                stdout,
                stderr: Vec::new(),
                code: 2,
            }
        }
        ValidationOutcome::MalformedJson { parse_error } => ValidateReport {
            stdout: vec![
                format!("[info] validating {p}"),
                // parse_error carries line/column — never truncate it.
                format!("[error] {p} is not valid JSON: {parse_error}"),
                format!(
                    "[fail] {p} did not parse — fix the syntax at the position above (a trailing comma, an unclosed brace or quote), or delete it and recreate the model with `neo ide`."
                ),
            ],
            stderr: Vec::new(),
            code: 3,
        },
        ValidationOutcome::NotFound => ValidateReport {
            stdout: vec![format!(
                "[fail] no event-model.json at {p} — an event model is required. Create one with `neo ide`, or pass the path to an existing model file."
            )],
            stderr: Vec::new(),
            code: 4,
        },
    }
}

/// The exit code for an outcome, independent of rendering (used by `--json`).
fn status_code(outcome: &ValidationOutcome) -> i32 {
    match outcome {
        ValidationOutcome::Valid => 0,
        ValidationOutcome::Invalid { .. } => 2,
        ValidationOutcome::MalformedJson { .. } => 3,
        ValidationOutcome::NotFound => 4,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::validate::{ErrorKind, ValidationError};

    fn path() -> PathBuf {
        PathBuf::from("/tmp/event-model.json")
    }

    fn verr(pointer: &str, message: &str, kind: ErrorKind) -> ValidationError {
        ValidationError {
            pointer: pointer.to_string(),
            message: message.to_string(),
            kind,
        }
    }

    #[test]
    fn report_valid_exits_0() {
        let r = build_report(&ValidationOutcome::Valid, &path(), false);
        assert_eq!(r.code, 0);
        assert!(r.stdout.iter().any(|l| l.starts_with("[ok]")), "{:?}", r.stdout);
        assert!(r.stderr.is_empty());
    }

    #[test]
    fn report_invalid_exits_2() {
        let outcome = ValidationOutcome::Invalid {
            errors: vec![
                verr("/nodes/0/type", "Node `n1`: unknown type", ErrorKind::Schema),
                verr(
                    "/edges/0/sourceId",
                    "Edge `e1`: `sourceId` references node `x` which is orphan",
                    ErrorKind::ReferentialIntegrity,
                ),
            ],
        };
        let r = build_report(&outcome, &path(), false);
        assert_eq!(r.code, 2);
        let errs: Vec<_> = r.stdout.iter().filter(|l| l.starts_with("[error]")).collect();
        assert_eq!(errs.len(), 2, "one line per error: {:?}", r.stdout);
        assert!(
            r.stdout.iter().any(|l| l.starts_with("[fail]") && l.contains('2')),
            "fail summary names the count: {:?}",
            r.stdout
        );
        // Verbatim message survives (the tiny-LLM-actionable text must not be reworded).
        assert!(r.stdout.iter().any(|l| l.contains("orphan")), "{:?}", r.stdout);
    }

    #[test]
    fn report_invalid_empty_pointer_renders_root() {
        let outcome = ValidationOutcome::Invalid {
            errors: vec![verr("", "whole-document error", ErrorKind::Schema)],
        };
        let r = build_report(&outcome, &path(), false);
        assert!(r.stdout.iter().any(|l| l.contains("<root>")), "{:?}", r.stdout);
    }

    #[test]
    fn report_malformed_exits_3() {
        let outcome = ValidationOutcome::MalformedJson {
            parse_error: "expected value at line 3 column 5".to_string(),
        };
        let r = build_report(&outcome, &path(), false);
        assert_eq!(r.code, 3);
        assert!(
            r.stdout.iter().any(|l| l.contains("line 3 column 5")),
            "parse_error (with position) must be printed untruncated: {:?}",
            r.stdout
        );
    }

    #[test]
    fn report_notfound_exits_4() {
        let r = build_report(&ValidationOutcome::NotFound, &path(), false);
        assert_eq!(r.code, 4);
        assert!(r.stdout.iter().any(|l| l.contains("event-model.json")), "{:?}", r.stdout);
        assert!(r.stdout.iter().any(|l| l.contains("neo ide")), "{:?}", r.stdout);
    }

    #[test]
    fn report_json_stdout_is_pure_outcome() {
        for outcome in [
            ValidationOutcome::Valid,
            ValidationOutcome::Invalid {
                errors: vec![verr("/x", "m", ErrorKind::Schema)],
            },
            ValidationOutcome::MalformedJson {
                parse_error: "boom".to_string(),
            },
            ValidationOutcome::NotFound,
        ] {
            let expected_code = status_code(&outcome);
            let r = build_report(&outcome, &path(), true);
            assert_eq!(r.code, expected_code, "json exit code mirrors status");
            assert!(r.stderr.is_empty(), "stderr must be empty in json mode");
            assert_eq!(r.stdout.len(), 1, "json mode emits exactly one stdout payload");
            let line = &r.stdout[0];
            assert!(
                !line.contains("[error]") && !line.contains("[fail]") && !line.contains("[info]"),
                "no human prefixes may leak into json output: {line}"
            );
            let v: serde_json::Value = serde_json::from_str(line).expect("json payload is pure JSON");
            assert!(v.get("status").is_some(), "payload carries a status: {line}");
        }
    }

    #[test]
    fn report_is_deterministic() {
        let outcome = ValidationOutcome::Invalid {
            errors: vec![
                verr("/a", "m1", ErrorKind::Schema),
                verr("/b", "m2", ErrorKind::ReferentialIntegrity),
            ],
        };
        assert_eq!(
            build_report(&outcome, &path(), false),
            build_report(&outcome, &path(), false)
        );
        assert_eq!(
            build_report(&outcome, &path(), true),
            build_report(&outcome, &path(), true)
        );
    }
}
