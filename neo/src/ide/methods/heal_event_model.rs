//! `workspace/healEventModel` — invoke `claude -p` to repair a malformed
//! `event-model.json` in-place.
//!
//! Contract:
//!   1. Read `<workspace_root>/event-model.json` and re-validate.
//!   2. If already valid, return `Healed` (no-op).
//!   3. Otherwise spawn `claude -p` with the validation errors + schema
//!      inlined into the prompt and the workspace scoped via `--add-dir`.
//!   4. After the subprocess exits, re-read and re-validate.
//!   5. Return `Healed` if now valid, `StillInvalid { errors }` if not.
//!
//! Errors (surface as JSON-RPC `RpcError`, not as part of the success outcome):
//!   - `NeoError::HealingClaudeMissing` — `claude` not on PATH
//!   - `NeoError::HealingFailed` — subprocess exited non-zero, hit timeout,
//!     or otherwise could not run to completion
//!   - `NeoError::IoErrorAt` — file disappeared or became unreadable

use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, AsyncRead, BufReader};
use tokio::process::Command;

use crate::errors::NeoError;
use crate::ide::heal::apply::apply_diff;
use crate::ide::heal::diff::{compute_diff, HealDiff};
use crate::ide::methods::read_event_model::EVENT_MODEL_FILENAME;
use crate::ide::session::Session;
use crate::ide::validate::{self, ErrorKind, ValidationError, ValidationOutcome, SCHEMA_JSON};

#[derive(Debug, Deserialize, Default, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct HealEventModelParams {
    /// How aggressively to invoke the agent.
    ///
    /// - `Validate` (default, used by the auto-triggered modal): only spawn
    ///   `claude` if the file actually fails validation. If it's already
    ///   valid, return `Healed` immediately as a no-op.
    /// - `Improve` (used by the manual "Heal with AI" button): always spawn
    ///   `claude` regardless of validation state. Lets the user ask the
    ///   agent to fix layout / add inferred edges on a passing file.
    #[serde(default)]
    pub mode: HealMode,
}

#[derive(Debug, Deserialize, Default, Clone, Copy, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum HealMode {
    #[default]
    Validate,
    Improve,
}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "camelCase", rename_all_fields = "camelCase", tag = "status")]
pub enum HealOutcome {
    Healed,
    StillInvalid { errors: Vec<ValidationError> },
    /// User clicked Cancel on the heal overlay while the LLM was running.
    /// The subprocess was killed; the deterministic pre-pass's patches
    /// (if any) were still written to disk so the user keeps the free
    /// wins. `deterministicApplied` is the count from that pre-pass.
    Cancelled { deterministic_applied: usize },
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HealEventModelResult {
    pub outcome: HealOutcome,
}

/// Knobs that production callers don't touch but tests override. Production
/// goes through `handle(...)` which uses `HealConfig::default()` — `claude`
/// on PATH, 15-minute timeout. The budget covers (a) sonnet/opus thinking
/// at ~5k–20k tokens streamed at human-readable rate, (b) several Read
/// tool calls against the workspace, (c) the final StructuredOutput emit,
/// AND (d) any 529 backoff cycles (up to 10 retries with growing delays —
/// 30s+ on a bad Anthropic capacity day). 5 minutes was the original v1
/// number and turned out to clip real heals against medium-sized models.
#[derive(Debug, Clone)]
pub struct HealConfig {
    /// Path to the `claude` binary. Default: `"claude"` (resolved via PATH).
    pub claude_binary: PathBuf,
    /// Hard timeout for the subprocess.
    pub timeout: Duration,
}

impl Default for HealConfig {
    fn default() -> Self {
        Self {
            claude_binary: PathBuf::from("claude"),
            timeout: Duration::from_secs(900),
        }
    }
}

pub async fn handle(
    session: Session,
    params: HealEventModelParams,
) -> Result<HealEventModelResult, NeoError> {
    handle_with_config(session, params.mode, HealConfig::default()).await
}

pub(crate) async fn handle_with_config(
    session: Session,
    mode: HealMode,
    config: HealConfig,
) -> Result<HealEventModelResult, NeoError> {
    let path = session.workspace.root.join(EVENT_MODEL_FILENAME);
    tracing::info!(path = %path.display(), ?mode, "heal: starting");

    let original_content = std::fs::read_to_string(&path).map_err(|e| {
        NeoError::io_at(
            "reading `event-model.json` to start healing",
            path.clone(),
            e,
        )
    })?;
    let workspace_root = session.workspace.root.clone();

    // ───── Phase 1: deterministic pre-pass ─────────────────────────────
    //
    // The vast majority of heal repairs are mechanical: missing edges
    // implied by `command.produces`, `query.subscribes_to`, etc. We do
    // those in Rust against a parsed `ProjectInspection` and never burn
    // an LLM round-trip on them. Only fuzzy residuals (missing nodes,
    // typos, orphans) trickle through to claude.
    let inspection_t0 = Instant::now();
    let inspection = crate::inspect::inspect_project(&workspace_root);
    let inspection_ms = inspection_t0.elapsed().as_millis();

    let mut patched_value: Option<serde_json::Value> = serde_json::from_str(&original_content).ok();
    let mut patched_content = original_content.clone();
    let mut deterministic_diff: Option<HealDiff> = None;
    let mut deterministic_applied = 0usize;

    // Run the deterministic pass UNCONDITIONALLY when the JSON parses.
    // When the inspection is empty (non-NeoHaskell workspace), compute_diff
    // skips its node-materialisation + orphan-detection sections but still
    // produces position fixes and layout entries — that's the "just clean
    // up positions on a hand-authored file" path.
    if let Some(value) = patched_value.as_mut() {
        let diff_t0 = Instant::now();
        let diff = compute_diff(value, &inspection);
        let applied = if diff.applied_count() > 0 {
            apply_diff(value, &diff)
        } else {
            0
        };
        let diff_ms = diff_t0.elapsed().as_millis();
        tracing::info!(
            domains = inspection.domains.len(),
            inspection_ms = inspection_ms,
            diff_ms = diff_ms,
            summary = %diff.summary(),
            applied = applied,
            "heal: deterministic pre-pass complete",
        );
        if applied > 0 {
            patched_content = serde_json::to_string_pretty(value).map_err(|e| {
                NeoError::HealingFailed {
                    reason: format!(
                        "could not re-serialise model after deterministic patch: {e}"
                    ),
                    stderr_tail: String::new(),
                }
            })?;
        }
        deterministic_applied = applied;
        deterministic_diff = Some(diff);
    }

    // Surface what the deterministic pass did to the frontend overlay so
    // the user sees the free wins before any LLM cost.
    if let Some(ref diff) = deterministic_diff {
        if deterministic_applied > 0 || !diff.residuals.is_empty() {
            session.notify(
                "$/progress",
                serde_json::json!({
                    "token": "healEventModel",
                    "value": {
                        "kind": "autoRepair",
                        "appliedCount": deterministic_applied,
                        "residualCount": diff.residuals.len(),
                        "summary": diff.summary(),
                    }
                }),
            );
        }
    }

    // Re-validate AFTER patching.
    let validation_after = validate::validate_event_model(&patched_content);

    // ───── Phase 2: decide whether the LLM is needed ───────────────────
    //
    // Two short-circuit paths skip claude entirely:
    //   * Validate mode + file now valid + no residuals → done.
    //   * Improve mode + file now valid + no residuals → done.
    //   (Validate mode + invalid + residuals → still spawn, because
    //    the diff couldn't fix everything.)
    let residual_count = deterministic_diff
        .as_ref()
        .map(|d| d.residuals.len())
        .unwrap_or(0);

    let needs_llm = match (&validation_after, residual_count, mode) {
        (ValidationOutcome::Valid, 0, _) => false,
        (ValidationOutcome::Valid, _, HealMode::Validate) => {
            // Validate mode is non-invasive: if the file passes validation
            // we don't ask the LLM to add inferred nodes even if residuals
            // exist. The user has to flip to Improve to opt in.
            false
        }
        _ => true,
    };

    if !needs_llm {
        if patched_content != original_content {
            atomic_write(&path, &patched_content)?;
            tracing::info!(
                applied = deterministic_applied,
                "heal: deterministic pass alone fixed the file — no LLM round-trip needed",
            );
        } else {
            tracing::info!(
                "heal: file already valid AND inspection matches — no-op",
            );
        }
        session.notify(
            "$/progress",
            serde_json::json!({
                "token": "healEventModel",
                "value": { "kind": "end" }
            }),
        );
        return Ok(HealEventModelResult {
            outcome: HealOutcome::Healed,
        });
    }

    // ───── Phase 3: shrink the prompt and spawn claude ────────────────
    let initial_errors = match validation_after {
        ValidationOutcome::Valid => Vec::new(),
        ValidationOutcome::Invalid { errors } => errors,
        ValidationOutcome::MalformedJson { parse_error } => {
            vec![ValidationError {
                pointer: String::new(),
                message: format!(
                    "file is not valid JSON: {parse_error}. The whole document must be parseable JSON before any other rule applies."
                ),
                kind: ErrorKind::Schema,
            }]
        }
        ValidationOutcome::NotFound => {
            return Err(NeoError::io_at(
                "reading `event-model.json` to start healing",
                path,
                std::io::Error::from(std::io::ErrorKind::NotFound),
            ));
        }
    };

    // Pick a model that fits the residual load. Tiny residuals → haiku
    // (faster + cheaper). Bigger residuals on a NeoHaskell project →
    // sonnet. No inspection at all → opus (open-ended audit).
    let project_summary = crate::commands::inspect::project_summary_for_prompt(&workspace_root);
    let model_arg = pick_model(residual_count, initial_errors.len(), project_summary.is_some());
    tracing::info!(
        has_neo_summary = project_summary.is_some(),
        residual_count = residual_count,
        validation_errors = initial_errors.len(),
        model = model_arg,
        "heal: composing prompt for LLM pass",
    );

    let prompt = build_prompt(
        &path,
        &workspace_root,
        project_summary.as_deref(),
        deterministic_diff.as_ref(),
        deterministic_applied,
        mode,
        &patched_content,
        &initial_errors,
    );

    // Structured-output wrapper. The agent's final message must be a
    // JSON object whose `eventModel` field is the healed model. We
    // validate that field ourselves with the embedded schema and write
    // it to disk — the agent never touches the filesystem, so we can
    // drop Edit/Write entirely.
    let output_schema = serde_json::json!({
        "type": "object",
        "required": ["eventModel"],
        "additionalProperties": false,
        "properties": {
            "eventModel": { "type": "object" },
            "summary": { "type": "string" },
            "changesMade": {
                "type": "array",
                "items": { "type": "string" }
            }
        }
    });
    let output_schema_str = output_schema.to_string();

    // Flag list. `--max-turns` is NOT a valid claude flag (caused immediate
    // exit-1 in earlier iterations); use `--verbose` to make claude chatty
    // on stderr so the streaming log shows progress.
    let args: Vec<String> = vec![
        "-p".to_string(),
        "--add-dir".to_string(),
        workspace_root.display().to_string(),
        // Read only — no Edit/Write/Bash. The agent should NOT need any
        // tools (we inject everything in the prompt) but Read remains as
        // an escape hatch for verifying NeoHaskell .hs files in edge
        // cases the inspector summary didn't cover.
        "--allowed-tools".to_string(),
        "Read".to_string(),
        "--json-schema".to_string(),
        output_schema_str,
        "--model".to_string(),
        model_arg.to_string(),
        "--verbose".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--include-partial-messages".to_string(),
        prompt.clone(),
    ];
    let args_for_log: Vec<String> = args
        .iter()
        .take(args.len() - 1)
        .cloned()
        .chain(std::iter::once(format!("<prompt {} bytes>", prompt.len())))
        .collect();
    tracing::info!(
        binary = %config.claude_binary.display(),
        cwd = %workspace_root.display(),
        timeout_secs = config.timeout.as_secs(),
        prompt_bytes = prompt.len(),
        args = ?args_for_log,
        "heal: spawning claude -p",
    );

    let spawn_result = Command::new(&config.claude_binary)
        .args(&args)
        .current_dir(&workspace_root)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .spawn();

    let mut child = match spawn_result {
        Ok(c) => c,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            tracing::error!(
                binary = %config.claude_binary.display(),
                "heal: claude binary not found on PATH",
            );
            return Err(NeoError::HealingClaudeMissing);
        }
        Err(e) => {
            tracing::error!(error = %e, "heal: failed to spawn claude");
            return Err(NeoError::HealingFailed {
                reason: format!(
                    "failed to spawn `{}`: {e}",
                    config.claude_binary.display()
                ),
                stderr_tail: String::new(),
            });
        }
    };

    tracing::info!(pid = ?child.id(), "heal: claude subprocess spawned, streaming output");

    // Tell the client that heal started — frontend uses this to switch
    // its spinner overlay into the "with streaming log" mode.
    session.notify(
        "$/progress",
        serde_json::json!({
            "token": "healEventModel",
            "value": { "kind": "begin", "title": "Healing event model" }
        }),
    );

    // Take the piped handles BEFORE waiting so we can stream them line-by-
    // line. Without this, the user sees no progress during the (potentially
    // multi-minute) heal — the whole point of the logging exercise.
    let stdout = child
        .stdout
        .take()
        .expect("stdout was piped at spawn");
    let stderr = child
        .stderr
        .take()
        .expect("stderr was piped at spawn");

    let stdout_buf: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let stderr_buf: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));

    let stdout_task = {
        let buf = Arc::clone(&stdout_buf);
        let session = session.clone();
        tokio::spawn(stream_lines(stdout, "stdout", buf, session))
    };
    let stderr_task = {
        let buf = Arc::clone(&stderr_buf);
        let session = session.clone();
        tokio::spawn(stream_lines(stderr, "stderr", buf, session))
    };

    // Install the cancellation token in the session AFTER the subprocess
    // is up. `cancelHealEventModel` reads `session.heal_cancel` and calls
    // `notify_one()` — we race that signal against `child.wait()` and the
    // timeout below. The guard clears the session slot on drop so the
    // next heal doesn't see a stale notify.
    let (cancel_notify, _cancel_guard) = session.install_heal_cancel();

    let start = Instant::now();
    let timeout_fut = tokio::time::sleep(config.timeout);
    let cancel_fut = cancel_notify.notified();
    tokio::pin!(timeout_fut, cancel_fut);

    enum RaceOutcome {
        Exited(std::io::Result<std::process::ExitStatus>),
        TimedOut,
        Cancelled,
    }

    let race = tokio::select! {
        res = child.wait() => RaceOutcome::Exited(res),
        _ = &mut timeout_fut => RaceOutcome::TimedOut,
        _ = &mut cancel_fut => RaceOutcome::Cancelled,
    };
    let elapsed = start.elapsed();

    let status = match race {
        RaceOutcome::Exited(Ok(s)) => s,
        RaceOutcome::Exited(Err(e)) => {
            tracing::error!(error = %e, "heal: wait error on claude subprocess");
            // Best-effort drain so we don't leak the tasks.
            let _ = stdout_task.await;
            let _ = stderr_task.await;
            return Err(NeoError::HealingFailed {
                reason: format!("waiting for `claude -p` to exit: {e}"),
                stderr_tail: String::new(),
            });
        }
        RaceOutcome::Cancelled => {
            tracing::info!("heal: user cancelled; killing claude subprocess");
            let _ = child.kill().await;
            let _ = child.wait().await;
            let _ = stdout_task.await;
            let _ = stderr_task.await;

            // Persist the deterministic pre-pass's patches so the user
            // keeps the free wins on cancel. If nothing was patched, the
            // file on disk is untouched.
            if patched_content != original_content {
                atomic_write(&path, &patched_content)?;
            }
            session.notify(
                "$/progress",
                serde_json::json!({
                    "token": "healEventModel",
                    "value": { "kind": "end" }
                }),
            );
            return Ok(HealEventModelResult {
                outcome: HealOutcome::Cancelled { deterministic_applied },
            });
        }
        RaceOutcome::TimedOut => {
            tracing::warn!(
                timeout_secs = config.timeout.as_secs(),
                "heal: claude timed out, killing",
            );
            let _ = child.kill().await;
            let _ = child.wait().await;
            let _ = stdout_task.await;
            let _ = stderr_task.await;
            let stdout_dump = collect_all(&stdout_buf);
            let stderr_dump = collect_all(&stderr_buf);
            tracing::error!(
                timeout_secs = config.timeout.as_secs(),
                "heal: claude killed after timeout — output captured before kill:\n\
                 --- captured stdout ---\n\
                 {stdout_block}\n\
                 --- captured stderr ---\n\
                 {stderr_block}\n\
                 --- end claude output ---",
                stdout_block = if stdout_dump.is_empty() {
                    "(empty)"
                } else {
                    stdout_dump.as_str()
                },
                stderr_block = if stderr_dump.is_empty() {
                    "(empty)"
                } else {
                    stderr_dump.as_str()
                },
            );
            let tail = collect_tail(&stderr_buf, 20);
            let retry_count = count_api_retries(&stdout_buf);
            let retry_hint = if retry_count > 0 {
                format!(
                    " (saw {retry_count} Anthropic API retry events during this run — \
                     the API was returning HTTP 429/529; some of the budget was spent \
                     on backoff. Try again, or check https://status.anthropic.com)"
                )
            } else {
                String::new()
            };
            return Err(NeoError::HealingFailed {
                reason: format!(
                    "timed out after {} seconds{retry_hint}",
                    config.timeout.as_secs()
                ),
                stderr_tail: tail,
            });
        }
    };

    // Drain streaming tasks (process has exited; tasks reach EOF shortly).
    let _ = stdout_task.await;
    let _ = stderr_task.await;

    let stdout_line_count = stdout_buf.lock().map(|g| g.len()).unwrap_or(0);
    let stderr_line_count = stderr_buf.lock().map(|g| g.len()).unwrap_or(0);
    tracing::info!(
        elapsed_secs = elapsed.as_secs(),
        exit_code = ?status.code(),
        stdout_lines = stdout_line_count,
        stderr_lines = stderr_line_count,
        "heal: claude exited",
    );

    if !status.success() {
        let stdout_dump = collect_all(&stdout_buf);
        let stderr_dump = collect_all(&stderr_buf);
        let code = status
            .code()
            .map(|c| c.to_string())
            .unwrap_or_else(|| "killed by signal".to_string());
        // Print the full captured output as ONE multi-line error so the user
        // doesn't have to scroll through interleaved per-line streams. Empty
        // sections are called out explicitly.
        tracing::error!(
            exit_code = %code,
            "heal: claude failed\n\
             --- captured stdout ({stdout_lines} lines) ---\n\
             {stdout_block}\n\
             --- captured stderr ({stderr_lines} lines) ---\n\
             {stderr_block}\n\
             --- end claude output ---",
            stdout_lines = stdout_line_count,
            stderr_lines = stderr_line_count,
            stdout_block = if stdout_dump.is_empty() {
                "(empty — claude wrote nothing to stdout)"
            } else {
                stdout_dump.as_str()
            },
            stderr_block = if stderr_dump.is_empty() {
                "(empty — claude wrote nothing to stderr)"
            } else {
                stderr_dump.as_str()
            },
        );
        let tail = collect_tail(&stderr_buf, 20);
        return Err(NeoError::HealingFailed {
            reason: format!("exit code {code}"),
            stderr_tail: if tail.is_empty() {
                "(stderr empty)".to_string()
            } else {
                tail
            },
        });
    }

    // The agent's final assistant message (via `--json-schema`) is the
    // healed model wrapped in a small envelope. We scan stdout for the
    // stream-json `result` event, parse its `result` field, and write
    // the embedded `eventModel` ourselves. This makes the agent purely
    // a transformation function — no filesystem mutation.
    let stdout_lines = stdout_buf.lock().map(|g| g.clone()).unwrap_or_default();
    let healed_payload = match extract_structured_output(&stdout_lines) {
        Ok(p) => p,
        Err(reason) => {
            tracing::error!(
                error = %reason,
                "heal: could not extract structured output from claude",
            );
            return Err(NeoError::HealingFailed {
                reason: format!("agent returned no usable structured output: {reason}"),
                stderr_tail: collect_tail(&stderr_buf, 20),
            });
        }
    };
    tracing::info!(
        summary_chars = healed_payload.summary.as_ref().map(|s| s.len()).unwrap_or(0),
        change_count = healed_payload.changes_made.as_ref().map(|v| v.len()).unwrap_or(0),
        "heal: agent returned structured output",
    );
    if let Some(ref summary) = healed_payload.summary {
        tracing::info!(summary = %summary, "heal: agent summary");
    }
    for change in healed_payload.changes_made.as_deref().unwrap_or(&[]) {
        tracing::info!(change = %change, "heal: agent change");
    }

    let new_content =
        serde_json::to_string_pretty(&healed_payload.event_model).map_err(|e| {
            NeoError::HealingFailed {
                reason: format!("could not serialise agent's eventModel to JSON: {e}"),
                stderr_tail: String::new(),
            }
        })?;

    let outcome = match validate::validate_event_model(&new_content) {
        ValidationOutcome::Valid => {
            atomic_write(&path, &new_content)?;
            tracing::info!("heal: file is now valid — healed and saved");
            HealOutcome::Healed
        }
        ValidationOutcome::Invalid { errors } => {
            tracing::warn!(
                remaining_errors = errors.len(),
                "heal: agent's output still has validation errors — file NOT written",
            );
            HealOutcome::StillInvalid { errors }
        }
        ValidationOutcome::MalformedJson { parse_error } => {
            tracing::warn!(
                parse_error = %parse_error,
                "heal: agent's output isn't valid JSON — file NOT written",
            );
            HealOutcome::StillInvalid {
                errors: vec![ValidationError {
                    pointer: String::new(),
                    message: format!(
                        "the agent returned a JSON object whose `eventModel` field isn't valid JSON: {parse_error}. The file on disk is untouched."
                    ),
                    kind: ErrorKind::Schema,
                }],
            }
        }
        ValidationOutcome::NotFound => unreachable!("validate never returns NotFound for in-memory content"),
    };

    session.notify(
        "$/progress",
        serde_json::json!({
            "token": "healEventModel",
            "value": { "kind": "end" }
        }),
    );

    Ok(HealEventModelResult { outcome })
}

/// Read `reader` line-by-line. Emit each line as a `tracing::info!` event
/// keyed on `stream` ("stdout" / "stderr") under the
/// `neo::ide::heal::claude` target so users can `tail -f` (or simply watch
/// `neo ide`'s stderr) and see claude's progress in real time. Each line
/// is also appended to `buf` so the caller can still build a stderr-tail
/// for the failure-path error message.
async fn stream_lines<R>(
    reader: R,
    stream: &'static str,
    buf: Arc<Mutex<Vec<String>>>,
    session: Session,
) where
    R: AsyncRead + Unpin,
{
    let mut lines = BufReader::new(reader).lines();
    loop {
        match lines.next_line().await {
            Ok(Some(line)) => {
                tracing::info!(target: "neo::ide::heal::claude", stream, "{line}");
                // Push to the in-memory buffer (for failure-path tail
                // capture) AND notify the WS client so the frontend
                // overlay can render the line as it arrives.
                if let Ok(mut guard) = buf.lock() {
                    guard.push(line.clone());
                }
                session.notify(
                    "$/progress",
                    serde_json::json!({
                        "token": "healEventModel",
                        "value": {
                            "kind": "log",
                            "stream": stream,
                            "line": line,
                        }
                    }),
                );
            }
            Ok(None) => break,
            Err(e) => {
                tracing::warn!(
                    target: "neo::ide::heal::claude",
                    stream,
                    "read error: {e}",
                );
                break;
            }
        }
    }
}

/// Decoded structured-output envelope from the agent's final assistant
/// message. The actual `event-model.json` content is `event_model`; the
/// other fields are for surfacing in the heal log + future UI.
struct HealedPayload {
    event_model: serde_json::Value,
    summary: Option<String>,
    changes_made: Option<Vec<String>>,
}

/// Walk claude's streamed stdout lines looking for the `result` event
/// (`{"type":"result","subtype":"success", ...}`). Under `--json-schema`,
/// claude-code emits the schema-validated payload on a top-level
/// `structured_output` field of that event — `result.result` continues
/// to hold the human-readable closing assistant message, not the JSON.
///
/// We prefer `structured_output`. If absent (e.g. a future call without
/// `--json-schema`, or older claude-code), we fall back to parsing
/// `result.result` as a JSON string. If neither yields a usable payload,
/// we return a reason naming both attempted paths.
fn extract_structured_output(stdout_lines: &[String]) -> Result<HealedPayload, String> {
    let mut last_result: Option<serde_json::Value> = None;
    for line in stdout_lines {
        let trimmed = line.trim_start();
        if !trimmed.starts_with('{') {
            continue;
        }
        let Ok(value) = serde_json::from_str::<serde_json::Value>(trimmed) else {
            continue;
        };
        if value.get("type").and_then(|v| v.as_str()) == Some("result") {
            last_result = Some(value);
        }
    }
    let result = last_result.ok_or_else(|| {
        "no `{type:\"result\"}` event found in claude stdout".to_string()
    })?;

    let subtype = result
        .get("subtype")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    if subtype != "success" {
        let reason = result
            .get("result")
            .and_then(|v| v.as_str())
            .unwrap_or("(no detail)")
            .chars()
            .take(400)
            .collect::<String>();
        return Err(format!("claude reported result.subtype={subtype}: {reason}"));
    }

    // Prefer the top-level `structured_output` field: under `--json-schema`,
    // claude-code emits the schema-validated payload there directly as an
    // object (not as a stringified JSON). `result.result` holds the
    // human-readable closing message in that case, not the JSON.
    let payload: serde_json::Value = if let Some(so) = result.get("structured_output") {
        if so.is_object() {
            so.clone()
        } else if let Some(s) = so.as_str() {
            serde_json::from_str(s).map_err(|e| {
                format!(
                    "agent's `structured_output` field is a string but not valid JSON: {e}"
                )
            })?
        } else {
            return Err(format!(
                "agent's `structured_output` field is neither an object nor a string: {so}"
            ));
        }
    } else {
        // Fallback: parse `result.result` as a JSON string. This is the
        // shape claude-code produces without `--json-schema`.
        let payload_str = result
            .get("result")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                "result event has neither a top-level `structured_output` field \
                 nor a `result` string to fall back on"
                    .to_string()
            })?;
        serde_json::from_str(payload_str).map_err(|e| {
            format!(
                "agent's `result.result` is not valid JSON (no `structured_output` \
                 field to fall back on either): {e}"
            )
        })?
    };

    let event_model = payload
        .get("eventModel")
        .cloned()
        .ok_or_else(|| {
            "agent's structured output is missing the required `eventModel` field".to_string()
        })?;

    let summary = payload
        .get("summary")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let changes_made = payload
        .get("changesMade")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect::<Vec<_>>()
        });

    Ok(HealedPayload {
        event_model,
        summary,
        changes_made,
    })
}

fn collect_tail(buf: &Arc<Mutex<Vec<String>>>, n: usize) -> String {
    let Ok(guard) = buf.lock() else {
        return String::new();
    };
    let start = guard.len().saturating_sub(n);
    guard[start..].join("\n")
}

fn collect_all(buf: &Arc<Mutex<Vec<String>>>) -> String {
    let Ok(guard) = buf.lock() else {
        return String::new();
    };
    guard.join("\n")
}

/// Count `{"type":"system","subtype":"api_retry",...}` events in a captured
/// stdout buffer. Used in the timeout error to tell the user how much of
/// the budget was eaten by Anthropic capacity backoffs.
fn count_api_retries(buf: &Arc<Mutex<Vec<String>>>) -> usize {
    let Ok(guard) = buf.lock() else {
        return 0;
    };
    guard
        .iter()
        .filter(|line| {
            // Cheap substring check first; fall back to JSON only on a hit.
            line.contains("\"api_retry\"")
                && serde_json::from_str::<serde_json::Value>(line.trim_start())
                    .ok()
                    .as_ref()
                    .and_then(|v| v.get("subtype"))
                    .and_then(|v| v.as_str())
                    == Some("api_retry")
        })
        .count()
}

/// Build the LLM-pass prompt. The deterministic phase already handled
/// the mechanical edge/kind/position work; this prompt only asks claude
/// to resolve the residuals (missing nodes, orphans, schema violations
/// the diff couldn't fix). Mode controls how aggressive the LLM should
/// be about adding new nodes.
fn build_prompt(
    path: &Path,
    workspace_root: &Path,
    project_summary: Option<&str>,
    deterministic_diff: Option<&HealDiff>,
    deterministic_applied: usize,
    mode: HealMode,
    current_file_content: &str,
    errors: &[ValidationError],
) -> String {
    let validation_block = if errors.is_empty() {
        "  (no schema or referential errors remain — the file currently validates.)".to_string()
    } else {
        errors
            .iter()
            .map(|e| {
                let pointer = if e.pointer.is_empty() {
                    "(whole document)"
                } else {
                    e.pointer.as_str()
                };
                format!("  - {pointer}: {}", e.message)
            })
            .collect::<Vec<_>>()
            .join("\n")
    };

    // What did the deterministic pass already do? Showing this stops the
    // LLM from second-guessing edges/kinds/positions that are already correct.
    let auto_block = match deterministic_diff {
        Some(diff) if deterministic_applied > 0 => format!(
            "A deterministic Rust pre-pass already applied {applied} mechanical \
             fixes against this file (compared the JSON to the NeoHaskell code's \
             commands/events/queries/integrations). Specifically: {summary}. \
             These are DONE — the model below already reflects them. Do not re-do \
             this work; focus only on the residuals below.",
            applied = deterministic_applied,
            summary = diff.summary(),
        ),
        Some(_) | None => "No deterministic pre-pass changes were applied.".to_string(),
    };

    // The residuals are what's LEFT for the LLM — the fuzzy stuff the
    // Rust pass intentionally punted on.
    let residual_block = match deterministic_diff {
        Some(diff) if !diff.residuals.is_empty() => {
            let mut buf = String::from(
                "The deterministic pre-pass identified residual issues it could \
                 not fix without judgment. Resolve each one in your output:\n",
            );
            for r in &diff.residuals {
                buf.push_str("  - ");
                buf.push_str(&residual_to_human(r));
                buf.push('\n');
            }
            buf
        }
        _ => "  (no residuals — the deterministic pass found no fuzzy issues.)".to_string(),
    };

    // ONE policy per mode. The Rust deterministic pre-pass now materialises
    // every code-side symbol (commands / events / queries / integrations)
    // and wires their edges; nothing in the residual list ever asks the LLM
    // to "add a missing node". Whatever's left is fuzzy: orphaned model
    // nodes that the code doesn't back, residual schema breakage that the
    // diff couldn't fix, or in Improve mode polish work (chapter grouping,
    // better slice/entity naming).
    let mode_directive = match mode {
        HealMode::Validate => {
            "MODE = VALIDATE. Your job is the minimum repair needed for the file \
             to satisfy the schema. Fix the listed validation errors. The \
             deterministic pre-pass already created every node, slice, entity, \
             and edge implied by the NeoHaskell code — do NOT add more. Touch \
             only what's necessary to make validation pass and to resolve \
             orphan-node residuals (rename a clear typo, otherwise leave alone)."
        }
        HealMode::Improve => {
            "MODE = IMPROVE. The deterministic pre-pass already materialised \
             every command / event / query / integration the NeoHaskell code \
             declares and wired their edges. Your job is polish, not \
             construction: rename auto-generated slice / entity names (e.g. \
             from raw dir names like \"Orders\" to a cleaner form, or from a \
             command name to a verb-phrase like \"Place Order\"), group slices \
             into chapters when there's a clear narrative, and resolve orphan \
             residuals. Do NOT add new commands / events / queries / \
             integrations — if you think one is needed, leave a comment in \
             `changesMade` instead."
        }
    };

    let neo_summary_block = match project_summary {
        Some(summary) => format!(
            "Pre-computed NeoHaskell domain summary (authoritative — driven by `neo inspect`):\n\n```json\n{summary}\n```\n",
            summary = summary,
        ),
        None => String::from(
            "(`neo inspect` found no NeoHaskell domains — work from the JSON below.)\n",
        ),
    };

    format!(
        "You are repairing residual issues in a NeoHaskell event-model JSON file.\n\
\n\
File: {file_path}\n\
Workspace: {workspace}\n\
\n\
{mode_directive}\n\
\n\
== What the deterministic pre-pass already did ==\n\
{auto_block}\n\
\n\
== Residuals (your job to resolve) ==\n\
{residual_block}\n\
\n\
== Remaining schema/referential validation errors ==\n\
{validation_block}\n\
\n\
== Reference: NeoHaskell domain summary ==\n\
{neo_summary}\n\
\n\
== Current event-model.json (already post-deterministic-patch) ==\n\
\n\
```json\n\
{current_file}\n\
```\n\
\n\
== Schema (file MUST validate against this exactly) ==\n\
{schema}\n\
\n\
== Output format (REQUIRED) ==\n\
\n\
Return the FULL corrected event-model in the `eventModel` field of the structured output. Preserve every existing id (chapters, entities, slices, nodes, edges) wherever you don't have a hard reason to change it. Use `edge-<short-random>` for any new edge ids. Do not wrap the output in markdown fences.\n\
\n\
```json\n\
{{\n  \"eventModel\": <healed model>,\n  \"summary\": \"<one paragraph>\",\n  \"changesMade\": [\"<bullet>\", ...]\n}}\n\
```",
        file_path = path.display(),
        workspace = workspace_root.display(),
        mode_directive = mode_directive,
        auto_block = auto_block,
        residual_block = residual_block,
        validation_block = validation_block,
        neo_summary = neo_summary_block,
        current_file = current_file_content,
        schema = SCHEMA_JSON,
    )
}

/// Pick a claude model based on the residual workload. Cheap heuristic —
/// the deterministic pass already removed the bulk of the work, so the
/// remaining LLM pass is usually small and well-defined.
fn pick_model(residual_count: usize, validation_error_count: usize, has_neo_summary: bool) -> &'static str {
    if !has_neo_summary {
        // No inspection — open-ended audit, opus does best.
        return "opus";
    }
    let total_load = residual_count + validation_error_count;
    if total_load <= 3 {
        // Tiny load → haiku is fast AND cheap. The prompt is short enough
        // and the decisions are well-scoped enough that haiku handles them.
        "haiku"
    } else {
        // Larger residual → sonnet. Opus is overkill when the diff has
        // already mapped the work.
        "sonnet"
    }
}

/// Atomic write via `tmp + rename`. A botched mid-write leaves the
/// original `event-model.json` untouched.
fn atomic_write(path: &Path, content: &str) -> Result<(), NeoError> {
    let tmp = path.with_extension("json.heal-tmp");
    std::fs::write(&tmp, content.as_bytes()).map_err(|e| {
        NeoError::io_at("writing healed event-model.json (tmp)", tmp.clone(), e)
    })?;
    std::fs::rename(&tmp, path).map_err(|e| {
        let _ = std::fs::remove_file(&tmp);
        NeoError::io_at(
            "renaming healed event-model.json into place",
            path.to_path_buf(),
            e,
        )
    })?;
    Ok(())
}

/// One-line human description of a `Residual` for inclusion in the LLM prompt.
fn residual_to_human(r: &crate::ide::heal::diff::Residual) -> String {
    use crate::ide::heal::diff::Residual;
    match r {
        Residual::OrphanModelNode { node_name, node_type, node_id } => format!(
            "orphanModelNode `{node_name}` (type={node_type}, id={node_id}) — exists in the model but not in the code; if it's a typo for a code symbol rename it, otherwise leave it (may be a planned feature)"
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::workspace::Workspace;
    use std::os::unix::fs::PermissionsExt;
    use std::sync::Arc;

    const VALID_MODEL: &str = r#"{
  "id": "m1",
  "name": "demo",
  "chapters": [],
  "entities": [],
  "slices": [],
  "nodes": [],
  "edges": [],
  "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
}"#;

    const INVALID_MODEL: &str = r#"{
  "name": "missing id",
  "chapters": [],
  "entities": [],
  "slices": [],
  "nodes": [],
  "edges": [],
  "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
}"#;

    fn fixture_session(dir: &std::path::Path) -> Session {
        let ws = Workspace::from_root(dir).unwrap();
        Session::new(Arc::new(ws))
    }

    /// Write a bash script at `path` with `body` as its shell content. The
    /// script becomes a stub `claude` that tests point `HealConfig` at via
    /// its absolute path.
    fn write_stub(path: &std::path::Path, body: &str) {
        let script = format!("#!/usr/bin/env bash\n{body}\n");
        std::fs::write(path, script).unwrap();
        let mut perms = std::fs::metadata(path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(path, perms).unwrap();
    }

    fn quick_config(claude_binary: PathBuf, timeout_ms: u64) -> HealConfig {
        HealConfig {
            claude_binary,
            timeout: Duration::from_millis(timeout_ms),
        }
    }

    /// Build a stream-json `result` event whose top-level
    /// `structured_output` field carries the schema-validated wrapper
    /// payload (`{ eventModel, summary, changesMade }`). This mirrors
    /// what claude-code emits when invoked with `--json-schema` — the
    /// human-readable assistant closing message lands on `result`, and
    /// the JSON payload lands on `structured_output`.
    fn stream_json_result_line(event_model_json: &str) -> String {
        let inner = serde_json::json!({
            "eventModel": serde_json::from_str::<serde_json::Value>(event_model_json)
                .expect("event_model_json fixture must be valid JSON"),
            "summary": "stub test",
            "changesMade": ["stub change"],
        });
        let outer = serde_json::json!({
            "type": "result",
            "subtype": "success",
            "is_error": false,
            "result": "stub closing message",
            "structured_output": inner,
        });
        outer.to_string()
    }

    /// Legacy-shape `result` event: payload stringified into
    /// `result.result`, no top-level `structured_output` field. Tests use
    /// this to lock in the extractor's fallback path for the case where
    /// claude-code is invoked without `--json-schema`.
    fn stream_json_result_line_legacy(event_model_json: &str) -> String {
        let inner = serde_json::json!({
            "eventModel": serde_json::from_str::<serde_json::Value>(event_model_json)
                .expect("event_model_json fixture must be valid JSON"),
            "summary": "stub test (legacy)",
            "changesMade": ["stub change"],
        });
        let outer = serde_json::json!({
            "type": "result",
            "subtype": "success",
            "is_error": false,
            "result": inner.to_string(),
        });
        outer.to_string()
    }

    /// Generate a stub-claude shell-script body that emits the given
    /// stream-json `result` line on stdout. Bash here-doc keeps the
    /// nested JSON quoting sane.
    fn stub_emitting_result(result_line: &str) -> String {
        // Use `printf %s` instead of echo so embedded backslashes /
        // quotes survive untouched. The line is single-quoted so the
        // shell does no expansion at all.
        format!("printf '%s\\n' '{}'\nexit 0", result_line.replace('\'', r"'\''"))
    }

    #[tokio::test]
    async fn heal_returns_healed_when_stub_fixes_file() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, INVALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        // Stub emits a structured-output `result` event carrying a
        // valid model. The heal flow parses + validates + writes.
        let result_line = stream_json_result_line(VALID_MODEL);
        write_stub(&stub_path, &stub_emitting_result(&result_line));

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000))
            .await
            .expect("heal should succeed");
        assert_eq!(result.outcome, HealOutcome::Healed);
        // File on disk should now be valid.
        let after = std::fs::read_to_string(&model_path).unwrap();
        assert!(after.contains("\"id\""));
        assert!(after.contains("m1"));
    }

    #[tokio::test]
    async fn heal_returns_still_invalid_when_stub_leaves_errors() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        // Stub echoes BACK the still-invalid model (no `id` field). The
        // heal flow parses + validates → StillInvalid; file untouched.
        let result_line = stream_json_result_line(INVALID_MODEL);
        write_stub(&stub_path, &stub_emitting_result(&result_line));

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000))
            .await
            .expect("heal should return Ok with StillInvalid");
        match result.outcome {
            HealOutcome::StillInvalid { errors } => {
                assert!(!errors.is_empty(), "expected at least one remaining error");
            }
            other => panic!("expected StillInvalid, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn heal_returns_claude_missing_when_binary_absent() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();

        // Point at a path that definitely doesn't exist.
        let bogus = std::path::PathBuf::from("/nonexistent/path/to/claude-does-not-exist-12345");
        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(bogus, 10_000)).await;
        assert!(
            matches!(result, Err(NeoError::HealingClaudeMissing)),
            "expected HealingClaudeMissing, got {result:?}"
        );
    }

    #[tokio::test]
    async fn heal_returns_failed_when_subprocess_nonzero_exit() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        write_stub(&stub_path, "echo 'bang' 1>&2\nexit 1");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000)).await;
        match result {
            Err(NeoError::HealingFailed { reason, stderr_tail }) => {
                assert!(reason.contains("exit code 1"), "reason: {reason}");
                assert!(stderr_tail.contains("bang"), "stderr_tail: {stderr_tail}");
            }
            other => panic!("expected HealingFailed, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn heal_returns_failed_on_timeout() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        // Sleep longer than the timeout.
        write_stub(&stub_path, "sleep 10\nexit 0");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 200)).await;
        match result {
            Err(NeoError::HealingFailed { reason, .. }) => {
                assert!(reason.contains("timed out"), "reason should mention timeout: {reason}");
            }
            other => panic!("expected HealingFailed (timeout), got {other:?}"),
        }
    }

    #[tokio::test]
    async fn heal_errors_when_file_missing() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        // Note: no event-model.json on disk.
        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        write_stub(&stub_path, "exit 0");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000)).await;
        match result {
            Err(NeoError::IoErrorAt { operation, path, .. }) => {
                assert!(operation.contains("event-model.json"), "op: {operation}");
                assert!(path.contains("event-model.json"), "path: {path}");
            }
            other => panic!("expected IoErrorAt, got {other:?}"),
        }
    }

    /// Helper: spawn the stub claude with a script that captures argv +
    /// stdin-prompt to a file, then assert on it.
    async fn run_with_argv_capture(
        workspace: &std::path::Path,
        capture: &std::path::Path,
    ) -> Result<HealEventModelResult, NeoError> {
        run_with_argv_capture_in_mode(workspace, capture, HealMode::Validate).await
    }

    async fn run_with_argv_capture_in_mode(
        workspace: &std::path::Path,
        capture: &std::path::Path,
        mode: HealMode,
    ) -> Result<HealEventModelResult, NeoError> {
        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        // Write argv (one per line) + pwd to the capture file, then exit 0.
        // The model file is left untouched so we get StillInvalid back; the
        // caller only cares about side-effects on the capture file.
        write_stub(
            &stub_path,
            &format!(
                "printf '%s\\n' \"$@\" > '{cap}'\nprintf 'PWD=%s\\n' \"$PWD\" >> '{cap}'\nexit 0",
                cap = capture.display()
            ),
        );
        let session = fixture_session(workspace);
        // Need to keep stub_dir alive past the call — leak it by moving into a static-ish
        // location. Simpler: write a copy of the stub to a path we own.
        let owned_stub = workspace.join(".test-claude-stub.sh");
        std::fs::copy(&stub_path, &owned_stub).unwrap();
        let mut perms = std::fs::metadata(&owned_stub).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&owned_stub, perms).unwrap();
        handle_with_config(session, mode, quick_config(owned_stub, 10_000)).await
    }

    #[tokio::test]
    async fn heal_prompt_contains_validation_errors() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        // New prompt contract: the "Remaining ... validation errors" section
        // appears, and the offending field id from the schema error is
        // included in the listed error.
        assert!(
            logged.to_lowercase().contains("validation errors"),
            "argv should include the validation errors header (case-insensitive), got: {logged}"
        );
        assert!(logged.contains("id"), "prompt should name the missing `id` field");
    }

    #[tokio::test]
    async fn heal_prompt_contains_file_path() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        assert!(
            logged.contains("event-model.json"),
            "prompt should include the file path, got: {logged}"
        );
    }

    #[tokio::test]
    async fn heal_prompt_contains_schema() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        assert!(
            logged.contains("$schema"),
            "prompt should include the JSON Schema header, got first 200 chars: {}",
            &logged.chars().take(200).collect::<String>()
        );
    }

    #[tokio::test]
    async fn heal_prompt_states_mode_and_includes_schema_plus_model() {
        // The new prompt contract (post-deterministic-pass): much shorter
        // than the original. Asserts the load-bearing pieces remain:
        //   * mode header (Validate or Improve)
        //   * the schema embedded verbatim
        //   * the current model embedded
        //   * the file path
        //   * structured-output format reminder (`eventModel` wrapper)
        // The detailed wiring-policy teaching is no longer needed — the
        // Rust deterministic pre-pass handled that work.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();

        assert!(logged.contains("MODE = VALIDATE"), "prompt should name the mode");
        assert!(logged.contains("$schema"), "prompt should embed the schema");
        assert!(
            logged.contains("event-model.json"),
            "prompt should include the file path"
        );
        assert!(
            logged.contains("\"eventModel\""),
            "prompt should reference the structured-output `eventModel` wrapper"
        );
        assert!(
            logged.contains("Output format"),
            "prompt should include an output format section"
        );
    }

    #[tokio::test]
    async fn heal_uses_workspace_root_as_cwd() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        // PWD=<workspace canonical path>
        let canonical = workspace.canonicalize().unwrap();
        assert!(
            logged.contains(&format!("PWD={}", canonical.display())),
            "stub PWD should match workspace root; got: {logged}"
        );
    }

    #[tokio::test]
    async fn heal_passes_allowed_tools_flag() {
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        assert!(
            logged.contains("--allowed-tools"),
            "argv should include --allowed-tools, got: {logged}"
        );
        // Read-only — the agent must NOT have Edit/Write/Bash because
        // we write the healed file ourselves.
        assert!(
            logged.contains("\nRead\n") || logged.contains(" Read "),
            "argv should pass `Read` (and only Read) as the allowed-tools value, got: {logged}"
        );
        assert!(
            !logged.contains("Read,Edit,Write"),
            "argv must NOT grant Edit or Write; agent writes nothing — we do",
        );
        // And the structured-output schema MUST be set.
        assert!(
            logged.contains("--json-schema"),
            "argv should include --json-schema, got: {logged}"
        );
        assert!(
            logged.contains("eventModel"),
            "json-schema arg should mention `eventModel`, got: {logged}",
        );
    }

    #[tokio::test]
    async fn heal_passes_opus_when_workspace_is_not_neohaskell() {
        // Empty workspace (no `src/` with NeoHaskell domains). The
        // `neo inspect` summary is absent, so the agent has to do open-
        // ended discovery — we stay on opus for that.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        assert!(logged.contains("--model"), "argv should include --model, got: {logged}");
        assert!(
            logged.contains("\nopus\n") || logged.contains("\nopus") || logged.contains(" opus "),
            "without a NeoHaskell project summary, model should be opus; got: {logged}"
        );
    }

    #[tokio::test]
    async fn heal_uses_haiku_when_neo_inspect_finds_a_small_residual() {
        // Drop a minimal NeoHaskell domain into the workspace so
        // `neo inspect` returns a non-empty summary. With a small total
        // load (residuals + validation errors ≤ 3) the heal flow should
        // pick haiku — cheap and fast for the trivially-scoped LLM pass.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();
        let core = workspace.join("src/App/Cart/Core.hs");
        std::fs::create_dir_all(core.parent().unwrap()).unwrap();
        std::fs::write(
            &core,
            "module App.Cart.Core where\n\
             data CartEvent = ItemAdded {} deriving (Generic)\n",
        )
        .unwrap();
        let cmd = workspace.join("src/App/Cart/Commands/AddItem.hs");
        std::fs::create_dir_all(cmd.parent().unwrap()).unwrap();
        std::fs::write(
            &cmd,
            "module App.Cart.Commands.AddItem where\n\
             decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        )
        .unwrap();

        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture(workspace, &capture).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        assert!(logged.contains("--model"), "argv should include --model");
        assert!(
            logged.contains("\nhaiku\n") || logged.contains(" haiku ") || logged.contains("\nhaiku"),
            "small NeoHaskell residual (1 command + 1 event + 1 validation error) should route to haiku; got first 500 chars: {}",
            &logged.chars().take(500).collect::<String>()
        );
        // The summary block embeds the discovered command + event.
        assert!(
            logged.contains("ItemAdded") && logged.contains("AddItem"),
            "prompt should contain the discovered command + event"
        );
    }

    #[tokio::test]
    async fn heal_uses_sonnet_when_orphan_residual_is_large() {
        // After the materialisation pre-pass, the only LLM-residual class
        // is orphan model nodes (model has them, code doesn't). Plant 5
        // orphan command nodes in a passing-schema model with a tiny
        // NeoHaskell project, then run in Improve mode to force the LLM
        // pass. Five residuals → total_load > 3 → sonnet.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let core = workspace.join("src/App/Cart/Core.hs");
        std::fs::create_dir_all(core.parent().unwrap()).unwrap();
        std::fs::write(
            &core,
            "module App.Cart.Core where\n\
             data CartEvent = ItemAdded {} deriving (Generic)\n",
        )
        .unwrap();
        let cmd = workspace.join("src/App/Cart/Commands/AddItem.hs");
        std::fs::create_dir_all(cmd.parent().unwrap()).unwrap();
        std::fs::write(
            &cmd,
            "module App.Cart.Commands.AddItem where\n\
             decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        )
        .unwrap();
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [{ "id": "ch1", "name": "Main", "order": 0 }],
            "entities": [{ "id": "ent1", "name": "Cart", "order": 0 }],
            "slices": [{ "id": "sl1", "name": "Stale", "chapterId": "ch1", "order": 0 }],
            "nodes": [
                { "id": "orphan1", "type": "command", "name": "OrphanOne",   "sliceId": "sl1", "entityId": "ent1" },
                { "id": "orphan2", "type": "command", "name": "OrphanTwo",   "sliceId": "sl1", "entityId": "ent1" },
                { "id": "orphan3", "type": "command", "name": "OrphanThree", "sliceId": "sl1", "entityId": "ent1" },
                { "id": "orphan4", "type": "command", "name": "OrphanFour",  "sliceId": "sl1", "entityId": "ent1" },
                { "id": "orphan5", "type": "command", "name": "OrphanFive",  "sliceId": "sl1", "entityId": "ent1" }
            ],
            "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        std::fs::write(
            workspace.join("event-model.json"),
            serde_json::to_string_pretty(&model).unwrap(),
        )
        .unwrap();

        let capture = workspace.join("argv.log");
        let _ = run_with_argv_capture_in_mode(workspace, &capture, HealMode::Improve).await;
        let logged = std::fs::read_to_string(&capture).unwrap();
        assert!(
            logged.contains("\nsonnet\n") || logged.contains(" sonnet ") || logged.contains("\nsonnet"),
            "large orphan residual should route to sonnet; got first 500 chars: {}",
            &logged.chars().take(500).collect::<String>()
        );
    }

    #[tokio::test]
    async fn heal_validate_mode_skips_subprocess_on_valid_file() {
        // Default mode (Validate) on a valid file must short-circuit before
        // spawning claude — saves API tokens on the auto-triggered path.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), VALID_MODEL).unwrap();
        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        // Stub would corrupt the file if invoked — we assert it isn't.
        write_stub(&stub_path, "echo 'STUB RAN — should not have been invoked'\nexit 1");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000))
            .await
            .expect("validate mode on valid file should succeed");
        assert_eq!(result.outcome, HealOutcome::Healed);
        // File untouched.
        let after = std::fs::read_to_string(workspace.join("event-model.json")).unwrap();
        assert_eq!(after, VALID_MODEL);
    }

    #[tokio::test]
    async fn heal_improve_mode_materialises_missing_nodes_without_llm() {
        // Pre-change: missing commands / events showed up as residuals and
        // claude was spawned to add them. With the deterministic pre-pass
        // doing the materialisation in Rust, the LLM is no longer needed
        // here — the stub MUST NOT run.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, VALID_MODEL).unwrap();

        let core = workspace.join("src/App/Cart/Core.hs");
        std::fs::create_dir_all(core.parent().unwrap()).unwrap();
        std::fs::write(
            &core,
            "module App.Cart.Core where\n\
             data CartEvent = ItemAdded {} deriving (Generic)\n",
        )
        .unwrap();
        let cmd = workspace.join("src/App/Cart/Commands/AddItem.hs");
        std::fs::create_dir_all(cmd.parent().unwrap()).unwrap();
        std::fs::write(
            &cmd,
            "module App.Cart.Commands.AddItem where\n\
             decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        )
        .unwrap();

        // Bogus claude path: if the fast-path regresses and spawns it, we
        // fail with a clear error instead of a silent stub run.
        let bogus = std::path::PathBuf::from("/nonexistent/heal-stub");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Improve, quick_config(bogus, 10_000))
            .await
            .expect("deterministic materialisation should heal without claude");
        assert_eq!(result.outcome, HealOutcome::Healed);

        // The file on disk now contains a command + event materialised by
        // the deterministic pass.
        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let nodes = patched["nodes"].as_array().unwrap();
        assert!(
            nodes.iter().any(|n| n["type"] == "command" && n["name"] == "AddItem"),
            "deterministic pass should materialise AddItem command; got nodes: {nodes:?}"
        );
        assert!(
            nodes.iter().any(|n| n["type"] == "event" && n["name"] == "ItemAdded"),
            "deterministic pass should materialise ItemAdded event; got nodes: {nodes:?}"
        );
        // And the edge between them.
        let edges = patched["edges"].as_array().unwrap();
        assert!(
            edges.iter().any(|e| e["type"] == "commandProducesEvent"),
            "deterministic pass should wire commandProducesEvent edge; got edges: {edges:?}"
        );
    }

    #[tokio::test]
    async fn heal_fixes_positions_when_no_neohaskell_inspection() {
        // No NeoHaskell project: just a hand-authored event-model.json
        // whose integration sits at y=500 (event band, wrong). The
        // deterministic pass MUST still snap it back to the
        // command/query/integration band — the heal flow is the right
        // place to fix layout regardless of whether the workspace is
        // backed by code.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        let base_model = serde_json::json!({
            "id": "m1",
            "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Stuff", "order": 0 }],
            "slices": [
                { "id": "sl1", "name": "OnlySlice", "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "intg1", "type": "integration", "name": "Misplaced",
                  "sliceId": "sl1", "kind": "outbound" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": {
                    "intg1": { "x": 200, "y": 500 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        std::fs::write(
            &model_path,
            serde_json::to_string_pretty(&base_model).unwrap(),
        )
        .unwrap();

        // No src/ directory → inspection returns empty domains. Claude
        // must not be spawned for a position-only repair.
        let bogus = std::path::PathBuf::from("/nonexistent/heal-stub");
        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(bogus, 10_000))
            .await
            .expect("deterministic pass should run without inspection");
        assert_eq!(result.outcome, HealOutcome::Healed);

        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let y = patched["layout"]["nodePositions"]["intg1"]["y"].as_f64().unwrap();
        assert!(
            (y - 120.0).abs() < f64::EPSILON,
            "integration y should snap from 500 to canonical 120; got {y}",
        );
    }

    #[tokio::test]
    async fn heal_cancel_kills_subprocess_and_returns_cancelled() {
        // Stub that sleeps long enough to be cancellable but exits 0 if
        // not interrupted. We fire the heal in a tokio task, wait briefly
        // for it to spawn the subprocess (install heal_cancel), then call
        // session.cancel_heal() on the cloned session. The heal must
        // return Cancelled with the deterministic_applied count from the
        // pre-pass.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        // INVALID_MODEL forces needs_llm = true (validation fails on
        // missing `id`). No NeoHaskell project → deterministic_applied
        // stays at 0, which is what the assertion expects.
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        // Sleep 30 seconds — far longer than the test will wait. If the
        // kill path doesn't fire, the test times out (15s here).
        write_stub(&stub_path, "sleep 30\nexit 0");

        let session = fixture_session(workspace);
        let session_clone = session.clone();
        let heal_task = tokio::spawn(async move {
            handle_with_config(session_clone, HealMode::Validate, quick_config(stub_path, 15_000)).await
        });

        // Poll for the heal_cancel slot to be installed — heal_event_model
        // installs it AFTER spawning the subprocess.
        let mut tries = 0;
        loop {
            tokio::time::sleep(Duration::from_millis(50)).await;
            tries += 1;
            if session.cancel_heal() {
                break;
            }
            assert!(tries < 60, "heal_cancel slot never appeared — heal didn't reach the subprocess phase");
        }

        let result = heal_task.await.unwrap().expect("heal should resolve with Ok on cancel");
        match result.outcome {
            HealOutcome::Cancelled { deterministic_applied } => {
                assert_eq!(
                    deterministic_applied, 0,
                    "no NeoHaskell + no positions to fix → 0 deterministic applied; got {deterministic_applied}",
                );
            }
            other => panic!("expected Cancelled, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn heal_cancel_persists_deterministic_patches_to_disk() {
        // Cancel during the LLM stage MUST still write the pre-pass's
        // patches (otherwise the user loses the free wins). Plant a
        // valid-with-bad-position model so the deterministic pass has
        // real work to apply, then cancel while the stub is sleeping.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        // Valid JSON + valid schema, but integration at y=500 needs fix.
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Stuff", "order": 0 }],
            "slices": [{ "id": "sl1", "name": "Only", "chapterId": null, "order": 0 }],
            "nodes": [
                { "id": "intg1", "type": "integration", "name": "Misplaced",
                  "sliceId": "sl1", "kind": "outbound" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": { "intg1": { "x": 200, "y": 500 } },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        std::fs::write(&model_path, serde_json::to_string_pretty(&model).unwrap()).unwrap();
        // Plant a Cart domain with a command + event the model doesn't
        // have — that forces residual_count > 0 and (with Improve mode)
        // makes needs_llm = true so the LLM stage is reached.
        let core = workspace.join("src/App/Cart/Core.hs");
        std::fs::create_dir_all(core.parent().unwrap()).unwrap();
        std::fs::write(&core, "module App.Cart.Core where\ndata CartEvent = ItemAdded {} deriving (Generic)\n").unwrap();
        let cmd = workspace.join("src/App/Cart/Commands/AddItem.hs");
        std::fs::create_dir_all(cmd.parent().unwrap()).unwrap();
        std::fs::write(
            &cmd,
            "module App.Cart.Commands.AddItem where\n\
             decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        )
        .unwrap();
        // Wait — actually, Improve mode short-circuits when residuals=0
        // and the file's valid. With the materialiser pass, the inspection
        // creates the missing nodes deterministically → applied > 0,
        // residuals == 0 → still no LLM. To force the LLM we'd need
        // genuine residuals (orphans) or an invalid file. Switch to an
        // orphan in the model.
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Cart", "order": 0 }],
            "slices": [{ "id": "sl1", "name": "Only", "chapterId": null, "order": 0 }],
            "nodes": [
                { "id": "intg1", "type": "integration", "name": "Misplaced",
                  "sliceId": "sl1", "kind": "outbound" },
                { "id": "orphan", "type": "command", "name": "OrphanNotInCode",
                  "sliceId": "sl1", "entityId": "ent1" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": {
                    "intg1": { "x": 200, "y": 500 },
                    "orphan": { "x": 200, "y": 120 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        std::fs::write(&model_path, serde_json::to_string_pretty(&model).unwrap()).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        write_stub(&stub_path, "sleep 30\nexit 0");

        let session = fixture_session(workspace);
        let session_clone = session.clone();
        let heal_task = tokio::spawn(async move {
            handle_with_config(session_clone, HealMode::Improve, quick_config(stub_path, 15_000)).await
        });

        let mut tries = 0;
        loop {
            tokio::time::sleep(Duration::from_millis(50)).await;
            tries += 1;
            if session.cancel_heal() {
                break;
            }
            assert!(tries < 60, "heal_cancel slot never appeared");
        }

        let result = heal_task.await.unwrap().expect("heal should resolve on cancel");
        match result.outcome {
            HealOutcome::Cancelled { deterministic_applied } => {
                assert!(
                    deterministic_applied > 0,
                    "pre-pass should have applied at least the position fix; got {deterministic_applied}",
                );
            }
            other => panic!("expected Cancelled, got {other:?}"),
        }

        // The position fix MUST be on disk.
        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let y = patched["layout"]["nodePositions"]["intg1"]["y"].as_f64().unwrap();
        assert!(
            (y - 120.0).abs() < f64::EPSILON,
            "cancel must persist deterministic patches — integration y should be 120, got {y}",
        );
    }

    #[tokio::test]
    async fn heal_deterministic_pass_alone_repairs_missing_edge_without_llm() {
        // The fast-path's load-bearing scenario: a valid file with an
        // incomplete wiring (event not yet feeding its query), backed by
        // NeoHaskell code that says the edge SHOULD exist. The Rust
        // deterministic pass identifies the missing edge, patches the
        // file, re-validates, and writes — claude is never spawned.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        // Hand-build a small valid model where ItemAdded already exists
        // but the eventFeedsQuery edge to CartSummary is missing.
        let base_model = serde_json::json!({
            "id": "m1",
            "name": "demo",
            "chapters": [{ "id": "ch1", "name": "Main", "order": 0 }],
            "entities": [{ "id": "ent1", "name": "Cart", "order": 0 }],
            "slices": [
                { "id": "sl1", "name": "AddItem",     "chapterId": "ch1", "order": 0 },
                { "id": "sl2", "name": "CartSummary", "chapterId": "ch1", "order": 1 }
            ],
            "nodes": [
                { "id": "cmd1", "type": "command", "name": "AddItem",     "sliceId": "sl1", "entityId": "ent1" },
                { "id": "ev1",  "type": "event",   "name": "ItemAdded",   "sliceId": "sl1", "entityId": "ent1" },
                { "id": "qy1",  "type": "query",   "name": "CartSummary", "sliceId": "sl2" }
            ],
            "edges": [
                // commandProducesEvent IS already there.
                { "id": "e1", "type": "commandProducesEvent", "sourceId": "cmd1", "targetId": "ev1" }
                // eventFeedsQuery to qy1 is MISSING — diff must add it.
            ],
            "layout": {
                "nodePositions": {
                    "cmd1": { "x": 40,  "y": 120 },
                    "ev1":  { "x": 40,  "y": 400 },
                    "qy1":  { "x": 440, "y": 170 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        std::fs::write(
            &model_path,
            serde_json::to_string_pretty(&base_model).unwrap(),
        )
        .unwrap();

        // NeoHaskell backing.
        let core = workspace.join("src/App/Cart/Core.hs");
        std::fs::create_dir_all(core.parent().unwrap()).unwrap();
        std::fs::write(
            &core,
            "module App.Cart.Core where\n\
             data CartEvent = ItemAdded {} deriving (Generic)\n",
        )
        .unwrap();
        let cmd = workspace.join("src/App/Cart/Commands/AddItem.hs");
        std::fs::create_dir_all(cmd.parent().unwrap()).unwrap();
        std::fs::write(
            &cmd,
            "module App.Cart.Commands.AddItem where\n\
             decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        )
        .unwrap();
        let qry = workspace.join("src/App/Cart/Queries/CartSummary.hs");
        std::fs::create_dir_all(qry.parent().unwrap()).unwrap();
        std::fs::write(
            &qry,
            "module App.Cart.Queries.CartSummary where\n\
             -- subscribes to ItemAdded for the count\n",
        )
        .unwrap();

        // The stub MUST NOT run — point at a non-existent binary so this
        // test fails loudly if the fast-path regresses and tries to spawn.
        let bogus = std::path::PathBuf::from("/nonexistent/heal-stub");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Improve, quick_config(bogus, 10_000))
            .await
            .expect("deterministic pass should fix the file without claude");
        assert_eq!(result.outcome, HealOutcome::Healed);

        // The eventFeedsQuery edge is now in the on-disk file.
        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let edges = patched["edges"].as_array().unwrap();
        assert!(
            edges.iter().any(|e| {
                e["type"] == "eventFeedsQuery"
                    && e["sourceId"] == "ev1"
                    && e["targetId"] == "qy1"
            }),
            "deterministic pass should have added the eventFeedsQuery edge; got: {:?}",
            edges,
        );
    }

    #[tokio::test]
    async fn heal_improve_mode_skips_subprocess_when_no_residuals() {
        // Improve mode on a valid file with NO NeoHaskell context (so the
        // deterministic pass finds zero residuals) MUST short-circuit. The
        // old behavior always spawned claude here; the new architecture
        // saves the API tokens.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), VALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        write_stub(&stub_path, "echo 'STUB RAN — should not have been invoked'\nexit 1");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Improve, quick_config(stub_path, 10_000))
            .await
            .expect("improve mode on a clean valid file should short-circuit");
        assert_eq!(result.outcome, HealOutcome::Healed);
    }

    #[tokio::test]
    async fn heal_returns_healed_writes_agent_payload_to_disk_atomically() {
        // The heal flow's contract: the agent's `eventModel` field is
        // what lands on disk — not whatever was previously there. Use a
        // healed payload whose `id` is unique so the assertion can't
        // false-pass against the INVALID_MODEL we wrote first.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, INVALID_MODEL).unwrap();

        let healed_sentinel = r#"{
  "id": "HEALED_FROM_AGENT",
  "name": "agent-output",
  "chapters": [],
  "entities": [],
  "slices": [],
  "nodes": [],
  "edges": [],
  "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
}"#;
        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        let result_line = stream_json_result_line(healed_sentinel);
        write_stub(&stub_path, &stub_emitting_result(&result_line));

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000))
            .await
            .expect("heal should succeed");
        assert_eq!(result.outcome, HealOutcome::Healed);

        let on_disk = std::fs::read_to_string(&model_path).unwrap();
        assert!(
            on_disk.contains("HEALED_FROM_AGENT"),
            "file must contain the agent's healed payload, got: {on_disk}",
        );
        // The tmp file should be gone.
        let tmp = model_path.with_extension("json.heal-tmp");
        assert!(!tmp.exists(), "temp file should have been renamed away");
    }

    #[tokio::test]
    async fn heal_extracts_structured_output_from_legacy_result_string() {
        // Regression test for the fallback path: when the `result` event
        // has no top-level `structured_output` field (i.e. claude-code was
        // invoked WITHOUT `--json-schema`, or an older version), the
        // extractor must fall back to parsing `result.result` as a JSON
        // string. This is the path the original code took before we
        // wired `--json-schema`, and it should keep working.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, INVALID_MODEL).unwrap();

        let healed_sentinel = r#"{
  "id": "HEALED_FROM_LEGACY",
  "name": "legacy-output",
  "chapters": [],
  "entities": [],
  "slices": [],
  "nodes": [],
  "edges": [],
  "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
}"#;
        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        let result_line = stream_json_result_line_legacy(healed_sentinel);
        write_stub(&stub_path, &stub_emitting_result(&result_line));

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000))
            .await
            .expect("heal should succeed via legacy result.result path");
        assert_eq!(result.outcome, HealOutcome::Healed);

        let on_disk = std::fs::read_to_string(&model_path).unwrap();
        assert!(
            on_disk.contains("HEALED_FROM_LEGACY"),
            "file must contain the legacy-path healed payload, got: {on_disk}",
        );
    }

    #[tokio::test]
    async fn heal_returns_failed_when_agent_emits_no_result_event() {
        // If the stub exits 0 without emitting any `result` event,
        // the parser has nothing to write. Surface as HealingFailed
        // (not StillInvalid) — the agent fundamentally didn't respond.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        std::fs::write(workspace.join("event-model.json"), INVALID_MODEL).unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub_path = stub_dir.path().join("claude");
        write_stub(&stub_path, "echo 'no structured output here'\nexit 0");

        let session = fixture_session(workspace);
        let result = handle_with_config(session, HealMode::Validate, quick_config(stub_path, 10_000))
            .await;
        match result {
            Err(NeoError::HealingFailed { reason, .. }) => {
                assert!(
                    reason.contains("no usable structured output") || reason.contains("result"),
                    "reason should explain the missing result event: {reason}",
                );
            }
            other => panic!("expected HealingFailed, got {other:?}"),
        }
    }
}
