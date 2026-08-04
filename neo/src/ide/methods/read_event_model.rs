//! `workspace/readEventModel` — read `<workspace_root>/event-model.json` and
//! validate it against the embedded JSON Schema + referential-integrity rules.
//!
//! File-not-found is success-with-`{ status: "notFound", content: null }`,
//! not an error, so the frontend can treat "no file yet" as a fresh project
//! without branching on the JSON-RPC error code. When the file exists but
//! fails validation, `content` is still returned (so the modal can show the
//! user what's there) alongside the structured error list. Any other IO
//! failure (permissions, disk error) surfaces as `NeoError::IoErrorAt`.

use serde::{Deserialize, Serialize};

use crate::errors::NeoError;
use crate::ide::session::Session;
use crate::ide::validate::{self, ValidationOutcome};

pub const EVENT_MODEL_FILENAME: &str = "event-model.json";

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadEventModelParams {
    // No params in v1. Open shape for future filters / variants.
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadEventModelResult {
    /// Raw file contents (JSON string). `None` only when the file does not
    /// exist (`validation.status == "notFound"`). Present even when validation
    /// fails — the frontend modal displays it for context.
    pub content: Option<String>,
    /// Validation outcome. The frontend branches on `status` to decide
    /// whether to load, prompt for heal, or fall back to local state.
    pub validation: ValidationOutcome,
}

/// Read `path` and validate it against the schema + referential rules.
///
/// The single source of truth shared by the IDE `workspace/readEventModel`
/// handler below AND the `neo validate` CLI (`src/commands/validate.rs`), so the
/// two can never disagree on what "valid / absent / invalid / malformed" means.
///
/// - Missing file → `Ok((None, NotFound))` — absence is deliberately NOT an IO
///   error (a fresh project legitimately has no model yet).
/// - Any other IO failure (permissions, path is a directory, …) → `Err(io_at)`.
/// - Readable file → `Ok((Some(content), validate_event_model(&content)))`; the
///   content is returned even when validation fails so callers can display it.
pub fn read_and_validate(
    path: &std::path::Path,
) -> Result<(Option<String>, ValidationOutcome), NeoError> {
    match std::fs::read_to_string(path) {
        Ok(content) => {
            let validation = validate::validate_event_model(&content);
            Ok((Some(content), validation))
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            Ok((None, ValidationOutcome::not_found()))
        }
        Err(e) => Err(NeoError::io_at(
            "reading `event-model.json`",
            path.to_path_buf(),
            e,
        )),
    }
}

pub async fn handle(
    session: Session,
    _params: ReadEventModelParams,
) -> Result<ReadEventModelResult, NeoError> {
    let path = session.workspace.root.join(EVENT_MODEL_FILENAME);
    let (content, validation) = read_and_validate(&path)?;
    match &validation {
        ValidationOutcome::Valid => {
            tracing::debug!(
                path = %path.display(),
                bytes = content.as_deref().map(str::len).unwrap_or(0),
                "readEventModel: valid",
            );
        }
        ValidationOutcome::Invalid { errors } => {
            tracing::info!(
                path = %path.display(),
                error_count = errors.len(),
                "readEventModel: validation errors",
            );
        }
        ValidationOutcome::MalformedJson { parse_error } => {
            tracing::info!(
                path = %path.display(),
                parse_error = %parse_error,
                "readEventModel: file is malformed JSON",
            );
        }
        ValidationOutcome::NotFound => {
            tracing::debug!(path = %path.display(), "readEventModel: file not found");
        }
    }
    Ok(ReadEventModelResult { content, validation })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::workspace::Workspace;
    use std::sync::Arc;

    fn fixture_session(dir: &std::path::Path) -> Session {
        let ws = Workspace::from_root(dir).unwrap();
        Session::new(Arc::new(ws))
    }

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

    #[tokio::test]
    async fn read_returns_validation_not_found_for_missing_file() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        assert!(result.content.is_none(), "missing file → None content");
        assert!(
            matches!(result.validation, ValidationOutcome::NotFound),
            "expected NotFound, got {:?}",
            result.validation
        );
    }

    #[tokio::test]
    async fn read_returns_validation_valid_for_good_file() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), VALID_MODEL).unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        assert_eq!(result.content.as_deref(), Some(VALID_MODEL));
        assert!(
            matches!(result.validation, ValidationOutcome::Valid),
            "expected Valid, got {:?}",
            result.validation
        );
    }

    #[tokio::test]
    async fn read_returns_validation_invalid_for_schema_violation() {
        let dir = tempfile::tempdir().unwrap();
        // Valid JSON, but missing the required `id` field.
        let bad = r#"{"name":"demo","chapters":[],"entities":[],"slices":[],"nodes":[],"edges":[],"layout":{"nodePositions":{},"viewport":{"x":0,"y":0,"zoom":1}}}"#;
        std::fs::write(dir.path().join("event-model.json"), bad).unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        assert_eq!(result.content.as_deref(), Some(bad));
        match result.validation {
            ValidationOutcome::Invalid { errors } => {
                assert!(!errors.is_empty(), "expected at least one error");
            }
            other => panic!("expected Invalid, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn read_returns_validation_malformed_json_for_unparseable() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), "{not json").unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        assert_eq!(result.content.as_deref(), Some("{not json"));
        assert!(
            matches!(result.validation, ValidationOutcome::MalformedJson { .. }),
            "expected MalformedJson, got {:?}",
            result.validation
        );
    }

    #[tokio::test]
    async fn read_includes_content_even_when_invalid() {
        let dir = tempfile::tempdir().unwrap();
        let bad = "{not json";
        std::fs::write(dir.path().join("event-model.json"), bad).unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        assert_eq!(
            result.content.as_deref(),
            Some(bad),
            "content must be returned even for invalid files"
        );
    }

    #[tokio::test]
    async fn read_preserves_byte_for_byte_when_valid() {
        // Whitespace, indentation, unicode all round-trip — the Rust handler
        // is a pass-through, not a re-serialiser.
        let dir = tempfile::tempdir().unwrap();
        let content = "{\n  \"id\": \"m1\",\n  \"name\": \"日本語\",\n  \"chapters\": [],\n  \"entities\": [],\n  \"slices\": [],\n  \"nodes\": [],\n  \"edges\": [],\n  \"layout\": { \"nodePositions\": {}, \"viewport\": { \"x\": 0, \"y\": 0, \"zoom\": 1 } }\n}\n";
        std::fs::write(dir.path().join("event-model.json"), content).unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        assert_eq!(result.content.as_deref(), Some(content));
        assert!(matches!(result.validation, ValidationOutcome::Valid));
    }

    #[tokio::test]
    async fn read_serialized_uses_camel_case() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, ReadEventModelParams {}).await.unwrap();
        let s = serde_json::to_string(&result).unwrap();
        assert!(s.contains("\"content\":null"), "missing content:null: {s}");
        assert!(s.contains("\"validation\""), "missing validation field: {s}");
        assert!(
            s.contains("\"status\":\"notFound\""),
            "expected camelCase notFound: {s}"
        );
    }
}
