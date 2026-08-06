//! `workspace/writeEventModel` — write `<workspace_root>/event-model.json`.
//!
//! Atomic via write-tmp-then-rename so a SIGKILL mid-write can't truncate
//! the canonical file. The temp file lives in the same directory as the
//! target so the rename stays on a single filesystem.
//!
//! Last-writer-wins across concurrent sessions in v1. Optimistic concurrency
//! (mtime / sha CAS) is an additive future slice.

use serde::{Deserialize, Serialize};

use crate::errors::NeoError;
use crate::ide::methods::read_event_model::EVENT_MODEL_FILENAME;
use crate::ide::session::Session;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WriteEventModelParams {
    /// Raw bytes to write. The Rust side does not re-parse or pretty-print —
    /// the frontend's serialiser is the source of truth for the on-disk shape.
    pub content: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WriteEventModelResult {
    /// Echo the absolute path the file landed at, so the frontend can show
    /// the user "saved to <path>" without having to derive it client-side.
    pub path: String,
}

pub async fn handle(
    session: Session,
    params: WriteEventModelParams,
) -> Result<WriteEventModelResult, NeoError> {
    let path = session.workspace.root.join(EVENT_MODEL_FILENAME);
    let tmp = session
        .workspace
        .root
        .join(format!("{EVENT_MODEL_FILENAME}.tmp"));

    std::fs::write(&tmp, params.content.as_bytes())
        .map_err(|e| NeoError::io_at("writing temp `event-model.json.tmp`", tmp.clone(), e))?;

    std::fs::rename(&tmp, &path).map_err(|e| {
        // Best-effort cleanup of the stranded tmp; if cleanup fails too, the
        // rename error is what the user wants surfaced.
        let _ = std::fs::remove_file(&tmp);
        NeoError::io_at(
            "renaming temp to `event-model.json`",
            path.clone(),
            e,
        )
    })?;

    Ok(WriteEventModelResult { path: path.display().to_string() })
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

    #[tokio::test]
    async fn write_creates_file_with_exact_content() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        let content = r#"{"name":"demo","slices":[]}"#.to_string();
        let result = handle(
            session,
            WriteEventModelParams { content: content.clone() },
        )
        .await
        .unwrap();

        let on_disk = std::fs::read_to_string(dir.path().join("event-model.json")).unwrap();
        assert_eq!(on_disk, content);
        assert!(result.path.ends_with("event-model.json"), "result path: {}", result.path);
    }

    #[tokio::test]
    async fn write_overwrites_existing_file() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), "OLD CONTENT").unwrap();
        let session = fixture_session(dir.path());
        handle(
            session,
            WriteEventModelParams { content: "NEW CONTENT".to_string() },
        )
        .await
        .unwrap();
        let on_disk = std::fs::read_to_string(dir.path().join("event-model.json")).unwrap();
        assert_eq!(on_disk, "NEW CONTENT");
    }

    #[tokio::test]
    async fn write_does_not_leave_tmp_file_behind() {
        // Atomicity smoke: the .tmp must not be visible after a successful write.
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        handle(
            session,
            WriteEventModelParams { content: "anything".to_string() },
        )
        .await
        .unwrap();
        assert!(
            !dir.path().join("event-model.json.tmp").exists(),
            "tmp file should not linger after a successful write",
        );
    }

    #[tokio::test]
    async fn write_then_read_round_trips() {
        let dir = tempfile::tempdir().unwrap();
        let original = "{\n  \"deeply\": {\n    \"nested\": true\n  }\n}\n";

        let session = fixture_session(dir.path());
        handle(
            session,
            WriteEventModelParams { content: original.to_string() },
        )
        .await
        .unwrap();

        let session = fixture_session(dir.path());
        let read_result = super::super::read_event_model::handle(
            session,
            super::super::read_event_model::ReadEventModelParams {},
        )
        .await
        .unwrap();
        assert_eq!(read_result.content.as_deref(), Some(original));
    }
}
