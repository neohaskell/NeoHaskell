//! `initialize` — handshake: client introduces itself, server reports its
//! version + workspace + (empty for v1) capability map.
//!
//! Shape borrowed from LSP. Capabilities are intentionally empty in v1; the
//! field exists so future methods can announce their availability without a
//! protocol-version bump.

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::errors::NeoError;
use crate::ide::session::Session;
use crate::ide::workspace::NeoProjectInfo;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeParams {
    #[allow(dead_code)]
    pub client_info: ClientInfo,
    /// Open-shaped — accepted and ignored in v1. Future versions of the
    /// protocol may negotiate against this.
    #[serde(default)]
    #[allow(dead_code)]
    pub capabilities: serde_json::Value,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClientInfo {
    #[allow(dead_code)]
    pub name: String,
    #[allow(dead_code)]
    pub version: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResult {
    pub server_info: ServerInfo,
    pub server_capabilities: ServerCapabilities,
    pub workspace: WorkspaceInfo,
    pub session_id: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerInfo {
    pub name: &'static str,
    pub version: &'static str,
}

#[derive(Debug, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ServerCapabilities {
    // Intentionally empty in v1 — the shape exists so future methods can
    // announce. Don't add fields here speculatively.
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceInfo {
    pub id: String,
    pub root: PathBuf,
    pub project: Option<NeoProjectInfo>,
}

pub async fn handle(
    session: Session,
    _params: InitializeParams,
) -> Result<InitializeResult, NeoError> {
    // Workspace is already attached to the session by the transport at
    // accept time — handlers never re-resolve cwd, never reach into a
    // task-local. This is the Replit-mistake we explicitly designed away.
    let workspace = &session.workspace;
    Ok(InitializeResult {
        server_info: ServerInfo {
            name: "neo",
            version: env!("CARGO_PKG_VERSION"),
        },
        server_capabilities: ServerCapabilities::default(),
        workspace: WorkspaceInfo {
            id: workspace.id.0.clone(),
            root: workspace.root.clone(),
            project: workspace.project.clone(),
        },
        session_id: session.id.0.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::session::Session;
    use crate::ide::workspace::Workspace;
    use std::sync::Arc;

    fn fixture_session(dir: &std::path::Path) -> Session {
        let ws = Workspace::from_root(dir).unwrap();
        Session::new(Arc::new(ws))
    }

    fn fixture_params() -> InitializeParams {
        InitializeParams {
            client_info: ClientInfo {
                name: "test-client".to_string(),
                version: "0.0.0".to_string(),
            },
            capabilities: serde_json::Value::Null,
        }
    }

    #[tokio::test]
    async fn initialize_returns_server_info_and_workspace() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        let session_workspace_id = session.workspace.id.clone();
        let result = handle(session, fixture_params()).await.unwrap();
        assert_eq!(result.server_info.name, "neo");
        assert_eq!(result.server_info.version, env!("CARGO_PKG_VERSION"));
        assert_eq!(result.workspace.id, session_workspace_id.0);
        assert!(result.workspace.project.is_none(), "tempdir has no neo.json");
        assert!(result.session_id.starts_with("session_"));
    }

    #[tokio::test]
    async fn initialize_serialized_uses_camel_case() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, fixture_params()).await.unwrap();
        let s = serde_json::to_string(&result).unwrap();
        assert!(s.contains("\"serverInfo\""), "camelCase serverInfo: {s}");
        assert!(s.contains("\"serverCapabilities\""), "camelCase serverCapabilities: {s}");
        assert!(s.contains("\"sessionId\""), "camelCase sessionId: {s}");
        assert!(s.contains("\"workspace\""), "workspace present: {s}");
    }

    #[tokio::test]
    async fn initialize_reports_project_when_neo_json_present() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("neo.json"),
            r#"{"name":"demo","version":"1.2.3","neo-version":"0.1.0"}"#,
        )
        .unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, fixture_params()).await.unwrap();
        let s = serde_json::to_string(&result).unwrap();
        let project = result.workspace.project.as_ref().expect("neo.json present");
        assert_eq!(project.name, "demo");
        assert_eq!(project.version, "1.2.3");
        assert_eq!(project.neo_version, "0.1.0");
        assert!(s.contains("\"neoVersion\":\"0.1.0\""), "neo_version is camelCased: {s}");
    }
}
