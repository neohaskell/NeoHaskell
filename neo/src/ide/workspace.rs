//! The `Workspace` is the unit a `Session` operates on. Locally there's
//! always exactly one (the cwd at server boot). The cloud transport will
//! later mint one per tenant.
//!
//! The "Replit mistake" the design panel warned about: never let methods take
//! `cwd` or `project_root` as a param, never stash workspace state in a
//! `lazy_static` or handler closure — every `Session` carries its
//! `Arc<Workspace>` explicitly, set by the transport at accept time.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::errors::NeoError;

/// Deterministic, human-readable id for a workspace. In local mode this is
/// the canonical absolute path string of the workspace root; identical input
/// yields identical id across server restarts. The cloud transport will
/// substitute an opaque tenant-scoped id without changing the wire shape.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct WorkspaceId(pub String);

impl std::fmt::Display for WorkspaceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// What `initialize` reports about the project, if `neo.json` exists at the
/// workspace root. Absent → the user ran `neo ide` outside a project.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NeoProjectInfo {
    pub name: String,
    pub version: String,
    pub neo_version: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Workspace {
    pub id: WorkspaceId,
    pub root: PathBuf,
    pub project: Option<NeoProjectInfo>,
}

impl Workspace {
    /// Build a `Workspace` rooted at `root` (which is canonicalised). Reads
    /// `neo.json` if present and extracts the `NeoProjectInfo`; absent
    /// `neo.json` is fine — `project` is `None`.
    pub fn from_root(root: impl AsRef<Path>) -> Result<Self, NeoError> {
        let root = root.as_ref();
        let canonical = root
            .canonicalize()
            .map_err(|e| NeoError::io_at("canonicalising workspace root", root.to_path_buf(), e))?;
        let id = WorkspaceId(canonical.display().to_string());
        let neo_json_path = canonical.join("neo.json");
        let project = if neo_json_path.exists() {
            match crate::config::NeoConfig::load(&neo_json_path) {
                Ok(cfg) => Some(NeoProjectInfo {
                    name: cfg.name,
                    version: cfg.version,
                    neo_version: cfg.neo_version,
                }),
                // If neo.json is present but malformed, we still mint a
                // workspace — the IDE should let the user *see* the project
                // even if it can't be parsed. The malformed-config diagnostic
                // surfaces through later method calls that try to use it.
                Err(_) => None,
            }
        } else {
            None
        };
        Ok(Self { id, root: canonical, project })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn workspace_id_is_canonical_path_string() {
        let dir = tempfile::tempdir().unwrap();
        let ws_a = Workspace::from_root(dir.path()).unwrap();
        let ws_b = Workspace::from_root(dir.path()).unwrap();
        // Deterministic: same input path → same id
        assert_eq!(ws_a.id, ws_b.id);
        // The id is a canonical path string — `canonicalize()` on macOS may
        // prefix `/private` to /tmp; on Linux it may not. Either way it's
        // absolute and stable.
        assert!(ws_a.id.0.starts_with('/'), "id should be absolute: {}", ws_a.id);
    }

    #[test]
    fn workspace_project_is_none_when_no_neo_json() {
        let dir = tempfile::tempdir().unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        assert!(ws.project.is_none(), "no neo.json → project None");
    }

    #[test]
    fn workspace_project_is_some_when_neo_json_present() {
        let dir = tempfile::tempdir().unwrap();
        let neo_json = dir.path().join("neo.json");
        std::fs::write(
            &neo_json,
            r#"{"name":"demo","version":"0.0.1","neo-version":"0.1.0"}"#,
        )
        .unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        let project = ws.project.expect("project should be parsed from neo.json");
        assert_eq!(project.name, "demo");
        assert_eq!(project.version, "0.0.1");
        assert_eq!(project.neo_version, "0.1.0");
    }

    #[test]
    fn workspace_project_is_none_when_neo_json_malformed() {
        // Per Workspace::from_root's contract: the IDE should still mint the
        // workspace so the user can see + fix the broken neo.json via the
        // editor.
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("neo.json"), "{ not valid json").unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        assert!(ws.project.is_none(), "malformed neo.json should not crash workspace mint");
    }

}
