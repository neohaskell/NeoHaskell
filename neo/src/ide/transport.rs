//! Transport layer.
//!
//! This is the **one seam** the design panel (LSP, Tauri, Replit/Coder) all
//! insisted on: handlers must never see HTTP headers, raw requests, or
//! `cwd` — they take a `&Session` and resolve the workspace through a
//! `Transport` impl. Swapping `LocalTransport` for a future
//! `CloudTransport` (bearer-token auth, tenant DB lookup, per-tenant chroot)
//! is a pure addition with no application-protocol change.

use std::sync::Arc;

use crate::ide::rpc::RpcError;
use crate::ide::session::Session;
use crate::ide::workspace::Workspace;

pub trait Transport: Send + Sync + 'static {
    /// Mint a `Session` for a freshly-upgraded WebSocket. Local impl accepts
    /// anything; cloud impl validates auth and attaches the tenant's
    /// workspace. The returned `Session` already carries an `Arc<Workspace>`
    /// — handlers read `session.workspace` directly.
    fn accept(&self) -> Result<Session, RpcError>;
}

/// The trust-local transport: every WS upgrade mints a session attached to
/// the one workspace we registered at server boot. No auth, no tenant
/// routing.
pub struct LocalTransport {
    workspace: Arc<Workspace>,
}

impl LocalTransport {
    pub fn new(workspace: Workspace) -> Self {
        Self { workspace: Arc::new(workspace) }
    }
}

impl Transport for LocalTransport {
    fn accept(&self) -> Result<Session, RpcError> {
        Ok(Session::new(self.workspace.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_transport_accept_mints_session_with_workspace() {
        let dir = tempfile::tempdir().unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        let ws_id = ws.id.clone();
        let transport = LocalTransport::new(ws);
        let session = transport.accept().expect("local accept always succeeds");
        assert_eq!(session.workspace.id, ws_id);
    }

    #[test]
    fn local_transport_two_accepts_share_workspace_but_have_distinct_session_ids() {
        let dir = tempfile::tempdir().unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        let transport = LocalTransport::new(ws);
        let a = transport.accept().unwrap();
        let b = transport.accept().unwrap();
        assert_eq!(a.workspace.id, b.workspace.id);
        assert!(Arc::ptr_eq(&a.workspace, &b.workspace), "share the same Arc");
        assert_ne!(a.id, b.id);
    }
}
