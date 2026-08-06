//! Per-WebSocket-connection session.
//!
//! Minted at WS upgrade by the `Transport`. Carries the `Workspace` directly
//! (resolved by the transport at accept time) so handlers don't need
//! task-local globals or a separate `Ctx` argument to find the workspace
//! they're operating on.

use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};
use tokio::sync::Notify;

use crate::ide::workspace::Workspace;


/// Opaque per-session identifier. We deliberately avoid pulling in `uuid` as
/// a dep — a monotonic counter + the boot timestamp gives an id that is
/// unique within a server lifetime (enough for the local case) and grep-able
/// in logs (`session_42@1747838291`). The cloud transport can substitute a
/// real UUID/JWT-derived id without changing the wire shape.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SessionId(pub String);

impl SessionId {
    pub fn mint() -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        Self(format!("session_{n}@{secs}"))
    }
}

impl std::fmt::Display for SessionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// An outbound WebSocket frame the connection loop should send to the
/// client. Both notifications (from `session.notify`) and final
/// responses (from completed RPC handlers) share this channel so the
/// loop has a single, uniform sender to drain. That way notifications
/// pushed mid-handler are sent IMMEDIATELY, instead of buffering until
/// the handler returns.
#[derive(Debug, Clone)]
pub enum OutboundMessage {
    Notification(crate::ide::rpc::Notification),
    Response(crate::ide::rpc::Response),
}

pub type OutboundSender = tokio::sync::mpsc::UnboundedSender<OutboundMessage>;

#[derive(Debug, Clone)]
pub struct Session {
    pub id: SessionId,
    pub workspace: Arc<Workspace>,
    /// Channel into the per-connection WS sender. `None` for sessions
    /// minted outside a real connection (most unit tests). When `None`,
    /// `notify` silently drops — handlers can call it unconditionally.
    outbound_tx: Option<OutboundSender>,
    /// Cancellation signal for the in-flight `workspace/healEventModel`
    /// request. The heal handler installs a `Notify` here while a heal is
    /// running, then clears it on exit. `workspace/cancelHealEventModel`
    /// reads this slot and calls `notify_one()` so the heal's `tokio::select!`
    /// can race the cancel against `child.wait()`. `Arc<Mutex<…>>` so all
    /// `Session` clones (one per RPC dispatch) see the same slot.
    heal_cancel: Arc<Mutex<Option<Arc<Notify>>>>,
}

impl Session {
    pub fn new(workspace: Arc<Workspace>) -> Self {
        Self {
            id: SessionId::mint(),
            workspace,
            outbound_tx: None,
            heal_cancel: Arc::new(Mutex::new(None)),
        }
    }

    /// Attach an outbound sink. Called by the transport at WS accept
    /// time so handlers running under a real connection can push
    /// notifications to the client mid-flight (progress logs, etc.).
    pub fn with_outbound(mut self, tx: OutboundSender) -> Self {
        self.outbound_tx = Some(tx);
        self
    }

    /// Send a JSON-RPC notification to the client. Non-blocking, fire-
    /// and-forget; drops silently if the connection is closed or the
    /// session was minted without a sink (e.g. unit tests).
    pub fn notify(&self, method: impl Into<String>, params: serde_json::Value) {
        let Some(ref tx) = self.outbound_tx else { return };
        let notif = crate::ide::rpc::Notification {
            jsonrpc: crate::ide::rpc::JsonRpcVersion,
            method: method.into(),
            params,
        };
        let _ = tx.send(OutboundMessage::Notification(notif));
    }

    /// Install a cancellation `Notify` for the in-flight heal. The
    /// returned guard clears the slot on drop so even a panicking heal
    /// handler doesn't leave a stale `Notify` behind that the next
    /// `cancelHealEventModel` call would mis-fire on.
    pub fn install_heal_cancel(&self) -> (Arc<Notify>, HealCancelGuard) {
        let notify = Arc::new(Notify::new());
        if let Ok(mut guard) = self.heal_cancel.lock() {
            *guard = Some(notify.clone());
        }
        let cleanup = HealCancelGuard {
            slot: Arc::clone(&self.heal_cancel),
        };
        (notify, cleanup)
    }

    /// Signal the in-flight heal (if any) to cancel. Returns `true` when
    /// a `Notify` was registered (a heal was actually running), `false`
    /// when nothing was in flight.
    pub fn cancel_heal(&self) -> bool {
        let notify = match self.heal_cancel.lock() {
            Ok(guard) => guard.clone(),
            Err(_) => None,
        };
        match notify {
            Some(n) => {
                n.notify_one();
                true
            }
            None => false,
        }
    }
}

/// RAII guard for the heal-cancel slot. Clears the session's
/// `heal_cancel` on drop so the next heal doesn't see a stale token.
pub struct HealCancelGuard {
    slot: Arc<Mutex<Option<Arc<Notify>>>>,
}

impl Drop for HealCancelGuard {
    fn drop(&mut self) {
        if let Ok(mut guard) = self.slot.lock() {
            *guard = None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_workspace() -> Arc<Workspace> {
        let dir = tempfile::tempdir().unwrap();
        Arc::new(Workspace::from_root(dir.path()).unwrap())
    }

    #[test]
    fn session_id_is_uniquely_minted() {
        let a = SessionId::mint();
        let b = SessionId::mint();
        assert_ne!(a, b, "consecutive mints must differ");
    }

    #[test]
    fn session_id_displays_grep_able() {
        let id = SessionId::mint();
        let s = id.to_string();
        assert!(s.starts_with("session_"), "id should start with `session_`: {s}");
        assert!(s.contains('@'), "id should embed timestamp: {s}");
    }

    #[test]
    fn session_attaches_workspace() {
        let ws = fixture_workspace();
        let ws_id = ws.id.clone();
        let s = Session::new(ws);
        assert_eq!(s.workspace.id, ws_id);
    }
}
