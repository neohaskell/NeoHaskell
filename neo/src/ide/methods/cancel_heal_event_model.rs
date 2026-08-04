//! `workspace/cancelHealEventModel` — abort an in-flight
//! `workspace/healEventModel` request.
//!
//! Signals the running heal handler via the session's `heal_cancel`
//! `Notify` slot. The handler's `tokio::select!` races the cancel
//! signal against the subprocess; on signal it kills `claude`, persists
//! the deterministic pre-pass's patches to disk so the user keeps the
//! free wins, and returns `HealOutcome::Cancelled`.
//!
//! Idempotent: a cancel with no heal in flight returns `cancelled: false`
//! and otherwise does nothing.

use serde::{Deserialize, Serialize};

use crate::errors::NeoError;
use crate::ide::session::Session;

#[derive(Debug, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct CancelHealEventModelParams {}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct CancelHealEventModelResult {
    /// `true` when a heal was in flight and the cancel signal fired.
    /// `false` when no heal was running.
    pub cancelled: bool,
}

pub async fn handle(
    session: Session,
    _params: CancelHealEventModelParams,
) -> Result<CancelHealEventModelResult, NeoError> {
    let cancelled = session.cancel_heal();
    tracing::info!(cancelled, "heal: cancel requested");
    Ok(CancelHealEventModelResult { cancelled })
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
    async fn cancel_with_no_heal_in_flight_returns_false() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());
        let result = handle(session, CancelHealEventModelParams {}).await.unwrap();
        assert_eq!(result, CancelHealEventModelResult { cancelled: false });
    }

    #[tokio::test]
    async fn cancel_signals_the_installed_notify_and_clears_on_guard_drop() {
        let dir = tempfile::tempdir().unwrap();
        let session = fixture_session(dir.path());

        // Install a heal_cancel slot (simulating an in-flight heal).
        let (notify, guard) = session.install_heal_cancel();

        // Spawn a task waiting on the notify; cancel should release it.
        let waiter = {
            let n = notify.clone();
            tokio::spawn(async move { n.notified().await })
        };
        let result = handle(session.clone(), CancelHealEventModelParams {}).await.unwrap();
        assert!(result.cancelled, "cancel must report success when a notify is installed");
        // The waiter wakes up because notify was fired.
        tokio::time::timeout(std::time::Duration::from_millis(500), waiter)
            .await
            .expect("waiter should wake after cancel")
            .unwrap();

        // Dropping the guard clears the slot — the next cancel finds nothing.
        drop(guard);
        let next = handle(session, CancelHealEventModelParams {}).await.unwrap();
        assert!(!next.cancelled, "cancel must report false after guard dropped");
    }
}
