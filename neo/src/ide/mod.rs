//! The `neo ide` API surface: a JSON-RPC 2.0 server mounted on the same
//! axum router that serves the bundled Vite IDE.
//!
//! Design panel: see `/Users/nick/.claude/plans/since-i-got-a-cryptic-planet.md`
//! for the rationale (LSP/Tauri/Replit convergence on JSON-RPC over one WS,
//! typed handlers, transport seam for cloud-eventual).
//!
//! Public surface is `AppState` (the axum router state) and the `methods` /
//! `server` entry points wired in by `commands/ide.rs`.

pub mod heal;
pub mod methods;
pub mod registry;
pub mod rpc;
pub mod server;
pub mod session;
pub mod sync;
pub mod transport;
pub mod validate;
pub mod workspace;

use std::sync::Arc;

use crate::ide::registry::MethodRegistry;
use crate::ide::transport::LocalTransport;

/// Broadcast signal that `event-model.json` changed on disk — emitted by the
/// background source watcher after a successful code→model sync. Every live WS
/// connection subscribes and forwards it to its client as a
/// `$/eventModelChanged` notification, so an open IDE re-reads the model. A
/// unit payload is enough: the frontend re-reads regardless of detail.
pub type ModelChangedTx = tokio::sync::broadcast::Sender<()>;

/// Axum router state shared across `/ws` upgrades. Each WebSocket connection
/// gets a shared reference to the registry (method handlers), the transport
/// (which mints the per-connection `Session` and resolves `Workspace`), and a
/// subscribe handle for the model-changed broadcast.
#[derive(Clone)]
pub struct AppState {
    pub registry: MethodRegistry,
    pub transport: Arc<LocalTransport>,
    pub model_changed_tx: ModelChangedTx,
}
