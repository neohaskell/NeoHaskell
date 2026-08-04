//! All registered JSON-RPC methods.
//!
//! Adding a new method should be:
//!   1. A new file under this directory (`my_method.rs`) with params/result
//!      structs and an `async fn handle(session: &Session, p: Params) ->
//!      Result<Result, NeoError>`.
//!   2. A `pub mod my_method;` line below.
//!   3. A `.register("my/method", my_method::handle)` line in `register_all`.
//!
//! If a new method requires touching `src/ide/rpc.rs` or `src/ide/registry.rs`,
//! the foundation is wrong — push back instead of patching.

pub mod cancel_heal_event_model;
pub mod heal_event_model;
pub mod initialize;
pub mod read_event_model;
pub mod relayout_event_model;
pub mod write_event_model;

use crate::ide::registry::MethodRegistry;

pub fn register_all(registry: MethodRegistry) -> MethodRegistry {
    registry
        .register("initialize", initialize::handle)
        .register("workspace/readEventModel", read_event_model::handle)
        .register("workspace/writeEventModel", write_event_model::handle)
        .register("workspace/healEventModel", heal_event_model::handle)
        .register(
            "workspace/cancelHealEventModel",
            cancel_heal_event_model::handle,
        )
        .register(
            "workspace/relayoutEventModel",
            relayout_event_model::handle,
        )
}
