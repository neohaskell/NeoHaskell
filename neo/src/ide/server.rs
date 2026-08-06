//! The axum WebSocket upgrade handler and the per-connection JSON-RPC loop.
//!
//! Lifecycle per connection:
//! 1. axum extracts `WebSocketUpgrade` + `State<AppState>`.
//! 2. We call `transport.accept()` to mint a `Session` (which already carries
//!    the workspace it's bound to).
//! 3. We hand off to the on-upgrade callback which runs the read/dispatch
//!    loop until the client closes or the socket dies.
//!
//! Per the panel's "cancellation reserved, not implemented" stance, the
//! per-connection state is intentionally minimal: just the registry and the
//! session. Cancellable methods land later and will add a CancellationToken
//! map keyed by request id.

use axum::{
    extract::{
        ws::{Message, WebSocket},
        State, WebSocketUpgrade,
    },
    response::IntoResponse,
};
use futures_util::{SinkExt, StreamExt};

use crate::ide::rpc::{parse_incoming, Incoming, Response};
use crate::ide::transport::Transport;
use crate::ide::AppState;

pub async fn ws_upgrade(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| async move {
        // The transport mints a session — accept failure (cloud auth, etc.)
        // closes the socket before any frame is exchanged. In local mode
        // accept never fails.
        let session = match state.transport.accept() {
            Ok(s) => s,
            Err(err) => {
                tracing::warn!(?err.code, %err.message, "transport rejected WS upgrade");
                return;
            }
        };
        tracing::debug!(session_id = %session.id, workspace_id = %session.workspace.id, "ws connection accepted");
        connection_loop(socket, session, state).await;
    })
}

async fn connection_loop(socket: WebSocket, session: crate::ide::session::Session, state: AppState) {
    let (mut sink, mut stream) = socket.split();

    // Per-connection outbound channel. Handlers (running in their own
    // spawned tasks — see below) push notifications and final responses
    // onto this queue; the loop body's only job is to drain it onto the
    // WS sink. Decoupling this from the handler future is what makes
    // mid-RPC notifications (e.g. heal progress logs) reach the client
    // IMMEDIATELY, instead of buffering until the handler returns.
    let (outbound_tx, mut outbound_rx) =
        tokio::sync::mpsc::unbounded_channel::<crate::ide::session::OutboundMessage>();
    let session = session.with_outbound(outbound_tx.clone());

    // Subscribe to the background source-watcher's model-changed broadcast so
    // this client re-reads `event-model.json` after a code→model sync.
    let mut model_changed_rx = state.model_changed_tx.subscribe();

    loop {
        tokio::select! {
            biased;

            // Drain outbound messages (notifications + responses) first
            // so claude's per-line stdout reaches the browser within
            // milliseconds of arriving on the server.
            out = outbound_rx.recv() => {
                let Some(out) = out else { break };
                let payload_res = match &out {
                    crate::ide::session::OutboundMessage::Notification(n) => serde_json::to_string(n),
                    crate::ide::session::OutboundMessage::Response(r) => serde_json::to_string(r),
                };
                let payload = match payload_res {
                    Ok(s) => s,
                    Err(e) => {
                        tracing::error!(error = %e, "failed to serialise outbound");
                        continue;
                    }
                };
                if let Err(e) = sink.send(Message::Text(payload)).await {
                    tracing::debug!(error = %e, session_id = %session.id, "ws send failed, closing");
                    break;
                }
            }

            msg = stream.next() => {
                let Some(msg) = msg else { break };
                let msg = match msg {
                    Ok(m) => m,
                    Err(e) => {
                        tracing::debug!(error = %e, session_id = %session.id, "ws read error, closing");
                        break;
                    }
                };

                let text = match msg {
                    Message::Text(t) => t,
                    Message::Binary(_) => continue,
                    Message::Ping(_) | Message::Pong(_) => continue,
                    Message::Close(_) => {
                        tracing::debug!(session_id = %session.id, "client closed ws");
                        break;
                    }
                };

                // Hand the frame to a per-request task so the connection
                // loop can keep draining `outbound_rx` while the handler
                // is awaiting its subprocess / network call. Each task
                // pushes its eventual Response back onto the same outbound
                // channel so this loop is the single point of WS write.
                let session_for_task = session.clone();
                let state_for_task = state.clone();
                let outbound_for_task = outbound_tx.clone();
                tokio::spawn(async move {
                    if let Some(response) =
                        handle_text_frame(&text, &session_for_task, &state_for_task).await
                    {
                        let _ = outbound_for_task
                            .send(crate::ide::session::OutboundMessage::Response(response));
                    }
                });
            }

            // The source watcher re-synced event-model.json — tell the client
            // to re-read. `session.notify` queues onto `outbound_tx`, which the
            // top (biased) arm drains on the next poll.
            changed = model_changed_rx.recv() => {
                use tokio::sync::broadcast::error::RecvError;
                match changed {
                    Ok(()) | Err(RecvError::Lagged(_)) => {
                        session.notify(
                            "$/eventModelChanged",
                            serde_json::json!({ "reason": "source files changed" }),
                        );
                    }
                    // Sender dropped (server shutting down) — stop this connection.
                    Err(RecvError::Closed) => break,
                }
            }
        }
    }
}

/// Parse + dispatch one text frame. Returns `Some(Response)` for requests,
/// `None` for notifications (which per spec must not be responded to). Parse
/// errors and Invalid Request errors yield `Some(Response { id: None, .. })`
/// per JSON-RPC 2.0 spec.
async fn handle_text_frame(
    text: &str,
    session: &crate::ide::session::Session,
    state: &AppState,
) -> Option<Response> {
    match parse_incoming(text) {
        Ok(Incoming::Request(req)) => {
            tracing::info!(
                method = %req.method,
                session_id = %session.id,
                "rpc request",
            );
            let id = req.id;
            let result = state.registry.dispatch(&req.method, session, req.params).await;
            if let Err(ref err) = result {
                tracing::warn!(
                    method = %req.method,
                    code = err.code,
                    message = %err.message,
                    "rpc request failed",
                );
            }
            Some(match result {
                Ok(value) => Response::success(id, value),
                Err(err) => Response::failure(id, err),
            })
        }
        Ok(Incoming::Notification(notif)) => {
            // No response — but log so future cancellation / progress
            // notifications are debuggable when they land.
            tracing::debug!(method = %notif.method, "notification received (no response, no handler in v1)");
            None
        }
        Err(rpc_err) => Some(Response::failure(None, rpc_err)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::methods::register_all;
    use crate::ide::registry::MethodRegistry;
    use crate::ide::session::Session;
    use crate::ide::transport::LocalTransport;
    use crate::ide::workspace::Workspace;
    use std::sync::Arc;

    fn fixture_state(dir: &std::path::Path) -> (AppState, Session) {
        let ws = Workspace::from_root(dir).unwrap();
        let transport = LocalTransport::new(ws);
        let session = transport.accept().unwrap();
        let registry = register_all(MethodRegistry::new());
        let (model_changed_tx, _) = tokio::sync::broadcast::channel(16);
        let state = AppState { registry, transport: Arc::new(transport), model_changed_tx };
        (state, session)
    }

    #[tokio::test]
    async fn handle_text_frame_initialize_returns_success_response() {
        let dir = tempfile::tempdir().unwrap();
        let (state, session) = fixture_state(dir.path());
        let frame = r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"clientInfo":{"name":"t","version":"0"}}}"#;
        let resp = handle_text_frame(frame, &session, &state).await.expect("request gets a response");
        assert!(resp.error.is_none(), "should succeed: {resp:?}");
        let result = resp.result.expect("result present");
        assert_eq!(result["serverInfo"]["name"], "neo");
    }

    #[tokio::test]
    async fn handle_text_frame_unknown_method_returns_method_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let (state, session) = fixture_state(dir.path());
        let frame = r#"{"jsonrpc":"2.0","id":7,"method":"does/not/exist"}"#;
        let resp = handle_text_frame(frame, &session, &state).await.expect("request gets a response");
        let err = resp.error.expect("error present");
        assert_eq!(err.code, crate::ide::rpc::error_codes::METHOD_NOT_FOUND);
    }

    #[tokio::test]
    async fn handle_text_frame_invalid_json_returns_parse_error_with_null_id() {
        let dir = tempfile::tempdir().unwrap();
        let (state, session) = fixture_state(dir.path());
        let resp = handle_text_frame("{garbage", &session, &state).await.expect("response on parse error");
        assert!(resp.id.is_none(), "parse-error response id must be null: {resp:?}");
        assert_eq!(resp.error.unwrap().code, crate::ide::rpc::error_codes::PARSE_ERROR);
    }

    #[tokio::test]
    async fn handle_text_frame_notification_yields_no_response() {
        let dir = tempfile::tempdir().unwrap();
        let (state, session) = fixture_state(dir.path());
        let frame = r#"{"jsonrpc":"2.0","method":"$/someEventClientSentUs"}"#;
        let resp = handle_text_frame(frame, &session, &state).await;
        assert!(resp.is_none(), "notifications do not get responses: {resp:?}");
    }

    #[tokio::test]
    async fn model_changed_emits_event_model_changed_notification() {
        // The connection loop forwards a model-changed broadcast by calling
        // `session.notify("$/eventModelChanged", …)`. Pin the exact method
        // string the frontend's reload handler subscribes to.
        let (tx, mut rx) =
            tokio::sync::mpsc::unbounded_channel::<crate::ide::session::OutboundMessage>();
        let dir = tempfile::tempdir().unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        let session = Session::new(Arc::new(ws)).with_outbound(tx);

        session.notify("$/eventModelChanged", serde_json::json!({ "reason": "source files changed" }));

        match rx.recv().await.expect("notification queued") {
            crate::ide::session::OutboundMessage::Notification(n) => {
                assert_eq!(n.method, "$/eventModelChanged");
            }
            other => panic!("expected a notification, got {other:?}"),
        }
    }
}
