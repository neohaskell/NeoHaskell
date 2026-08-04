//! The JSON-RPC method registry.
//!
//! `MethodRegistry::register` is the typed-handler shim: it accepts a
//! `fn(&Session, P) -> Future<Result<R, NeoError>>` (concrete `P`/`R` types,
//! no `serde_json::Value` in the handler signature) and stores it as a
//! type-erased boxed-future closure keyed by method name. The
//! `dispatch` path does the JSON marshalling, the `NeoError`-to-`RpcError`
//! conversion, and the `method-not-found` / `invalid-params` error codes.
//!
//! Handlers are arbitrarily nestable async fns. No `async-trait` dep — we
//! hand-roll the `Pin<Box<dyn Future>>` plumbing.

use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use serde::{de::DeserializeOwned, Serialize};

use crate::errors::NeoError;
use crate::ide::rpc::{error_codes, serialize_neo_error, RpcError};
use crate::ide::session::Session;

/// The fully-erased handler stored in the registry. Each call receives an
/// owned `Session` (cheap to clone — it's an `Arc<Workspace>` + small id +
/// timestamp) and the raw `params: Value`, and yields a
/// `Result<Value, RpcError>` ready to wrap into a `Response`.
///
/// Why owned `Session` rather than `&Session`: it lets `async fn` handlers
/// have `'static` return futures without the higher-ranked-trait-bound
/// gymnastics that `&Session` requires.
type ErasedHandler = Arc<
    dyn Fn(
            Session,
            serde_json::Value,
        ) -> Pin<
            Box<dyn Future<Output = Result<serde_json::Value, RpcError>> + Send + 'static>,
        > + Send
        + Sync,
>;

#[derive(Default, Clone)]
pub struct MethodRegistry {
    handlers: Arc<HashMap<String, ErasedHandler>>,
}

impl MethodRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a typed async handler under `method`. Returns a new
    /// registry; callers chain `.register(...).register(...)` so the registry
    /// itself stays `Arc`-cheap-to-clone after build.
    pub fn register<P, R, F, Fut>(mut self, method: &str, handler: F) -> Self
    where
        P: DeserializeOwned + Send + 'static,
        R: Serialize + 'static,
        F: Fn(Session, P) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<R, NeoError>> + Send + 'static,
    {
        let handler = Arc::new(handler);
        let erased: ErasedHandler = Arc::new(move |session, params| {
            let handler = handler.clone();
            Box::pin(async move {
                let typed: P = serde_json::from_value(params).map_err(|e| {
                    RpcError::new(
                        error_codes::INVALID_PARAMS,
                        format!("invalid params: {e}"),
                    )
                    .with_data(serde_json::json!({
                        "serdeError": e.to_string(),
                    }))
                })?;
                let result = handler(session, typed).await.map_err(|err| serialize_neo_error(&err))?;
                serde_json::to_value(result).map_err(|e| {
                    RpcError::new(
                        error_codes::INTERNAL_ERROR,
                        format!("failed to serialise response: {e}"),
                    )
                })
            })
        });
        Arc::make_mut(&mut self.handlers).insert(method.to_string(), erased);
        self
    }

    /// Dispatch a single request. The caller passes the per-connection
    /// `Session` by reference; it's cloned into the dispatched future so
    /// the future is `'static`. Returns the value to put in
    /// `Response.result`, or the `RpcError` to put in `Response.error`.
    pub async fn dispatch(
        &self,
        method: &str,
        session: &Session,
        params: serde_json::Value,
    ) -> Result<serde_json::Value, RpcError> {
        let handler = self.handlers.get(method).ok_or_else(|| {
            RpcError::new(
                error_codes::METHOD_NOT_FOUND,
                format!("method `{method}` is not registered"),
            )
        })?;
        handler(session.clone(), params).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::workspace::Workspace;
    use serde::{Deserialize, Serialize};
    use std::sync::Arc;

    fn test_session() -> Session {
        let dir = tempfile::tempdir().unwrap();
        let ws = Workspace::from_root(dir.path()).unwrap();
        Session::new(Arc::new(ws))
    }

    #[derive(Deserialize)]
    struct EchoParams {
        x: i64,
    }

    #[derive(Serialize, Debug, PartialEq)]
    struct EchoResult {
        x: i64,
        doubled: i64,
    }

    #[tokio::test]
    async fn register_then_dispatch_typed_handler() {
        let registry = MethodRegistry::new().register(
            "echo",
            |_session: Session, p: EchoParams| async move {
                Ok::<_, NeoError>(EchoResult { x: p.x, doubled: p.x * 2 })
            },
        );
        let session = test_session();
        let result = registry
            .dispatch("echo", &session, serde_json::json!({"x": 21}))
            .await
            .expect("dispatch should succeed");
        assert_eq!(result["x"], 21);
        assert_eq!(result["doubled"], 42);
    }

    #[tokio::test]
    async fn dispatch_unknown_method_yields_method_not_found() {
        let registry = MethodRegistry::new();
        let session = test_session();
        let err = registry
            .dispatch("nope", &session, serde_json::Value::Null)
            .await
            .unwrap_err();
        assert_eq!(err.code, error_codes::METHOD_NOT_FOUND);
        assert!(err.message.contains("`nope`"), "message names the method: {}", err.message);
    }

    #[tokio::test]
    async fn dispatch_bad_params_yields_invalid_params() {
        let registry = MethodRegistry::new().register(
            "echo",
            |_session: Session, p: EchoParams| async move {
                Ok::<_, NeoError>(EchoResult { x: p.x, doubled: 0 })
            },
        );
        let session = test_session();
        let err = registry
            .dispatch("echo", &session, serde_json::json!({"x": "not a number"}))
            .await
            .unwrap_err();
        assert_eq!(err.code, error_codes::INVALID_PARAMS);
        let data = err.data.expect("data should carry the serde error");
        assert!(
            data["serdeError"].as_str().unwrap().contains("number"),
            "serde error should mention the type mismatch: {data}",
        );
    }

    #[tokio::test]
    async fn dispatch_propagates_neo_error_as_server_error() {
        let registry = MethodRegistry::new().register(
            "fails",
            |_session: Session, _p: EchoParams| async move {
                Err::<EchoResult, NeoError>(NeoError::NoWorkspace)
            },
        );
        let session = test_session();
        let err = registry
            .dispatch("fails", &session, serde_json::json!({"x": 1}))
            .await
            .unwrap_err();
        assert_eq!(err.code, error_codes::SERVER_ERROR);
        let data = err.data.expect("NeoError data should be present");
        assert_eq!(data["diagnosticCode"].as_str().unwrap(), "neo::no_workspace");
    }

    #[tokio::test]
    async fn registry_is_clone_cheap_and_shareable() {
        // Compile-time + behaviour: cloning the registry yields a handle that
        // sees the same handlers. This matters because axum will clone the
        // AppState into every connection task.
        let registry = MethodRegistry::new().register(
            "echo",
            |_session: Session, p: EchoParams| async move {
                Ok::<_, NeoError>(EchoResult { x: p.x, doubled: p.x * 2 })
            },
        );
        let r2 = registry.clone();
        let session = test_session();
        let result = r2
            .dispatch("echo", &session, serde_json::json!({"x": 7}))
            .await
            .unwrap();
        assert_eq!(result["doubled"], 14);
    }
}
