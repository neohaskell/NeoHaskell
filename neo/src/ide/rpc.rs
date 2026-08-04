//! JSON-RPC 2.0 wire types.
//!
//! Hand-rolled (no `jsonrpsee`/`jsonrpc-core` dep) — the spec is small and
//! the codebase prefers minimal external surface.

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::errors::NeoError;

/// Standard JSON-RPC 2.0 error codes plus `SERVER_ERROR` for application
/// errors (`NeoError`).
pub mod error_codes {
    pub const PARSE_ERROR: i32 = -32700;
    pub const INVALID_REQUEST: i32 = -32600;
    pub const METHOD_NOT_FOUND: i32 = -32601;
    pub const INVALID_PARAMS: i32 = -32602;
    #[allow(dead_code)]
    pub const INTERNAL_ERROR: i32 = -32603;
    /// Generic application error: every `NeoError` surfaces here, with the
    /// rustc-style `code` / `help` / `url` packed into the `data` field so
    /// the frontend can render the same diagnostic the CLI does.
    pub const SERVER_ERROR: i32 = -32000;
}

/// A newtype that serialises as the string `"2.0"` and refuses any other
/// value on deserialise. Gives us a static guarantee that the wire is
/// JSON-RPC 2.0 without having to write a runtime check at every call site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct JsonRpcVersion;

impl Serialize for JsonRpcVersion {
    fn serialize<S: Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str("2.0")
    }
}

impl<'de> Deserialize<'de> for JsonRpcVersion {
    fn deserialize<D: Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        let s = String::deserialize(de)?;
        if s == "2.0" {
            Ok(JsonRpcVersion)
        } else {
            Err(serde::de::Error::custom(format!(
                "unsupported JSON-RPC version `{s}`, expected `2.0`"
            )))
        }
    }
}

/// JSON-RPC request id: either a number or a string. JSON null is represented
/// at the field level via `Option<RequestId>` (`None` ↔ `null`).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(untagged)]
pub enum RequestId {
    Number(i64),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub jsonrpc: JsonRpcVersion,
    /// Required per spec. May be JSON null — represented as `None` here.
    pub id: Option<RequestId>,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notification {
    pub jsonrpc: JsonRpcVersion,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

/// What a peer can send us. JSON-RPC 2.0 distinguishes Request vs
/// Notification by **presence of the `id` field** (not its value — `id: null`
/// is still a request). Serde's `untagged` enum can't see "key present vs
/// missing" cleanly when both variants accept missing-as-None, so we
/// implement `Deserialize` manually: peek at the object, branch on `id`'s
/// presence, then deserialize into the right variant.
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Incoming {
    Request(Request),
    Notification(Notification),
}

impl<'de> Deserialize<'de> for Incoming {
    fn deserialize<D: Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        let map: serde_json::Map<String, serde_json::Value> = Deserialize::deserialize(de)?;
        let has_id = map.contains_key("id");
        let value = serde_json::Value::Object(map);
        if has_id {
            let req = Request::deserialize(value).map_err(serde::de::Error::custom)?;
            Ok(Incoming::Request(req))
        } else {
            let notif = Notification::deserialize(value).map_err(serde::de::Error::custom)?;
            Ok(Incoming::Notification(notif))
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Response {
    pub jsonrpc: JsonRpcVersion,
    /// Required per spec; `None` serialises as JSON null (which is what the
    /// spec requires when the request id could not be parsed).
    pub id: Option<RequestId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<RpcError>,
}

impl Response {
    pub fn success(id: Option<RequestId>, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: JsonRpcVersion,
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn failure(id: Option<RequestId>, error: RpcError) -> Self {
        Self {
            jsonrpc: JsonRpcVersion,
            id,
            result: None,
            error: Some(error),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct RpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}

impl RpcError {
    pub fn new(code: i32, message: impl Into<String>) -> Self {
        Self { code, message: message.into(), data: None }
    }

    pub fn with_data(mut self, data: serde_json::Value) -> Self {
        self.data = Some(data);
        self
    }
}

/// Parse a single text frame into an `Incoming`. Distinguishes between
/// "not JSON at all" (`-32700`) and "JSON but not a valid request/notification"
/// (`-32600`). The caller is expected to wrap any returned `RpcError` in a
/// `Response { id: None, .. }` per the JSON-RPC 2.0 spec.
pub fn parse_incoming(text: &str) -> Result<Incoming, RpcError> {
    let value: serde_json::Value = serde_json::from_str(text).map_err(|e| {
        RpcError::new(
            error_codes::PARSE_ERROR,
            format!("invalid JSON: {e}"),
        )
    })?;
    serde_json::from_value(value).map_err(|e| {
        RpcError::new(
            error_codes::INVALID_REQUEST,
            format!("not a JSON-RPC 2.0 request or notification: {e}"),
        )
    })
}

/// Convert a `NeoError` into an `RpcError` keyed at the generic application
/// error code (-32000). The structured rustc-style payload is packed into
/// `data` so the frontend can render the same instruct-a-tiny-LLM diagnostic
/// the CLI does. Uses `miette::Diagnostic` to extract code/help/url — no new
/// error type, `NeoError` stays the source of truth.
pub fn serialize_neo_error(err: &NeoError) -> RpcError {
    use miette::Diagnostic;
    let message = err.to_string();
    let mut data = serde_json::Map::new();
    if let Some(code) = err.code() {
        data.insert(
            "diagnosticCode".into(),
            serde_json::Value::String(code.to_string()),
        );
    }
    if let Some(help) = err.help() {
        data.insert("help".into(), serde_json::Value::String(help.to_string()));
    }
    if let Some(url) = err.url() {
        data.insert("url".into(), serde_json::Value::String(url.to_string()));
    }
    let rpc = RpcError::new(error_codes::SERVER_ERROR, message);
    if data.is_empty() {
        rpc
    } else {
        rpc.with_data(serde_json::Value::Object(data))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn request_parses_minimal_envelope() {
        let incoming = parse_incoming(r#"{"jsonrpc":"2.0","id":1,"method":"x"}"#).unwrap();
        match incoming {
            Incoming::Request(req) => {
                assert_eq!(req.method, "x");
                assert_eq!(req.id, Some(RequestId::Number(1)));
                // params defaults to Value::Null when omitted
                assert!(req.params.is_null(), "params should default to null");
            }
            Incoming::Notification(_) => panic!("expected request, got notification"),
        }
    }

    #[test]
    fn request_parses_string_id() {
        let incoming = parse_incoming(r#"{"jsonrpc":"2.0","id":"abc","method":"x"}"#).unwrap();
        match incoming {
            Incoming::Request(req) => {
                assert_eq!(req.id, Some(RequestId::String("abc".to_string())));
            }
            _ => panic!("expected request"),
        }
    }

    #[test]
    fn request_parses_null_id() {
        let incoming = parse_incoming(r#"{"jsonrpc":"2.0","id":null,"method":"x"}"#).unwrap();
        match incoming {
            Incoming::Request(req) => assert_eq!(req.id, None),
            _ => panic!("expected request"),
        }
    }

    #[test]
    fn request_rejects_wrong_jsonrpc_version() {
        let err = parse_incoming(r#"{"jsonrpc":"1.0","id":1,"method":"x"}"#).unwrap_err();
        assert_eq!(err.code, error_codes::INVALID_REQUEST);
        assert!(
            err.message.contains("JSON-RPC"),
            "message should mention JSON-RPC: {}",
            err.message
        );
    }

    #[test]
    fn notification_parses_without_id_field() {
        let incoming = parse_incoming(r#"{"jsonrpc":"2.0","method":"ping"}"#).unwrap();
        match incoming {
            Incoming::Notification(n) => {
                assert_eq!(n.method, "ping");
            }
            Incoming::Request(_) => panic!("expected notification, got request"),
        }
    }

    #[test]
    fn notification_has_no_id_field_in_serialized_form() {
        let n = Notification {
            jsonrpc: JsonRpcVersion,
            method: "ping".to_string(),
            params: json!(null),
        };
        let s = serde_json::to_string(&n).unwrap();
        assert!(!s.contains("\"id\""), "serialized notification should omit `id`: {s}");
        assert!(s.contains("\"method\":\"ping\""), "method present: {s}");
    }

    #[test]
    fn response_serializes_result_xor_error() {
        let ok = Response::success(Some(RequestId::Number(1)), json!({"x":1}));
        let s = serde_json::to_string(&ok).unwrap();
        assert!(s.contains("\"result\""), "success should include result: {s}");
        assert!(!s.contains("\"error\""), "success should omit error: {s}");

        let err = Response::failure(
            Some(RequestId::Number(1)),
            RpcError::new(error_codes::METHOD_NOT_FOUND, "not found"),
        );
        let s = serde_json::to_string(&err).unwrap();
        assert!(!s.contains("\"result\""), "failure should omit result: {s}");
        assert!(s.contains("\"error\""), "failure should include error: {s}");
    }

    #[test]
    fn response_with_null_id_serializes_id_field_as_json_null() {
        // Per spec: id is REQUIRED in a Response; null when the request id
        // couldn't be parsed. So Option::None on the wire must be JSON null,
        // not omitted.
        let r = Response::failure(
            None,
            RpcError::new(error_codes::PARSE_ERROR, "bad json"),
        );
        let s = serde_json::to_string(&r).unwrap();
        assert!(s.contains("\"id\":null"), "id field must be present and null: {s}");
    }

    #[test]
    fn parse_error_for_invalid_json() {
        let err = parse_incoming("{garbage").unwrap_err();
        assert_eq!(err.code, error_codes::PARSE_ERROR);
        assert!(
            err.message.contains("invalid JSON"),
            "message should mention parse: {}",
            err.message,
        );
    }

    #[test]
    fn neo_error_serializes_with_code_message_help() {
        let err = NeoError::NoWorkspace;
        let rpc = serialize_neo_error(&err);
        assert_eq!(rpc.code, error_codes::SERVER_ERROR);
        assert!(rpc.message.contains("No `neo.json` found"), "message: {}", rpc.message);
        let data = rpc.data.expect("data should be present");
        assert_eq!(
            data["diagnosticCode"].as_str().unwrap(),
            "neo::no_workspace",
            "data: {data}",
        );
        assert!(data.get("help").and_then(|h| h.as_str()).is_some(), "help present: {data}");
    }

    #[test]
    fn neo_error_serializes_invalid_dep_with_full_payload() {
        let err = NeoError::InvalidDependency {
            key: "k".to_string(),
            value: "v".to_string(),
            reason: "r".to_string(),
            src: None,
            span: None,
        };
        let rpc = serialize_neo_error(&err);
        let data = rpc.data.expect("data should be present");
        assert_eq!(data["diagnosticCode"].as_str().unwrap(), "neo::invalid_dep");
        assert!(data.get("url").and_then(|u| u.as_str()).is_some());
    }

    #[test]
    fn request_id_round_trips_through_json() {
        let n: RequestId = serde_json::from_str("42").unwrap();
        assert_eq!(n, RequestId::Number(42));
        let s: RequestId = serde_json::from_str(r#""abc""#).unwrap();
        assert_eq!(s, RequestId::String("abc".to_string()));
        assert_eq!(serde_json::to_string(&RequestId::Number(42)).unwrap(), "42");
        assert_eq!(serde_json::to_string(&RequestId::String("abc".to_string())).unwrap(), r#""abc""#);
    }
}
