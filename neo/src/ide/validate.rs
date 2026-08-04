//! Schema + referential validator for `event-model.json`.
//!
//! The JSON Schema (draft 2020-12) lives at
//! `assets/ide/src/model/event-model.schema.json` and is the authoritative
//! shape — embedded into the binary via `include_str!`. The frontend's TS
//! interfaces in `assets/ide/src/model/types.ts` mirror the schema and must
//! be kept in lockstep until codegen ships (next slice — flagged in
//! `project_ide_jsonrpc_architecture.md`).
//!
//! Referential integrity (orphan edges, missing entity/chapter references)
//! is not expressible in pure JSON Schema, so we run it as a second pass
//! after schema validation passes. If schema validation fails, referential
//! checks are skipped — running them against shape-invalid data produces
//! false positives that drown the real errors.

use std::collections::HashSet;
use std::sync::OnceLock;

use jsonschema::{Validator, draft202012};
use serde::Serialize;
use serde_json::Value;

/// The raw bytes of the event-model JSON Schema, embedded at compile time.
/// Other modules (notably `heal_event_model`) inline this into the `claude -p`
/// prompt so the agent has the exact contract to work against.
pub const SCHEMA_JSON: &str =
    include_str!("../../assets/ide/src/model/event-model.schema.json");

static SCHEMA: OnceLock<Validator> = OnceLock::new();

fn schema() -> &'static Validator {
    SCHEMA.get_or_init(|| {
        let parsed: Value = serde_json::from_str(SCHEMA_JSON).unwrap_or_else(|e| {
            panic!(
                "embedded event-model schema is not valid JSON: {e}. This is a build-time invariant violation — the schema at assets/ide/src/model/event-model.schema.json must parse."
            )
        });
        draft202012::new(&parsed).unwrap_or_else(|e| {
            panic!(
                "embedded event-model schema is not a valid JSON Schema draft 2020-12 document: {e}. Fix assets/ide/src/model/event-model.schema.json."
            )
        })
    })
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "camelCase", rename_all_fields = "camelCase", tag = "status")]
pub enum ValidationOutcome {
    /// File did not exist on disk. The caller is `read_event_model`; the
    /// validator itself never produces this — exposed here so the read
    /// handler can return one `validation` shape for every outcome.
    NotFound,
    Valid,
    Invalid { errors: Vec<ValidationError> },
    MalformedJson { parse_error: String },
}

impl ValidationOutcome {
    /// Convenience constructor for the read-handler's file-missing path.
    pub fn not_found() -> Self {
        ValidationOutcome::NotFound
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ValidationError {
    /// JSON Pointer (RFC 6901) to the offending location. Empty string means
    /// the whole document.
    pub pointer: String,
    /// Human-readable message written so a small LLM can act on it without
    /// re-reading any docs (per `feedback_error_message_invariant`).
    pub message: String,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ErrorKind {
    Schema,
    ReferentialIntegrity,
}

/// Validate `raw` (the raw bytes of `event-model.json`).
///
/// Returns `MalformedJson` if the bytes don't parse as JSON, `Invalid` with
/// a list of schema and/or referential errors, or `Valid`. Error order is
/// stable: errors are sorted by `(pointer, message)` so callers can
/// assert on the exact sequence in tests.
pub fn validate_event_model(raw: &str) -> ValidationOutcome {
    let value: Value = match serde_json::from_str(raw) {
        Ok(v) => v,
        Err(e) => {
            return ValidationOutcome::MalformedJson {
                parse_error: e.to_string(),
            };
        }
    };

    let validator = schema();
    let mut errors: Vec<ValidationError> = validator
        .iter_errors(&value)
        .map(|e| ValidationError {
            pointer: e.instance_path.as_str().to_string(),
            message: e.to_string(),
            kind: ErrorKind::Schema,
        })
        .collect();

    if errors.is_empty() {
        errors.extend(referential_integrity_errors(&value));
    }

    if errors.is_empty() {
        ValidationOutcome::Valid
    } else {
        errors.sort_by(|a, b| {
            a.pointer.cmp(&b.pointer).then(a.message.cmp(&b.message))
        });
        ValidationOutcome::Invalid { errors }
    }
}

fn referential_integrity_errors(value: &Value) -> Vec<ValidationError> {
    // value is known to satisfy the schema, so the structural assumptions
    // below (object root, required arrays present) are guaranteed.
    let obj = value
        .as_object()
        .expect("schema guarantees object root");
    let nodes = obj["nodes"]
        .as_array()
        .expect("schema guarantees nodes is array");
    let entities = obj["entities"]
        .as_array()
        .expect("schema guarantees entities is array");
    let chapters = obj["chapters"]
        .as_array()
        .expect("schema guarantees chapters is array");
    let slices = obj["slices"]
        .as_array()
        .expect("schema guarantees slices is array");
    let edges = obj["edges"]
        .as_array()
        .expect("schema guarantees edges is array");

    let node_ids: HashSet<&str> = nodes
        .iter()
        .filter_map(|n| n.get("id").and_then(Value::as_str))
        .collect();
    let entity_ids: HashSet<&str> = entities
        .iter()
        .filter_map(|e| e.get("id").and_then(Value::as_str))
        .collect();
    let chapter_ids: HashSet<&str> = chapters
        .iter()
        .filter_map(|c| c.get("id").and_then(Value::as_str))
        .collect();

    let mut errors = Vec::new();

    for (i, edge) in edges.iter().enumerate() {
        let edge_id = edge.get("id").and_then(Value::as_str).unwrap_or("?");
        if let Some(src) = edge.get("sourceId").and_then(Value::as_str)
            && !node_ids.contains(src)
        {
            errors.push(ValidationError {
                pointer: format!("/edges/{i}/sourceId"),
                message: format!(
                    "Edge `{edge_id}`: `sourceId` references node `{src}`, which is not in the `nodes` array. Fix: add a node with `id` = `{src}` to `nodes`, OR change `sourceId` to an existing node id, OR delete this edge."
                ),
                kind: ErrorKind::ReferentialIntegrity,
            });
        }
        if let Some(tgt) = edge.get("targetId").and_then(Value::as_str)
            && !node_ids.contains(tgt)
        {
            errors.push(ValidationError {
                pointer: format!("/edges/{i}/targetId"),
                message: format!(
                    "Edge `{edge_id}`: `targetId` references node `{tgt}`, which is not in the `nodes` array. Fix: add a node with `id` = `{tgt}` to `nodes`, OR change `targetId` to an existing node id, OR delete this edge."
                ),
                kind: ErrorKind::ReferentialIntegrity,
            });
        }
    }

    for (i, node) in nodes.iter().enumerate() {
        let node_id = node.get("id").and_then(Value::as_str).unwrap_or("?");
        if let Some(eid_value) = node.get("entityId")
            && let Some(eid) = eid_value.as_str()
            && !entity_ids.contains(eid)
        {
            errors.push(ValidationError {
                pointer: format!("/nodes/{i}/entityId"),
                message: format!(
                    "Node `{node_id}`: `entityId` references entity `{eid}`, which is not in the `entities` array. Fix: add an entity with `id` = `{eid}` to `entities`, OR change `entityId` to an existing entity id, OR set `entityId` to null."
                ),
                kind: ErrorKind::ReferentialIntegrity,
            });
        }
    }

    for (i, slice) in slices.iter().enumerate() {
        let slice_id = slice.get("id").and_then(Value::as_str).unwrap_or("?");
        if let Some(cid_value) = slice.get("chapterId")
            && let Some(cid) = cid_value.as_str()
            && !chapter_ids.contains(cid)
        {
            errors.push(ValidationError {
                pointer: format!("/slices/{i}/chapterId"),
                message: format!(
                    "Slice `{slice_id}`: `chapterId` references chapter `{cid}`, which is not in the `chapters` array. Fix: add a chapter with `id` = `{cid}` to `chapters`, OR change `chapterId` to an existing chapter id, OR set `chapterId` to null."
                ),
                kind: ErrorKind::ReferentialIntegrity,
            });
        }
    }

    // `submodels` is an OPTIONAL top-level grouping (a submodel = a feature
    // that owns one or more chapters, stacked vertically). Older files predate
    // it, so treat a missing array as empty. Every `chapter.submodelId` that is
    // a string must reference an existing submodel.
    let submodel_ids: HashSet<&str> = obj
        .get("submodels")
        .and_then(Value::as_array)
        .map(|arr| {
            arr.iter()
                .filter_map(|s| s.get("id").and_then(Value::as_str))
                .collect()
        })
        .unwrap_or_default();

    for (i, chapter) in chapters.iter().enumerate() {
        let chapter_id = chapter.get("id").and_then(Value::as_str).unwrap_or("?");
        if let Some(sid_value) = chapter.get("submodelId")
            && let Some(sid) = sid_value.as_str()
            && !submodel_ids.contains(sid)
        {
            errors.push(ValidationError {
                pointer: format!("/chapters/{i}/submodelId"),
                message: format!(
                    "Chapter `{chapter_id}`: `submodelId` references submodel `{sid}`, which is not in the `submodels` array. Fix: add a submodel with `id` = `{sid}` to `submodels`, OR change `submodelId` to an existing submodel id, OR set `submodelId` to null."
                ),
                kind: ErrorKind::ReferentialIntegrity,
            });
        }
    }

    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn minimal_valid() -> Value {
        json!({
            "id": "m1",
            "name": "Demo",
            "chapters": [],
            "entities": [],
            "slices": [],
            "nodes": [],
            "edges": [],
            "layout": {
                "nodePositions": {},
                "viewport": { "x": 0.0, "y": 0.0, "zoom": 1.0 }
            }
        })
    }

    fn invalid_errors(outcome: ValidationOutcome) -> Vec<ValidationError> {
        match outcome {
            ValidationOutcome::Invalid { errors } => errors,
            other => panic!("expected Invalid, got {other:?}"),
        }
    }

    #[test]
    fn schema_compiles_at_runtime() {
        // Force first init; will panic with a clear message if the embedded
        // schema is malformed. A subsequent call returns the cached validator.
        let _ = schema();
    }

    #[test]
    fn valid_minimal_model_passes() {
        let raw = serde_json::to_string(&minimal_valid()).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn valid_full_model_passes() {
        let model = json!({
            "id": "m1",
            "name": "Full",
            "chapters": [{"id": "c1", "name": "C", "order": 0}],
            "entities": [{"id": "e1", "name": "E", "order": 0}],
            "slices": [{"id": "s1", "name": "S", "chapterId": "c1", "order": 0}],
            "nodes": [
                {"id": "n1", "type": "event", "name": "Ev", "entityId": "e1", "sliceId": "s1"},
                {"id": "n2", "type": "command", "name": "Cmd", "entityId": null, "sliceId": null},
                {"id": "n3", "type": "query", "name": "Q", "sliceId": null},
                {"id": "n4", "type": "integration", "name": "I", "kind": "inbound", "sliceId": null},
                {"id": "n5", "type": "uiPlaceholder", "name": "UI", "sliceId": null}
            ],
            "edges": [
                {"id": "ed1", "type": "commandProducesEvent", "sourceId": "n2", "targetId": "n1"},
                {"id": "ed2", "type": "eventFeedsQuery", "sourceId": "n1", "targetId": "n3"},
                {"id": "ed3", "type": "eventTriggersIntegration", "sourceId": "n1", "targetId": "n4"},
                {"id": "ed4", "type": "integrationTriggersCommand", "sourceId": "n4", "targetId": "n2"},
                {"id": "ed5", "type": "commandFromUI", "sourceId": "n5", "targetId": "n2"},
                {"id": "ed6", "type": "queryToUI", "sourceId": "n3", "targetId": "n5"}
            ],
            "layout": {
                "nodePositions": {"n1": {"x": 0.0, "y": 0.0}},
                "viewport": {"x": 0.0, "y": 0.0, "zoom": 1.0}
            }
        });
        let raw = serde_json::to_string(&model).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn valid_model_with_submodel_and_chapter_membership_passes() {
        let mut m = minimal_valid();
        m["submodels"] = json!([{"id": "sm1", "name": "Checkout", "order": 0}]);
        m["chapters"] = json!([{"id": "c1", "name": "C", "order": 0, "submodelId": "sm1"}]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn valid_model_with_bysubmodel_layout_passes() {
        // Per-feature node-position overrides (Features-as-pages free drag).
        let mut m = minimal_valid();
        m["layout"]["bySubmodel"] =
            json!({ "sm1": { "n1": { "x": 1.0, "y": 2.0 } }, "__ungrouped__": { "n2": { "x": 3.0, "y": 4.0 } } });
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn valid_node_with_fields_passes() {
        // A node carrying optional schema `fields` validates (semantic zoom /
        // Schema lens). `fields` is additive; absence is also valid (covered by
        // the minimal/full model tests above).
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {
                "id": "n1", "type": "command", "name": "PlaceOrder",
                "entityId": null, "sliceId": null,
                "fields": [
                    {"name": "orderId", "type": "UUID"},
                    {"name": "total", "type": "Money"}
                ]
            }
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn invalid_node_fields_rejected() {
        // `fields` must be an array of {name, type}; a bare string is rejected.
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "query", "name": "Q", "sliceId": null, "fields": "nope"}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errors = invalid_errors(validate_event_model(&raw));
        assert!(!errors.is_empty(), "expected a schema error for non-array fields");
    }

    #[test]
    fn invalid_node_field_missing_type_rejected() {
        // Each field requires both `name` and `type`.
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "query", "name": "Q", "sliceId": null,
             "fields": [{"name": "orderId"}]}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errors = invalid_errors(validate_event_model(&raw));
        assert!(!errors.is_empty(), "expected a schema error for a field missing `type`");
    }

    #[test]
    fn model_without_submodels_array_is_valid_backcompat() {
        // Files authored before submodels existed have no `submodels` key and
        // no `submodelId` on chapters — they must keep validating.
        let mut m = minimal_valid();
        m["chapters"] = json!([{"id": "c1", "name": "C", "order": 0}]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn referential_chapter_unknown_submodel_fails() {
        let mut m = minimal_valid();
        m["submodels"] = json!([{"id": "sm1", "name": "Checkout", "order": 0}]);
        m["chapters"] = json!([{"id": "c1", "name": "C", "order": 0, "submodelId": "ghost"}]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(
            errs.iter().any(|e| e.kind == ErrorKind::ReferentialIntegrity
                && e.pointer == "/chapters/0/submodelId"
                && e.message.contains("ghost")),
            "expected a referential error for the dangling submodelId, got: {errs:?}"
        );
    }

    #[test]
    fn referential_chapter_null_submodel_ok() {
        let mut m = minimal_valid();
        m["submodels"] = json!([]);
        m["chapters"] = json!([{"id": "c1", "name": "C", "order": 0, "submodelId": null}]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn missing_required_root_field_id_fails() {
        let mut m = minimal_valid();
        m.as_object_mut().unwrap().remove("id");
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(
            errs.iter().any(|e| e.kind == ErrorKind::Schema && e.message.contains("id")),
            "expected an error naming `id`, got: {errs:?}"
        );
    }

    #[test]
    fn missing_required_root_field_nodes_fails() {
        let mut m = minimal_valid();
        m.as_object_mut().unwrap().remove("nodes");
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(
            errs.iter().any(|e| e.kind == ErrorKind::Schema && e.message.contains("nodes")),
            "expected an error naming `nodes`, got: {errs:?}"
        );
    }

    #[test]
    fn additional_root_property_fails() {
        let mut m = minimal_valid();
        m.as_object_mut().unwrap().insert("foo".into(), json!(1));
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(
            errs.iter().any(|e| e.kind == ErrorKind::Schema),
            "expected at least one schema error, got: {errs:?}"
        );
    }

    #[test]
    fn node_with_unknown_type_fails() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "frobnicate", "name": "X", "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(errs.iter().any(|e| e.kind == ErrorKind::Schema));
    }

    #[test]
    fn event_node_missing_entity_id_fails() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "event", "name": "Ev", "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(errs.iter().any(|e| e.kind == ErrorKind::Schema));
    }

    #[test]
    fn integration_node_invalid_kind_fails() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "integration", "name": "I", "kind": "sideways", "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(errs.iter().any(|e| e.kind == ErrorKind::Schema));
    }

    #[test]
    fn edge_with_unknown_type_fails() {
        let mut m = minimal_valid();
        m["edges"] = json!([
            {"id": "e1", "type": "magic", "sourceId": "x", "targetId": "y"}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(errs.iter().any(|e| e.kind == ErrorKind::Schema));
    }

    #[test]
    fn empty_string_input_returns_malformed_json() {
        match validate_event_model("") {
            ValidationOutcome::MalformedJson { .. } => {}
            other => panic!("expected MalformedJson, got {other:?}"),
        }
    }

    #[test]
    fn whitespace_only_input_returns_malformed_json() {
        match validate_event_model("   \n  ") {
            ValidationOutcome::MalformedJson { .. } => {}
            other => panic!("expected MalformedJson, got {other:?}"),
        }
    }

    #[test]
    fn non_object_root_returns_schema_error() {
        let errs = invalid_errors(validate_event_model("[]"));
        assert!(errs.iter().any(|e| e.kind == ErrorKind::Schema));
    }

    #[test]
    fn null_root_returns_schema_error() {
        let errs = invalid_errors(validate_event_model("null"));
        assert!(errs.iter().any(|e| e.kind == ErrorKind::Schema));
    }

    #[test]
    fn truncated_json_returns_malformed_json_with_position() {
        match validate_event_model("{\"id\":") {
            ValidationOutcome::MalformedJson { parse_error } => {
                assert!(
                    parse_error.contains("line") || parse_error.contains("column"),
                    "expected position info, got: {parse_error}"
                );
            }
            other => panic!("expected MalformedJson, got {other:?}"),
        }
    }

    #[test]
    fn referential_orphan_edge_source_fails() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "event", "name": "Ev", "entityId": null, "sliceId": null}
        ]);
        m["edges"] = json!([
            {"id": "ed1", "type": "commandProducesEvent", "sourceId": "missing", "targetId": "n1"}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].kind, ErrorKind::ReferentialIntegrity);
        assert!(errs[0].message.contains("missing"));
    }

    #[test]
    fn referential_orphan_edge_target_fails() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "command", "name": "Cmd", "entityId": null, "sliceId": null}
        ]);
        m["edges"] = json!([
            {"id": "ed1", "type": "commandProducesEvent", "sourceId": "n1", "targetId": "missing"}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].kind, ErrorKind::ReferentialIntegrity);
        assert!(errs[0].message.contains("missing"));
    }

    #[test]
    fn referential_node_unknown_entity_fails() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "event", "name": "Ev", "entityId": "ghost", "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].kind, ErrorKind::ReferentialIntegrity);
        assert!(errs[0].message.contains("ghost"));
    }

    #[test]
    fn referential_slice_unknown_chapter_fails() {
        let mut m = minimal_valid();
        m["slices"] = json!([
            {"id": "s1", "name": "S", "chapterId": "ghost", "order": 0}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].kind, ErrorKind::ReferentialIntegrity);
        assert!(errs[0].message.contains("ghost"));
    }

    #[test]
    fn referential_node_null_entity_ok() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "event", "name": "Ev", "entityId": null, "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn multiple_errors_all_reported() {
        // Three schema errors at distinct pointers — referential pass skipped
        // because schema already failed.
        let m = json!({
            "id": "m1",
            "name": "X",
            "chapters": [],
            "entities": [],
            "slices": [],
            "nodes": [
                {"id": "n1", "type": "unknown1", "name": "x", "sliceId": null},
                {"id": "n2", "type": "unknown2", "name": "x", "sliceId": null}
            ],
            "edges": [
                {"id": "ed1", "type": "unknownEdge", "sourceId": "n1", "targetId": "n2"}
            ],
            "layout": {
                "nodePositions": {},
                "viewport": {"x": 0.0, "y": 0.0, "zoom": 1.0}
            }
        });
        let raw = serde_json::to_string(&m).unwrap();
        let errs = invalid_errors(validate_event_model(&raw));
        assert!(errs.len() >= 3, "expected at least 3 errors, got {}: {errs:?}", errs.len());
    }

    #[test]
    fn unicode_node_name_passes() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "event", "name": "日本語", "entityId": null, "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn id_collision_currently_allowed() {
        // v1: duplicate node ids are not flagged. Documented behaviour;
        // promote to a referential rule when callers need it.
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "dup", "type": "event", "name": "A", "entityId": null, "sliceId": null},
            {"id": "dup", "type": "event", "name": "B", "entityId": null, "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        assert_eq!(validate_event_model(&raw), ValidationOutcome::Valid);
    }

    #[test]
    fn validate_is_deterministic() {
        let mut m = minimal_valid();
        m["nodes"] = json!([
            {"id": "n1", "type": "event", "name": "Ev", "entityId": "ghost1", "sliceId": null},
            {"id": "n2", "type": "command", "name": "Cmd", "entityId": "ghost2", "sliceId": null}
        ]);
        let raw = serde_json::to_string(&m).unwrap();
        let a = validate_event_model(&raw);
        let b = validate_event_model(&raw);
        assert_eq!(a, b);
    }
}
