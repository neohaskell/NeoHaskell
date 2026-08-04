//! Apply a `HealDiff` to a model JSON value in place. Pure and deterministic:
//! same input model + same diff yields byte-identical output. New entity /
//! slice / node / edge IDs are derived from name-keyed hashes so re-running
//! the pass against an already-patched model is a no-op.

use serde_json::{json, Value};

use super::diff::HealDiff;

/// Mutate `model` to apply every action in `diff`. Returns the number of
/// operations actually performed (some may have been preempted by races —
/// e.g. an edge added between `compute_diff` and `apply_diff`).
pub fn apply_diff(model: &mut Value, diff: &HealDiff) -> usize {
    let mut applied = 0;

    applied += apply_add_chapters(model, diff);
    applied += apply_remove_chapters(model, diff);
    applied += apply_add_entities(model, diff);
    applied += apply_remove_slices(model, diff);
    applied += apply_add_slices(model, diff);
    applied += apply_add_nodes(model, diff);
    applied += apply_set_node_fields(model, diff);
    applied += apply_slice_updates(model, diff);
    applied += apply_kind_fixes(model, diff);
    applied += apply_position_fixes(model, diff);
    applied += apply_layout_entries(model, diff);
    applied += apply_add_edges(model, diff);
    applied += apply_remove_edges(model, diff);

    applied
}

fn apply_add_chapters(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.add_chapters.is_empty() {
        return 0;
    }
    let chapters = ensure_array(model, "chapters");
    let mut applied = 0;
    for ch in &diff.add_chapters {
        let exists_by_id = chapters
            .iter()
            .any(|c| c.get("id").and_then(|v| v.as_str()) == Some(ch.id.as_str()));
        let exists_by_name = chapters
            .iter()
            .any(|c| c.get("name").and_then(|v| v.as_str()) == Some(ch.name.as_str()));
        if exists_by_id || exists_by_name {
            continue;
        }
        chapters.push(json!({
            "id": ch.id,
            "name": ch.name,
            "order": ch.order,
        }));
        applied += 1;
    }
    applied
}

fn apply_remove_chapters(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.remove_chapters.is_empty() {
        return 0;
    }
    let Some(chapters) = model.get_mut("chapters").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let to_remove: std::collections::BTreeSet<&str> =
        diff.remove_chapters.iter().map(|s| s.as_str()).collect();
    let before = chapters.len();
    chapters.retain(|c| {
        c.get("id")
            .and_then(|v| v.as_str())
            .map(|id| !to_remove.contains(id))
            .unwrap_or(true)
    });
    before - chapters.len()
}

/// Remove slices named in `diff.remove_slices`. Double-gated: a slice is
/// dropped only when its id starts with `slice-heal-`, so a user-authored
/// slice is never removed even if the diff asked for it. Mirrors
/// `apply_remove_chapters`.
fn apply_remove_slices(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.remove_slices.is_empty() {
        return 0;
    }
    let Some(slices) = model.get_mut("slices").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let to_remove: std::collections::BTreeSet<&str> = diff
        .remove_slices
        .iter()
        .filter(|id| id.starts_with("slice-heal-"))
        .map(|s| s.as_str())
        .collect();
    let before = slices.len();
    slices.retain(|s| {
        s.get("id")
            .and_then(|v| v.as_str())
            .map(|id| !to_remove.contains(id))
            .unwrap_or(true)
    });
    before - slices.len()
}

fn apply_slice_updates(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.update_slices.is_empty() {
        return 0;
    }
    let Some(slices) = model.get_mut("slices").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let mut applied = 0;
    for update in &diff.update_slices {
        for s in slices.iter_mut() {
            if s.get("id").and_then(|v| v.as_str()) != Some(update.slice_id.as_str()) {
                continue;
            }
            let Some(obj) = s.as_object_mut() else {
                continue;
            };
            let mut touched = false;
            if let Some(ch) = &update.set_chapter_id {
                obj.insert("chapterId".to_string(), Value::String(ch.clone()));
                touched = true;
            }
            if let Some(o) = update.set_order {
                obj.insert("order".to_string(), json!(o));
                touched = true;
            }
            if touched {
                applied += 1;
            }
            break;
        }
    }
    applied
}

fn apply_add_entities(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.add_entities.is_empty() {
        return 0;
    }
    let entities = ensure_array(model, "entities");
    let mut applied = 0;
    for ent in &diff.add_entities {
        let exists_by_id = entities
            .iter()
            .any(|e| e.get("id").and_then(|v| v.as_str()) == Some(ent.id.as_str()));
        let exists_by_name = entities
            .iter()
            .any(|e| e.get("name").and_then(|v| v.as_str()) == Some(ent.name.as_str()));
        if exists_by_id || exists_by_name {
            continue;
        }
        entities.push(json!({
            "id": ent.id,
            "name": ent.name,
            "order": ent.order,
        }));
        applied += 1;
    }
    applied
}

fn apply_add_slices(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.add_slices.is_empty() {
        return 0;
    }
    let slices = ensure_array(model, "slices");
    let mut applied = 0;
    for sl in &diff.add_slices {
        let exists_by_id = slices
            .iter()
            .any(|s| s.get("id").and_then(|v| v.as_str()) == Some(sl.id.as_str()));
        let exists_by_name = slices
            .iter()
            .any(|s| s.get("name").and_then(|v| v.as_str()) == Some(sl.name.as_str()));
        if exists_by_id || exists_by_name {
            continue;
        }
        let chapter_id = match &sl.chapter_id {
            Some(id) => Value::String(id.clone()),
            None => Value::Null,
        };
        slices.push(json!({
            "id": sl.id,
            "name": sl.name,
            "chapterId": chapter_id,
            "order": sl.order,
        }));
        applied += 1;
    }
    applied
}

fn apply_add_nodes(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.add_nodes.is_empty() {
        return 0;
    }
    let nodes = ensure_array(model, "nodes");
    let mut applied = 0;
    for n in &diff.add_nodes {
        let exists_by_id = nodes
            .iter()
            .any(|x| x.get("id").and_then(|v| v.as_str()) == Some(n.id.as_str()));
        let exists_by_type_and_name = nodes.iter().any(|x| {
            x.get("type").and_then(|v| v.as_str()) == Some(n.node_type.as_str())
                && x.get("name").and_then(|v| v.as_str()) == Some(n.name.as_str())
        });
        if exists_by_id || exists_by_type_and_name {
            continue;
        }
        let mut obj = serde_json::Map::new();
        obj.insert("id".to_string(), Value::String(n.id.clone()));
        obj.insert("type".to_string(), Value::String(n.node_type.clone()));
        obj.insert("name".to_string(), Value::String(n.name.clone()));
        obj.insert("sliceId".to_string(), Value::String(n.slice_id.clone()));
        // Schema-driven field set per node type. CommandNode + EventNode
        // require `entityId` (nullable). QueryNode + IntegrationNode +
        // UIPlaceholderNode do NOT carry entityId. IntegrationNode requires
        // `kind`.
        match n.node_type.as_str() {
            "command" | "event" => {
                let v = match &n.entity_id {
                    Some(id) => Value::String(id.clone()),
                    None => Value::Null,
                };
                obj.insert("entityId".to_string(), v);
            }
            "integration" => {
                if let Some(kind) = &n.kind {
                    obj.insert("kind".to_string(), Value::String(kind.clone()));
                }
            }
            _ => {}
        }
        nodes.push(Value::Object(obj));
        applied += 1;
    }
    applied
}

/// Overwrite each targeted node's `fields` array from the parsed source. Runs
/// AFTER `apply_add_nodes` so freshly-materialised nodes already exist. Pure
/// data — touches only `node.fields`, never positions/slices/edges.
fn apply_set_node_fields(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.set_node_fields.is_empty() {
        return 0;
    }
    let Some(nodes) = model.get_mut("nodes").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let mut applied = 0;
    for set in &diff.set_node_fields {
        for node in nodes.iter_mut() {
            if node.get("id").and_then(|v| v.as_str()) != Some(set.node_id.as_str()) {
                continue;
            }
            if let Some(obj) = node.as_object_mut() {
                let fields: Vec<Value> = set
                    .fields
                    .iter()
                    .map(|f| json!({ "name": f.name, "type": f.type_name }))
                    .collect();
                obj.insert("fields".to_string(), Value::Array(fields));
                applied += 1;
            }
            break;
        }
    }
    applied
}

fn apply_kind_fixes(model: &mut Value, diff: &HealDiff) -> usize {
    let Some(nodes) = model.get_mut("nodes").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let mut applied = 0;
    for fix in &diff.fix_integration_kinds {
        for node in nodes.iter_mut() {
            if node.get("id").and_then(|v| v.as_str()) == Some(fix.node_id.as_str()) {
                if let Some(obj) = node.as_object_mut() {
                    obj.insert("kind".to_string(), Value::String(fix.to_kind.clone()));
                    applied += 1;
                }
                break;
            }
        }
    }
    applied
}

fn apply_position_fixes(model: &mut Value, diff: &HealDiff) -> usize {
    let Some(positions) = model
        .pointer_mut("/layout/nodePositions")
        .and_then(|v| v.as_object_mut())
    else {
        return 0;
    };
    let mut applied = 0;
    for fix in &diff.fix_positions {
        if let Some(entry) = positions.get_mut(&fix.node_id).and_then(|v| v.as_object_mut())
        {
            let mut touched = false;
            if let Some(to_y) = fix.to_y {
                entry.insert("y".to_string(), json!(to_y));
                touched = true;
            }
            if let Some(to_x) = fix.to_x {
                entry.insert("x".to_string(), json!(to_x));
                touched = true;
            }
            if touched {
                applied += 1;
            }
        }
    }
    applied
}

fn apply_layout_entries(model: &mut Value, diff: &HealDiff) -> usize {
    ensure_layout_skeleton(model);
    let Some(positions) = model
        .pointer_mut("/layout/nodePositions")
        .and_then(|v| v.as_object_mut())
    else {
        return 0;
    };
    let mut applied = 0;
    for entry in &diff.ensure_layout_entries {
        // Only insert if missing — apply_position_fixes handles existing
        // entries with wrong y values.
        if !positions.contains_key(&entry.node_id) {
            positions.insert(
                entry.node_id.clone(),
                json!({ "x": entry.x, "y": entry.y }),
            );
            applied += 1;
        }
    }
    applied
}

fn apply_add_edges(model: &mut Value, diff: &HealDiff) -> usize {
    if !model.is_object() {
        return 0;
    }
    if model.get("edges").is_none() {
        model.as_object_mut().unwrap().insert("edges".to_string(), json!([]));
    }
    let Some(edges) = model.get_mut("edges").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let mut applied = 0;
    for edge in &diff.add_edges {
        let edge_id = synth_edge_id(&edge.edge_type, &edge.source_id, &edge.target_id);
        // Idempotency guard: skip if an edge with same (type, source, target) already exists.
        let exists = edges.iter().any(|e| {
            e.get("type").and_then(|v| v.as_str()) == Some(edge.edge_type.as_str())
                && e.get("sourceId").and_then(|v| v.as_str()) == Some(edge.source_id.as_str())
                && e.get("targetId").and_then(|v| v.as_str()) == Some(edge.target_id.as_str())
        });
        if exists {
            continue;
        }
        edges.push(json!({
            "id": edge_id,
            "type": edge.edge_type,
            "sourceId": edge.source_id,
            "targetId": edge.target_id,
            "sourceHandle": edge.source_handle,
            "targetHandle": edge.target_handle,
        }));
        applied += 1;
    }
    applied
}

/// Remove edges named in `diff.remove_edges`. Double-gated: an edge is dropped
/// only when its `(type, source, target)` is requested AND its `id` starts
/// with `edge-heal-` — so a user-authored edge (different id scheme) survives
/// even if the diff asked for that triple. Mirrors `apply_remove_chapters`.
fn apply_remove_edges(model: &mut Value, diff: &HealDiff) -> usize {
    if diff.remove_edges.is_empty() {
        return 0;
    }
    let Some(edges) = model.get_mut("edges").and_then(|v| v.as_array_mut()) else {
        return 0;
    };
    let to_remove: std::collections::BTreeSet<(&str, &str, &str)> = diff
        .remove_edges
        .iter()
        .map(|e| (e.edge_type.as_str(), e.source_id.as_str(), e.target_id.as_str()))
        .collect();
    let before = edges.len();
    edges.retain(|e| {
        let id = e.get("id").and_then(|v| v.as_str()).unwrap_or("");
        if !id.starts_with("edge-heal-") {
            return true; // user-authored: never removed
        }
        let t = e.get("type").and_then(|v| v.as_str()).unwrap_or("");
        let s = e.get("sourceId").and_then(|v| v.as_str()).unwrap_or("");
        let tg = e.get("targetId").and_then(|v| v.as_str()).unwrap_or("");
        !to_remove.contains(&(t, s, tg))
    });
    before - edges.len()
}

fn ensure_array<'a>(model: &'a mut Value, key: &str) -> &'a mut Vec<Value> {
    let obj = model.as_object_mut().expect("model must be a JSON object");
    obj.entry(key.to_string()).or_insert_with(|| json!([]));
    obj.get_mut(key)
        .and_then(|v| v.as_array_mut())
        .expect("just-inserted array must be a JSON array")
}

fn ensure_layout_skeleton(model: &mut Value) {
    let Some(obj) = model.as_object_mut() else {
        return;
    };
    let layout = obj
        .entry("layout".to_string())
        .or_insert_with(|| json!({}));
    if let Some(layout_obj) = layout.as_object_mut() {
        layout_obj
            .entry("nodePositions".to_string())
            .or_insert_with(|| json!({}));
        layout_obj
            .entry("viewport".to_string())
            .or_insert_with(|| json!({ "x": 0, "y": 0, "zoom": 1 }));
    }
}

/// Deterministic edge id derived from the edge's content. The same diff
/// applied twice yields the same id, which keeps the patched file stable
/// across re-runs and avoids spurious diffs in git.
fn synth_edge_id(edge_type: &str, source_id: &str, target_id: &str) -> String {
    use std::hash::{DefaultHasher, Hash, Hasher};
    let mut h = DefaultHasher::new();
    edge_type.hash(&mut h);
    source_id.hash(&mut h);
    target_id.hash(&mut h);
    format!("edge-heal-{:016x}", h.finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::heal::diff::{
        compute_diff, EdgeRef, EdgeToAdd, EntityToAdd, HealDiff, KindFix, LayoutEntry, NodeToAdd,
        PositionFix, SliceToAdd,
    };
    use crate::inspect::{
        CommandInfo, DomainInspection, EventInfo, IntegrationInfo, IntegrationKind,
        ProjectInspection, QueryInfo,
    };
    use std::path::PathBuf;

    fn minimal_model() -> Value {
        json!({
            "id": "m1",
            "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Order", "order": 0 }],
            "slices": [
                { "id": "sl1", "name": "PlaceOrder", "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "cmd1", "type": "command", "name": "PlaceOrder",  "sliceId": "sl1", "entityId": "ent1" },
                { "id": "ev1",  "type": "event",   "name": "OrderPlaced", "sliceId": "sl1", "entityId": "ent1" }
            ],
            "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    fn empty_model() -> Value {
        json!({
            "id": "m1",
            "name": "demo",
            "chapters": [],
            "entities": [],
            "slices": [],
            "nodes": [],
            "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    fn fixture_inspection() -> ProjectInspection {
        ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![DomainInspection {
                name: "Orders".to_string(),
                path: PathBuf::from("/Orders"),
                events: vec![EventInfo {
                    name: "OrderPlaced".to_string(),
                    file: PathBuf::new(),
                    fields: vec![],
                }],
                commands: vec![CommandInfo {
                    name: "PlaceOrder".to_string(),
                    file: PathBuf::new(),
                    produces: vec!["OrderPlaced".to_string()],
                    via_web_transport: false,
                    fields: vec![],
                }],
                queries: vec![QueryInfo {
                    name: "OrderSummary".to_string(),
                    file: PathBuf::new(),
                    subscribes_to: vec!["OrderPlaced".to_string()],
                    ..Default::default()
                }],
                integrations: vec![IntegrationInfo {
                    name: "Notifier".to_string(),
                    file: PathBuf::new(),
                    kind: IntegrationKind::Outbound,
                    handles_events: vec!["OrderPlaced".to_string()],
                    emits_commands: vec![],
                }],
            }],
        }
    }

    #[test]
    fn apply_adds_edge_with_deterministic_id() {
        let mut model = minimal_model();
        let diff = HealDiff {
            add_edges: vec![EdgeToAdd {
                edge_type: "commandProducesEvent".to_string(),
                source_id: "cmd1".to_string(),
                target_id: "ev1".to_string(),
                source_handle: "bottom".to_string(),
                target_handle: "top".to_string(),
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let edges = model["edges"].as_array().unwrap();
        assert_eq!(edges.len(), 1);
        let id_first_run = edges[0]["id"].as_str().unwrap().to_string();

        // Re-running on the SAME starting model should produce the SAME id.
        let mut model2 = minimal_model();
        apply_diff(&mut model2, &diff);
        let id_second_run = model2["edges"][0]["id"].as_str().unwrap();
        assert_eq!(id_first_run, id_second_run, "edge ids must be deterministic");
    }

    #[test]
    fn apply_is_idempotent_on_existing_edges() {
        let mut model = minimal_model();
        let diff = HealDiff {
            add_edges: vec![EdgeToAdd {
                edge_type: "commandProducesEvent".to_string(),
                source_id: "cmd1".to_string(),
                target_id: "ev1".to_string(),
                source_handle: "bottom".to_string(),
                target_handle: "top".to_string(),
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let before = model["edges"].as_array().unwrap().len();
        apply_diff(&mut model, &diff);
        let after = model["edges"].as_array().unwrap().len();
        assert_eq!(before, after, "applying same diff twice should be a no-op");
    }

    fn model_with_efq_edges() -> Value {
        let mut model = minimal_model();
        model["nodes"].as_array_mut().unwrap().push(json!({
            "id": "qy1", "type": "query", "name": "OrderView", "sliceId": "sl1"
        }));
        model["edges"] = json!([
            // heal-authored event→query edge
            { "id": "edge-heal-deadbeef00000001", "type": "eventFeedsQuery", "sourceId": "ev1", "targetId": "qy1", "sourceHandle": "right", "targetHandle": "left" },
            // user-authored event→query edge with the SAME (type,src,tgt)-ish but its own id scheme
            { "id": "edge-user-0001", "type": "eventFeedsQuery", "sourceId": "ev1", "targetId": "qy1", "sourceHandle": "right", "targetHandle": "left" }
        ]);
        model
    }

    #[test]
    fn apply_remove_edges_drops_matching_heal_edges() {
        let mut model = model_with_efq_edges();
        let diff = HealDiff {
            remove_edges: vec![EdgeRef {
                edge_type: "eventFeedsQuery".to_string(),
                source_id: "ev1".to_string(),
                target_id: "qy1".to_string(),
                reason: String::new(),
            }],
            ..Default::default()
        };
        let removed = apply_diff(&mut model, &diff);
        assert_eq!(removed, 1, "exactly the heal-authored edge removed");
        let ids: Vec<&str> = model["edges"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| e["id"].as_str().unwrap())
            .collect();
        assert_eq!(ids, vec!["edge-user-0001"], "user edge survives, heal edge gone");
    }

    #[test]
    fn remove_edges_preserves_user_authored_edge() {
        // Remove an edge that exists ONLY as a user-authored one ⇒ no-op.
        let mut model = minimal_model();
        model["nodes"].as_array_mut().unwrap().push(json!({
            "id": "qy1", "type": "query", "name": "OrderView", "sliceId": "sl1"
        }));
        model["edges"] = json!([
            { "id": "edge-user-xyz", "type": "eventFeedsQuery", "sourceId": "ev1", "targetId": "qy1" }
        ]);
        let diff = HealDiff {
            remove_edges: vec![EdgeRef {
                edge_type: "eventFeedsQuery".to_string(),
                source_id: "ev1".to_string(),
                target_id: "qy1".to_string(),
                reason: String::new(),
            }],
            ..Default::default()
        };
        let removed = apply_diff(&mut model, &diff);
        assert_eq!(removed, 0, "user-authored edge must never be removed");
        assert_eq!(model["edges"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn remove_edges_is_idempotent() {
        let mut model = model_with_efq_edges();
        let diff = HealDiff {
            remove_edges: vec![EdgeRef {
                edge_type: "eventFeedsQuery".to_string(),
                source_id: "ev1".to_string(),
                target_id: "qy1".to_string(),
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let removed_again = apply_diff(&mut model, &diff);
        assert_eq!(removed_again, 0, "second removal is a no-op");
    }

    #[test]
    fn apply_writes_kind_fix() {
        let mut model = minimal_model();
        model["nodes"].as_array_mut().unwrap().push(json!({
            "id": "intg1", "type": "integration", "name": "Bridge",
            "sliceId": "sl1", "kind": "outbound"
        }));
        let diff = HealDiff {
            fix_integration_kinds: vec![KindFix {
                node_id: "intg1".to_string(),
                node_name: "Bridge".to_string(),
                from_kind: "outbound".to_string(),
                to_kind: "inbound".to_string(),
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let kind = model["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n["id"] == "intg1")
            .unwrap()["kind"]
            .as_str()
            .unwrap();
        assert_eq!(kind, "inbound");
    }

    #[test]
    fn apply_writes_position_fix() {
        let mut model = minimal_model();
        model["layout"]["nodePositions"]["cmd1"] = json!({ "x": 40, "y": 400 });
        let diff = HealDiff {
            fix_positions: vec![PositionFix {
                node_id: "cmd1".to_string(),
                node_name: "PlaceOrder".to_string(),
                node_kind: "command".to_string(),
                from_y: Some(400.0),
                to_y: Some(120.0),
                from_x: None,
                to_x: None,
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let pos = &model["layout"]["nodePositions"]["cmd1"];
        assert_eq!(pos["y"].as_f64(), Some(120.0));
        assert_eq!(pos["x"].as_f64(), Some(40.0), "x must be preserved");
    }

    #[test]
    fn apply_writes_layout_entry_when_missing() {
        let mut model = minimal_model();
        let diff = HealDiff {
            ensure_layout_entries: vec![LayoutEntry {
                node_id: "cmd1".to_string(),
                x: 40.0,
                y: 120.0,
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        assert_eq!(
            model["layout"]["nodePositions"]["cmd1"],
            json!({ "x": 40.0, "y": 120.0 })
        );
    }

    #[test]
    fn apply_does_not_overwrite_existing_layout_entries() {
        let mut model = minimal_model();
        model["layout"]["nodePositions"]["cmd1"] = json!({ "x": 999, "y": 999 });
        let diff = HealDiff {
            ensure_layout_entries: vec![LayoutEntry {
                node_id: "cmd1".to_string(),
                x: 40.0,
                y: 120.0,
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        assert_eq!(
            model["layout"]["nodePositions"]["cmd1"],
            json!({ "x": 999, "y": 999 }),
            "ensureLayoutEntries must not overwrite existing positions",
        );
    }

    #[test]
    fn apply_inserts_entity_slice_node_into_empty_model() {
        let mut model = empty_model();
        let diff = HealDiff {
            add_entities: vec![EntityToAdd {
                id: "ent-new".to_string(),
                name: "Orders".to_string(),
                order: 0.0,
                reason: String::new(),
            }],
            add_slices: vec![SliceToAdd {
                id: "sl-new".to_string(),
                name: "PlaceOrder".to_string(),
                chapter_id: None,
                order: 0.0,
                reason: String::new(),
            }],
            add_nodes: vec![NodeToAdd {
                id: "node-new".to_string(),
                node_type: "command".to_string(),
                name: "PlaceOrder".to_string(),
                slice_id: "sl-new".to_string(),
                entity_id: Some("ent-new".to_string()),
                kind: None,
                reason: String::new(),
            }],
            ..Default::default()
        };
        let applied = apply_diff(&mut model, &diff);
        assert!(applied >= 3, "expected at least 3 ops applied, got {applied}");
        assert_eq!(
            model["entities"][0],
            json!({ "id": "ent-new", "name": "Orders", "order": 0.0 })
        );
        assert_eq!(
            model["slices"][0],
            json!({ "id": "sl-new", "name": "PlaceOrder", "chapterId": null, "order": 0.0 })
        );
        assert_eq!(
            model["nodes"][0],
            json!({
                "id": "node-new",
                "type": "command",
                "name": "PlaceOrder",
                "sliceId": "sl-new",
                "entityId": "ent-new",
            })
        );
    }

    #[test]
    fn apply_does_not_duplicate_existing_entity_by_name() {
        let mut model = empty_model();
        model["entities"] = json!([{ "id": "ent-orig", "name": "Orders", "order": 0 }]);
        let diff = HealDiff {
            add_entities: vec![EntityToAdd {
                id: "ent-new".to_string(),
                name: "Orders".to_string(),
                order: 1.0,
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let entities = model["entities"].as_array().unwrap();
        assert_eq!(entities.len(), 1, "should not duplicate; got {:?}", entities);
        assert_eq!(entities[0]["id"].as_str().unwrap(), "ent-orig");
    }

    #[test]
    fn apply_does_not_duplicate_existing_node_by_type_and_name() {
        let mut model = minimal_model();
        let before = model["nodes"].as_array().unwrap().len();
        let diff = HealDiff {
            add_nodes: vec![NodeToAdd {
                id: "node-dupe".to_string(),
                node_type: "command".to_string(),
                name: "PlaceOrder".to_string(),
                slice_id: "sl1".to_string(),
                entity_id: Some("ent1".to_string()),
                kind: None,
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let after = model["nodes"].as_array().unwrap().len();
        assert_eq!(before, after, "node already present — must not duplicate");
    }

    #[test]
    fn apply_writes_integration_node_with_kind() {
        let mut model = empty_model();
        model["slices"] = json!([{ "id": "sl-i", "name": "Notifier", "chapterId": null, "order": 0 }]);
        let diff = HealDiff {
            add_nodes: vec![NodeToAdd {
                id: "intg-new".to_string(),
                node_type: "integration".to_string(),
                name: "Notifier".to_string(),
                slice_id: "sl-i".to_string(),
                entity_id: None,
                kind: Some("outbound".to_string()),
                reason: String::new(),
            }],
            ..Default::default()
        };
        apply_diff(&mut model, &diff);
        let n = &model["nodes"][0];
        assert_eq!(n["type"].as_str(), Some("integration"));
        assert_eq!(n["kind"].as_str(), Some("outbound"));
        assert!(n.get("entityId").is_none(), "integration must not carry entityId");
    }

    /// End-to-end: compute_diff + apply_diff on a fixture leaves the model
    /// in a state where compute_diff returns no further proposals.
    #[test]
    fn pipeline_is_fixed_point() {
        let mut model = minimal_model();
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![DomainInspection {
                name: "Orders".to_string(),
                path: PathBuf::from("/Orders"),
                events: vec![EventInfo {
                    name: "OrderPlaced".to_string(),
                    file: PathBuf::new(),
                    fields: vec![],
                }],
                commands: vec![CommandInfo {
                    name: "PlaceOrder".to_string(),
                    file: PathBuf::new(),
                    produces: vec!["OrderPlaced".to_string()],
                    via_web_transport: false,
                    fields: vec![],
                }],
                queries: vec![],
                integrations: vec![],
            }],
        };

        let diff = compute_diff(&model, &inspection);
        apply_diff(&mut model, &diff);

        let diff_after = compute_diff(&model, &inspection);
        assert!(
            diff_after.add_edges.is_empty(),
            "second compute_diff should propose no further edges; got {:?}",
            diff_after.add_edges,
        );
        assert!(
            diff_after.add_nodes.is_empty(),
            "second compute_diff should propose no further nodes; got {:?}",
            diff_after.add_nodes,
        );
        assert!(
            diff_after.add_slices.is_empty(),
            "second compute_diff should propose no further slices; got {:?}",
            diff_after.add_slices,
        );
        assert!(
            diff_after.add_entities.is_empty(),
            "second compute_diff should propose no further entities; got {:?}",
            diff_after.add_entities,
        );
    }

    /// Apply against an empty model + full inspection materialises the
    /// entire structure in one pass, and re-applying yields no-op.
    #[test]
    fn pipeline_materialises_full_structure_from_empty_model() {
        let mut model = empty_model();
        let inspection = fixture_inspection();

        let diff = compute_diff(&model, &inspection);
        let applied = apply_diff(&mut model, &diff);
        assert!(applied > 0, "first apply should do work");

        // Validation: an entity now exists, slices for every code symbol,
        // nodes wired with edges.
        assert!(!model["entities"].as_array().unwrap().is_empty());
        assert!(!model["slices"].as_array().unwrap().is_empty());
        let nodes = model["nodes"].as_array().unwrap();
        assert!(nodes.iter().any(|n| n["name"] == "PlaceOrder" && n["type"] == "command"));
        assert!(nodes.iter().any(|n| n["name"] == "OrderPlaced" && n["type"] == "event"));
        assert!(nodes.iter().any(|n| n["name"] == "OrderSummary" && n["type"] == "query"));
        assert!(nodes.iter().any(|n| n["name"] == "Notifier" && n["type"] == "integration"));
        let edges = model["edges"].as_array().unwrap();
        assert!(
            edges
                .iter()
                .any(|e| e["type"] == "commandProducesEvent"),
            "missing commandProducesEvent edge in {edges:?}"
        );
        assert!(edges.iter().any(|e| e["type"] == "eventFeedsQuery"));
        assert!(edges.iter().any(|e| e["type"] == "eventTriggersIntegration"));

        // Re-applying does nothing.
        let diff_after = compute_diff(&model, &inspection);
        let reapplied = apply_diff(&mut model, &diff_after);
        assert_eq!(
            reapplied, 0,
            "re-applying after a clean pass should be a no-op; diff={diff_after:?}",
        );
    }

    // Suppress the unused-import warning when nothing in this test module
    // happens to use these. They're load-bearing on the named integration
    // test below — keep them imported.
    fn _imports_used() -> (IntegrationInfo, IntegrationKind, QueryInfo) {
        (
            IntegrationInfo {
                name: String::new(),
                file: PathBuf::new(),
                kind: IntegrationKind::Outbound,
                handles_events: vec![],
                emits_commands: vec![],
            },
            IntegrationKind::Outbound,
            QueryInfo {
                name: String::new(),
                file: PathBuf::new(),
                subscribes_to: vec![],
                ..Default::default()
            },
        )
    }
}
