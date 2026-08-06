//! Code→model sync: refresh `event-model.json` from the parsed Haskell source.
//!
//! This is the single engine shared by BOTH the CLI (`neo inspect sync`) and the
//! `neo ide` background watcher — there is exactly one read/diff/apply/write path
//! so the two can never drift.
//!
//! One-way (code→model): the IDE never authors fields; source code is the
//! canonical author. Writing fields *back* into Haskell source (model→code
//! codegen) is explicitly out of scope (future work).
//!
//! Layout policy — the user's rule "editing fields of an EXISTING node is a DATA
//! sync, never a layout sync":
//!   * If the source introduced a genuinely NEW node/slice/entity/chapter, run
//!     the FULL heal so the new structure gets a position.
//!   * Otherwise apply only the data-only pass (field reconcile + any new edges
//!     / integration-kind fixes on existing nodes) with ZERO layout movement —
//!     no relayout, no wave reorder, no band reflow, no position changes.
//!
//! Deterministic + idempotent: a second call on an already-synced project
//! rewrites nothing (`applied == 0`, file left byte-identical).
//!
//! Concurrency: writes are atomic (write-tmp-then-rename), and — consistent with
//! `workspace/writeEventModel`'s documented v1 stance — last-writer-wins against
//! a concurrent IDE autosave. Atomic rename means no torn files; a field update
//! lost to a race re-applies on the next source change.

use std::path::Path;

use crate::errors::NeoError;
use crate::ide::heal::apply::apply_diff;
use crate::ide::heal::diff::{compute_diff_with_options, ComputeOptions};
use crate::ide::methods::read_event_model::EVENT_MODEL_FILENAME;

/// What a `sync_event_model` run did.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyncOutcome {
    /// Total apply operations performed. `0` ⇒ the model already matched the
    /// code and the file on disk is untouched.
    pub applied: usize,
    /// How many nodes had their `fields` (re)written from source.
    pub fields_updated: usize,
    /// `true` when a NEW node/slice/entity/chapter appeared in source, so the
    /// FULL heal (structural + layout) ran instead of the data-only field pass.
    pub ran_full_heal: bool,
}

/// Sync `event-model.json` at `workspace_root` from the project source code.
/// See the module docs for the one-way + layout policy.
pub fn sync_event_model(workspace_root: &Path) -> Result<SyncOutcome, NeoError> {
    let path = workspace_root.join(EVENT_MODEL_FILENAME);
    let original = std::fs::read_to_string(&path)
        .map_err(|e| NeoError::io_at("reading `event-model.json` for sync", path.clone(), e))?;
    let mut model: serde_json::Value = serde_json::from_str(&original).map_err(|e| {
        NeoError::HealingFailed {
            reason: format!(
                "sync requires a valid event-model.json — JSON parse failed: {e}. Open `{}`, fix the \
                 syntax (a trailing comma, an unclosed brace/quote), save, and re-run `neo inspect sync`. \
                 If it is beyond repair, delete it and recreate the model with `neo ide`.",
                path.display()
            ),
            stderr_tail: String::new(),
        }
    })?;

    let inspection = crate::inspect::inspect_project(workspace_root);

    // Probe with the structural-only options: does the source introduce NEW
    // structure (a node/slice/entity/chapter) that would need laying out?
    let probe = compute_diff_with_options(&model, &inspection, ComputeOptions::structural_only());
    let needs_layout = !probe.add_nodes.is_empty()
        || !probe.add_slices.is_empty()
        || !probe.add_entities.is_empty()
        || !probe.add_chapters.is_empty();

    let diff = if needs_layout {
        // New structure ⇒ full heal so the new nodes get positions.
        compute_diff_with_options(&model, &inspection, ComputeOptions::full())
    } else {
        // No new structure ⇒ apply the data-only probe directly: field
        // reconcile + any new edges / kind fixes on existing nodes, ZERO layout.
        probe
    };

    let fields_updated = diff.set_node_fields.len();
    let applied = apply_diff(&mut model, &diff);

    if applied > 0 {
        let new_content = serde_json::to_string_pretty(&model).map_err(|e| {
            NeoError::HealingFailed {
                reason: format!("sync could not re-serialise event-model.json: {e}"),
                stderr_tail: String::new(),
            }
        })?;
        atomic_write(&path, &new_content)?;
    }

    Ok(SyncOutcome { applied, fields_updated, ran_full_heal: needs_layout })
}

/// Atomic write: tmp-then-rename so a crash mid-write can't truncate the file.
/// The tmp lives in the same directory so the rename stays on one filesystem.
fn atomic_write(path: &Path, content: &str) -> Result<(), NeoError> {
    let tmp = path.with_extension("json.sync-tmp");
    std::fs::write(&tmp, content.as_bytes())
        .map_err(|e| NeoError::io_at("writing synced event-model.json (tmp)", tmp.clone(), e))?;
    std::fs::rename(&tmp, path).map_err(|e| {
        let _ = std::fs::remove_file(&tmp);
        NeoError::io_at(
            "renaming synced event-model.json into place",
            path.to_path_buf(),
            e,
        )
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    fn write(root: &Path, rel: &str, body: &str) {
        let p = root.join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, body).unwrap();
    }

    fn read_model(root: &Path) -> serde_json::Value {
        serde_json::from_str(&std::fs::read_to_string(root.join("event-model.json")).unwrap()).unwrap()
    }

    #[test]
    fn sync_event_model_missing_file_errors_actionably() {
        let dir = tempfile::tempdir().unwrap();
        let err = sync_event_model(dir.path()).unwrap_err();
        let rendered = format!("{err}");
        assert!(
            rendered.contains("event-model.json"),
            "error must name the file: {rendered}"
        );
    }

    #[test]
    fn sync_event_model_writes_event_fields_without_moving_layout() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        write(
            root,
            "src/App/Cart/Core.hs",
            "module App.Cart.Core where\n\
             data CartEvent = ItemAdded { stockId :: Uuid, quantity :: Int } deriving (Generic)\n",
        );
        std::fs::create_dir_all(root.join("src/App/Cart/Commands")).unwrap();
        // Model already has the ItemAdded event node, positioned, no fields.
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "ent", "name": "Cart", "order": 0 }],
            "slices": [{ "id": "s1", "name": "ItemAdded", "chapterId": null, "order": 0 }],
            "nodes": [{ "id": "ev1", "type": "event", "name": "ItemAdded", "sliceId": "s1", "entityId": "ent" }],
            "edges": [],
            "layout": { "nodePositions": { "ev1": { "x": 123, "y": 400 } }, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        std::fs::write(root.join("event-model.json"), serde_json::to_string_pretty(&model).unwrap()).unwrap();

        let outcome = sync_event_model(root).unwrap();
        assert!(outcome.fields_updated >= 1, "the event node's fields should sync");

        let after = read_model(root);
        let ev = after["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "ItemAdded").unwrap();
        let names: Vec<&str> = ev["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
        assert_eq!(names, vec!["stockId", "quantity"]);
        // Position preserved exactly — a field edit moves nothing.
        assert_eq!(after["layout"]["nodePositions"]["ev1"]["x"], 123.0);
        assert_eq!(after["layout"]["nodePositions"]["ev1"]["y"], 400.0);
    }

    #[test]
    fn sync_event_model_is_idempotent() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        write(
            root,
            "src/App/Cart/Core.hs",
            "module App.Cart.Core where\ndata CartEvent = ItemAdded { stockId :: Uuid } deriving (Generic)\n",
        );
        std::fs::create_dir_all(root.join("src/App/Cart/Commands")).unwrap();
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "ent", "name": "Cart", "order": 0 }],
            "slices": [{ "id": "s1", "name": "ItemAdded", "chapterId": null, "order": 0 }],
            "nodes": [{ "id": "ev1", "type": "event", "name": "ItemAdded", "sliceId": "s1", "entityId": "ent" }],
            "edges": [],
            "layout": { "nodePositions": { "ev1": { "x": 123, "y": 400 } }, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        std::fs::write(root.join("event-model.json"), serde_json::to_string_pretty(&model).unwrap()).unwrap();

        let first = sync_event_model(root).unwrap();
        assert!(first.applied > 0, "first sync applies the field write");
        let after_first = std::fs::read_to_string(root.join("event-model.json")).unwrap();

        let second = sync_event_model(root).unwrap();
        assert_eq!(second.applied, 0, "second sync must be a fixed point");
        assert_eq!(
            std::fs::read_to_string(root.join("event-model.json")).unwrap(),
            after_first,
            "second sync must leave the file byte-identical",
        );
    }

    #[test]
    fn sync_event_model_full_heal_when_source_adds_node() {
        // Source has a command + event the model lacks ⇒ a NEW node appears ⇒
        // the full heal runs and materialises it WITH a position and fields.
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        write(
            root,
            "src/App/Cart/Core.hs",
            "module App.Cart.Core where\ndata CartEvent = ItemAdded { stockId :: Uuid } deriving (Generic)\n",
        );
        write(
            root,
            "src/App/Cart/Commands/AddItem.hs",
            "module App.Cart.Commands.AddItem where\n\
             data AddItem = AddItem { stockId :: Uuid }\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        );
        let model = serde_json::json!({
            "id": "m", "name": "demo", "chapters": [], "entities": [], "slices": [],
            "nodes": [], "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        std::fs::write(root.join("event-model.json"), serde_json::to_string_pretty(&model).unwrap()).unwrap();

        let outcome = sync_event_model(root).unwrap();
        assert!(outcome.ran_full_heal, "a new node must trigger the full heal path");

        let after = read_model(root);
        let nodes = after["nodes"].as_array().unwrap();
        let cmd = nodes.iter().find(|n| n["name"] == "AddItem").expect("AddItem materialised");
        let cmd_id = cmd["id"].as_str().unwrap();
        assert!(
            after["layout"]["nodePositions"].get(cmd_id).is_some(),
            "a materialised node must get a layout position",
        );
        let names: Vec<&str> = cmd["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
        assert_eq!(names, vec!["stockId"], "the new command's fields sync too");
    }
}
