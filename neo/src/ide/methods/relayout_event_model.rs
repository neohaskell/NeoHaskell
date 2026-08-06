//! `workspace/relayoutEventModel` — re-run the deterministic LAYOUT pass
//! of the heal pipeline against `event-model.json` without touching the
//! file's structure.
//!
//! This is the "I just want my positions cleaned up" entry point. It runs:
//!   * Y-band position fixes (snaps off-band nodes back to canonical y)
//!   * Chapter grouping (groups heal-prefixed slices by their entity)
//!   * X-axis slice-column rebalance (pushes colliding columns apart)
//!   * Missing layout entries (assigns a position to any node lacking one)
//!
//! It does NOT:
//!   * Add entities / slices / nodes / edges from the inspection
//!   * Detect orphans
//!   * Fix integration kinds
//!   * Spawn `claude` — pure Rust, no LLM
//!
//! Use cases:
//!   * Quick UI button: "Re-layout" — clean up positions without the
//!     full heal workflow / confirmations.
//!   * Hand-authored event-model.json from a non-NeoHaskell workspace
//!     where the user just wants the canonical layout.

use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::errors::NeoError;
use crate::ide::heal::apply::apply_diff;
use crate::ide::heal::diff::{compute_diff_with_options, ComputeOptions};
use crate::ide::methods::read_event_model::EVENT_MODEL_FILENAME;
use crate::ide::session::Session;
use crate::ide::validate;

#[derive(Debug, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct RelayoutEventModelParams {}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct RelayoutEventModelResult {
    /// Number of deterministic layout fixes applied to the file. `0`
    /// means the file's layout was already canonical — the file on disk
    /// is unchanged.
    pub applied: usize,
    /// Short human-readable summary of what changed.
    pub summary: String,
}

pub async fn handle(
    session: Session,
    _params: RelayoutEventModelParams,
) -> Result<RelayoutEventModelResult, NeoError> {
    let path = session.workspace.root.join(EVENT_MODEL_FILENAME);
    let original = std::fs::read_to_string(&path).map_err(|e| {
        NeoError::io_at("reading `event-model.json` for relayout", path.clone(), e)
    })?;
    let mut value: serde_json::Value = serde_json::from_str(&original).map_err(|e| {
        NeoError::HealingFailed {
            reason: format!("relayout requires valid JSON; parse failed: {e}"),
            stderr_tail: String::new(),
        }
    })?;
    let inspection = crate::inspect::inspect_project(&session.workspace.root);

    let diff = compute_diff_with_options(&value, &inspection, ComputeOptions::layout_only());
    let applied = apply_diff(&mut value, &diff);
    let summary = diff.summary();

    tracing::info!(applied, %summary, "relayout: deterministic layout pass complete");

    if applied > 0 {
        let new_content = serde_json::to_string_pretty(&value).map_err(|e| {
            NeoError::HealingFailed {
                reason: format!("relayout could not re-serialise model: {e}"),
                stderr_tail: String::new(),
            }
        })?;
        atomic_write(&path, &new_content)?;
        // Quick post-validate so we never write a broken file.
        let _ = validate::validate_event_model(&new_content);
    }

    Ok(RelayoutEventModelResult { applied, summary })
}

fn atomic_write(path: &Path, content: &str) -> Result<(), NeoError> {
    let tmp = path.with_extension("json.relayout-tmp");
    std::fs::write(&tmp, content.as_bytes()).map_err(|e| {
        NeoError::io_at("writing relayouted event-model.json (tmp)", tmp.clone(), e)
    })?;
    std::fs::rename(&tmp, path).map_err(|e| {
        let _ = std::fs::remove_file(&tmp);
        NeoError::io_at(
            "renaming relayouted event-model.json into place",
            path.to_path_buf(),
            e,
        )
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ide::workspace::Workspace;
    use std::sync::Arc;

    fn fixture_session(dir: &Path) -> Session {
        let ws = Workspace::from_root(dir).unwrap();
        Session::new(Arc::new(ws))
    }

    #[tokio::test]
    async fn relayout_only_fixes_positions_does_not_materialize() {
        // Workspace has a NeoHaskell project (so a heal would materialize
        // nodes). But the relayout call MUST leave node/slice/entity
        // counts unchanged — only layout-level fields move.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let core = workspace.join("src/App/Cart/Core.hs");
        std::fs::create_dir_all(core.parent().unwrap()).unwrap();
        std::fs::write(
            &core,
            "module App.Cart.Core where\ndata CartEvent = ItemAdded {} deriving (Generic)\n",
        )
        .unwrap();
        let cmd = workspace.join("src/App/Cart/Commands/AddItem.hs");
        std::fs::create_dir_all(cmd.parent().unwrap()).unwrap();
        std::fs::write(
            &cmd,
            "module App.Cart.Commands.AddItem where\n\
             decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
        )
        .unwrap();

        // Plant a valid model with an integration at the wrong y. Relayout
        // must snap it back to the canonical band.
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Cart", "order": 0 }],
            "slices": [{ "id": "sl1", "name": "Stale", "chapterId": null, "order": 0 }],
            "nodes": [
                { "id": "intg1", "type": "integration", "name": "Misplaced",
                  "sliceId": "sl1", "kind": "outbound" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": { "intg1": { "x": 200, "y": 500 } },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        let model_path = workspace.join("event-model.json");
        std::fs::write(
            &model_path,
            serde_json::to_string_pretty(&model).unwrap(),
        )
        .unwrap();

        let session = fixture_session(workspace);
        let result = handle(session, RelayoutEventModelParams {}).await.unwrap();
        assert!(result.applied > 0, "relayout should fix the off-band y; summary={}", result.summary);

        // File should now have the integration at y=120 — but no new
        // nodes (relayout doesn't materialize AddItem / ItemAdded).
        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let nodes = patched["nodes"].as_array().unwrap();
        assert_eq!(
            nodes.len(),
            1,
            "relayout must not add nodes; got {:?}",
            nodes
        );
        let y = patched["layout"]["nodePositions"]["intg1"]["y"].as_f64().unwrap();
        assert!((y - 120.0).abs() < f64::EPSILON, "integration y should snap to 120; got {y}");
    }

    #[tokio::test]
    async fn relayout_orders_slices_by_wave_and_is_fixed_point() {
        // A spaghetti model: stored slice order (First=0, Second=1, Third=2)
        // is the REVERSE of the causal flow. The flow is Third(initializer
        // command) -> Second(integration) -> First(triggered command), wired
        // by an eventTriggersIntegration + integrationTriggersCommand chain.
        // Relayout must reorder by the wave (Third < Second < First) and be a
        // fixed point on the second call. No NeoHaskell project in the
        // workspace, so only the layout/wave pass runs (no materialisation).
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "s1", "name": "First",  "chapterId": null, "order": 0 },
                { "id": "s2", "name": "Second", "chapterId": null, "order": 1 },
                { "id": "s3", "name": "Third",  "chapterId": null, "order": 2 }
            ],
            "nodes": [
                { "id": "c0", "type": "command", "name": "Initiate",  "sliceId": "s3", "entityId": "e" },
                { "id": "e0", "type": "event",   "name": "Initiated", "sliceId": "s3", "entityId": "e" },
                { "id": "i0", "type": "integration", "name": "Bridge", "sliceId": "s2", "kind": "inbound" },
                { "id": "c1", "type": "command", "name": "Continue",  "sliceId": "s1", "entityId": "e" },
                { "id": "e1", "type": "event",   "name": "Continued", "sliceId": "s1", "entityId": "e" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent",       "sourceId": "c0", "targetId": "e0" },
                { "id": "x2", "type": "eventTriggersIntegration",   "sourceId": "e0", "targetId": "i0" },
                { "id": "x3", "type": "integrationTriggersCommand", "sourceId": "i0", "targetId": "c1" },
                { "id": "x4", "type": "commandProducesEvent",       "sourceId": "c1", "targetId": "e1" }
            ],
            "layout": {
                "nodePositions": {
                    "c0": { "x": 800, "y": 120 }, "e0": { "x": 800, "y": 400 },
                    "i0": { "x": 400, "y": 120 },
                    "c1": { "x": 40,  "y": 120 }, "e1": { "x": 40,  "y": 400 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, serde_json::to_string_pretty(&model).unwrap()).unwrap();

        let session = fixture_session(workspace);
        let result = handle(session, RelayoutEventModelParams {}).await.unwrap();
        assert!(result.applied > 0, "relayout should reorder; summary={}", result.summary);

        // Verify the wave order: Third (initializer) before Second before First.
        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let order_of = |name: &str| {
            patched["slices"]
                .as_array()
                .unwrap()
                .iter()
                .find(|s| s["name"] == name)
                .unwrap()["order"]
                .as_f64()
                .unwrap()
        };
        assert!(
            order_of("Third") < order_of("Second") && order_of("Second") < order_of("First"),
            "wave order should be Third < Second < First; got Third={}, Second={}, First={}",
            order_of("Third"), order_of("Second"), order_of("First"),
        );

        // Second relayout is a no-op — the model is now canonical.
        let after_first = std::fs::read_to_string(&model_path).unwrap();
        let session2 = fixture_session(workspace);
        let result2 = handle(session2, RelayoutEventModelParams {}).await.unwrap();
        assert_eq!(result2.applied, 0, "second relayout must be a fixed point; summary={}", result2.summary);
        assert_eq!(
            std::fs::read_to_string(&model_path).unwrap(),
            after_first,
            "second relayout must leave the file byte-identical",
        );
    }

    #[tokio::test]
    async fn relayout_prunes_orphan_empty_heal_slices() {
        // Reproduces the real-world breakage: a prior heal left a nodeless
        // `slice-heal-` slice ("Ghost") behind — its node ended up homed in a
        // different slice — plus a dedicated `chapter-heal-` for it. Relayout
        // must drop the empty slice AND reclaim its chapter, while keeping the
        // real flow's slice/chapter and a user-authored empty slice intact.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [
                { "id": "chapter-heal-real",  "name": "Init",  "order": 0 },
                { "id": "chapter-heal-ghost", "name": "Ghost", "order": 1 }
            ],
            "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "slice-heal-real",  "name": "Init",      "chapterId": "chapter-heal-real",  "order": 0 },
                { "id": "slice-heal-ghost", "name": "Ghost",     "chapterId": "chapter-heal-ghost", "order": 1 },
                { "id": "slice-user-empty", "name": "UserEmpty", "chapterId": null,                 "order": 2 }
            ],
            "nodes": [
                { "id": "c0", "type": "command", "name": "Initiate",  "sliceId": "slice-heal-real", "entityId": "e" },
                { "id": "e0", "type": "event",   "name": "Initiated", "sliceId": "slice-heal-real", "entityId": "e" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent", "sourceId": "c0", "targetId": "e0" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, serde_json::to_string_pretty(&model).unwrap()).unwrap();

        let session = fixture_session(workspace);
        let result = handle(session, RelayoutEventModelParams {}).await.unwrap();
        assert!(result.applied > 0, "relayout should prune the orphan; summary={}", result.summary);

        let patched: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&model_path).unwrap()).unwrap();
        let slice_ids: Vec<&str> = patched["slices"].as_array().unwrap()
            .iter().map(|s| s["id"].as_str().unwrap()).collect();
        let chapter_ids: Vec<&str> = patched["chapters"].as_array().unwrap()
            .iter().map(|c| c["id"].as_str().unwrap()).collect();
        assert!(!slice_ids.contains(&"slice-heal-ghost"), "empty heal slice must be gone; got {slice_ids:?}");
        assert!(!chapter_ids.contains(&"chapter-heal-ghost"), "ghost chapter must be reclaimed; got {chapter_ids:?}");
        assert!(slice_ids.contains(&"slice-heal-real"), "node-bearing slice must survive");
        assert!(slice_ids.contains(&"slice-user-empty"), "user-authored empty slice must survive");

        // Pruning must converge in a SINGLE pass: a second relayout is a true
        // fixed point (applied == 0, file byte-identical). This guards the
        // wave_slice_columns fix — if a removed slice still reserved a column,
        // the first pass would leave a gap and the second would re-tighten
        // every column.
        let after_first = std::fs::read_to_string(&model_path).unwrap();
        let session2 = fixture_session(workspace);
        let result2 = handle(session2, RelayoutEventModelParams {}).await.unwrap();
        assert_eq!(
            result2.applied, 0,
            "second relayout must be a fixed point; summary={}", result2.summary,
        );
        assert_eq!(
            std::fs::read_to_string(&model_path).unwrap(),
            after_first,
            "second relayout must leave the file byte-identical",
        );
    }

    #[tokio::test]
    async fn relayout_returns_zero_applied_when_file_already_canonical() {
        // No NeoHaskell project, valid model with positioned nodes in
        // the right bands → relayout has nothing to do.
        let dir = tempfile::tempdir().unwrap();
        let workspace = dir.path();
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [], "entities": [], "slices": [],
            "nodes": [], "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let model_path = workspace.join("event-model.json");
        std::fs::write(&model_path, serde_json::to_string_pretty(&model).unwrap()).unwrap();
        let original = std::fs::read_to_string(&model_path).unwrap();

        let session = fixture_session(workspace);
        let result = handle(session, RelayoutEventModelParams {}).await.unwrap();
        assert_eq!(result.applied, 0);
        // File untouched.
        assert_eq!(std::fs::read_to_string(&model_path).unwrap(), original);
    }
}
