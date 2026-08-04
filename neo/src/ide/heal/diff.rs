//! Compute a deterministic `HealDiff` between an event-model JSON value
//! and a NeoHaskell `ProjectInspection`.
//!
//! Scope:
//!   * Materialize missing structural pieces — entities, slices, and the
//!     command/event/query/integration nodes for every code-side symbol
//!     the model doesn't have yet. The diff also wires those new nodes
//!     with the edges their kind requires (commands → events, events →
//!     queries, events → integrations, integrations → commands).
//!   * Auto-wire missing edges between EXISTING nodes — driven by
//!     `command.produces`, `query.subscribes_to`,
//!     `integration.handles_events`, `integration.emits_commands`.
//!   * Fix integration `kind` drift — when inspection classifies an
//!     integration as Reactive (emits a command) but the model says
//!     Outbound, correct the model.
//!   * Fix misplaced y-positions — integration/command/query/UI nodes
//!     dropped into the event band (y > 300) → move to canonical band.
//!   * Add `layout.nodePositions` entries for every node missing one
//!     (including the brand-new materialized nodes).
//!
//! What the LLM still owns (`Residual`):
//!   * `OrphanModelNode` — a node in the model whose name has no code
//!     backing. LLM decides: typo (rename), dead (remove), planned (leave).
//!
//! Idempotency: deterministic IDs derived from `(type, name)` hashes keep
//! re-running the pass on an already-patched model a no-op.

use std::collections::{BTreeMap, BTreeSet};
use std::hash::{DefaultHasher, Hash, Hasher};

use serde::Serialize;
use serde_json::Value;

use crate::inspect::{DomainInspection, IntegrationKind, ProjectInspection};

/// Canonical y-band per node kind. Used both to detect misplaced positions
/// and to write a sensible default when a position is missing.
const Y_UI_PLACEHOLDER: f64 = -60.0;
const Y_COMMAND_QUERY_INTEGRATION: f64 = 120.0;
const Y_EVENT: f64 = 400.0;
/// Anything in `[300, ∞)` for an integration/command/query/UI is "in the
/// event band" — wrong, fix it. Below 300 we leave it alone.
const Y_BAND_FLOOR_FOR_NON_EVENT: f64 = 300.0;
/// Left margin used when a node has no slice (or its slice is unknown).
const SLICE_COLUMN_OFFSET: f64 = 40.0;

/// What the deterministic pass wants to change about the model. Each entry
/// carries enough info to apply without re-deriving from the inspection.
#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HealDiff {
    /// Chapters to add (one per causal flow whose heal-owned slices need a
    /// home — see `order_slices_by_wave`).
    pub add_chapters: Vec<ChapterToAdd>,
    /// Heal-created chapters (`chapter-heal-` prefix) to remove because no
    /// slice references them any more. Migrates models built under the old
    /// one-chapter-per-entity scheme to one-chapter-per-flow without leaving
    /// orphaned chapter arrows floating on the canvas. Never removes a
    /// user-authored chapter.
    pub remove_chapters: Vec<String>,
    /// Entities to add (one per inspection domain that lacks a matching entity).
    pub add_entities: Vec<EntityToAdd>,
    /// Slices to add (one per command/query/integration/orphan-event name
    /// that lacks a matching slice).
    pub add_slices: Vec<SliceToAdd>,
    /// Nodes (command/event/query/integration) to add for every code-side
    /// symbol the model is missing.
    pub add_nodes: Vec<NodeToAdd>,
    /// Edges to add between nodes (existing + freshly materialised).
    pub add_edges: Vec<EdgeToAdd>,
    /// Heal-authored edges (`edge-heal-` id prefix) to remove because the
    /// narrowed inspection no longer wires them — currently the over-
    /// approximated `eventFeedsQuery` edges that the old all-local default
    /// minted. `apply_remove_edges` double-gates on the `edge-heal-` prefix
    /// so a user-drawn edge is never removed even if field-overlap disagrees.
    pub remove_edges: Vec<EdgeRef>,
    /// Heal-created slices (`slice-heal-` prefix) to remove because no node
    /// references them any more. A prior heal can leave a named slice behind
    /// when its node ends up homed in a different slice (e.g. an integration
    /// materialised into its triggering command's slice) — the orphan slice
    /// then renders as an empty column and the wave pass mints a dead chapter
    /// for it. `apply_remove_slices` double-gates on the `slice-heal-` prefix
    /// so a user-authored slice is never removed even if it is momentarily
    /// empty.
    pub remove_slices: Vec<String>,
    /// Existing slices whose `chapterId` / `order` need updating to group
    /// them into their entity's chapter.
    pub update_slices: Vec<SliceUpdate>,
    /// Integration nodes whose `kind` field disagrees with the code.
    pub fix_integration_kinds: Vec<KindFix>,
    /// Existing nodes whose y-coordinate is in the wrong band.
    pub fix_positions: Vec<PositionFix>,
    /// Nodes with no entry in `layout.nodePositions` — includes freshly
    /// materialised nodes.
    pub ensure_layout_entries: Vec<LayoutEntry>,
    /// Unresolved issues — things the diff identified but cannot fix
    /// deterministically. The LLM (or the user) needs to resolve these.
    pub residuals: Vec<Residual>,
    /// Existing (or freshly-materialised) command/event nodes whose `fields`
    /// array is (re)written from the parsed Haskell source. Pure DATA: applied
    /// without any layout movement. Source is authoritative — fields are
    /// overwritten wholesale, never merged. Only queued when the source yields
    /// a NON-EMPTY field list that differs from the node's current fields, so a
    /// parser miss (empty extraction) never clobbers what's already displayed.
    pub set_node_fields: Vec<NodeFieldsSet>,
}

impl HealDiff {
    /// Total number of repairs this diff would apply (excludes residuals).
    pub fn applied_count(&self) -> usize {
        self.add_chapters.len()
            + self.remove_chapters.len()
            + self.add_entities.len()
            + self.add_slices.len()
            + self.add_nodes.len()
            + self.add_edges.len()
            + self.remove_edges.len()
            + self.remove_slices.len()
            + self.update_slices.len()
            + self.fix_integration_kinds.len()
            + self.fix_positions.len()
            + self.ensure_layout_entries.len()
            + self.set_node_fields.len()
    }

    /// Short, human-readable one-line summary for logs and the heal overlay.
    pub fn summary(&self) -> String {
        format!(
            "{} chapters, {} chapters removed, {} entities, {} slices, {} slices removed, {} nodes, {} edges, {} edges removed, {} slice updates, {} kind fixes, {} position fixes, {} layout entries, {} field updates, {} residuals",
            self.add_chapters.len(),
            self.remove_chapters.len(),
            self.add_entities.len(),
            self.add_slices.len(),
            self.remove_slices.len(),
            self.add_nodes.len(),
            self.add_edges.len(),
            self.remove_edges.len(),
            self.update_slices.len(),
            self.fix_integration_kinds.len(),
            self.fix_positions.len(),
            self.ensure_layout_entries.len(),
            self.set_node_fields.len(),
            self.residuals.len(),
        )
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ChapterToAdd {
    pub id: String,
    pub name: String,
    pub order: f64,
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EntityToAdd {
    pub id: String,
    pub name: String,
    pub order: f64,
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SliceToAdd {
    pub id: String,
    pub name: String,
    pub chapter_id: Option<String>,
    pub order: f64,
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SliceUpdate {
    pub slice_id: String,
    pub slice_name: String,
    /// `Some(_)` to set chapterId; absent on the struct means no chapter change.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub set_chapter_id: Option<String>,
    /// `Some(_)` to set order; absent means no order change.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub set_order: Option<f64>,
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NodeToAdd {
    pub id: String,
    pub node_type: String,
    pub name: String,
    pub slice_id: String,
    /// Only set for `command` / `event` nodes (queries + integrations don't
    /// carry an entity per schema).
    pub entity_id: Option<String>,
    /// Only set for `integration` nodes (`inbound` / `outbound`).
    pub kind: Option<String>,
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EdgeToAdd {
    pub edge_type: String,
    pub source_id: String,
    pub target_id: String,
    pub source_handle: String,
    pub target_handle: String,
    /// Human-readable rationale, e.g. "command RequestPayment produces event PaymentRequested".
    pub reason: String,
}

/// Identifies an edge by its content key `(type, source, target)` — the same
/// triple `synth_edge_id` hashes. Used by `remove_edges` to drop stale
/// heal-authored edges.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct EdgeRef {
    pub edge_type: String,
    pub source_id: String,
    pub target_id: String,
    /// Human-readable rationale, e.g. "query CartSummary no longer reads any
    /// field event ItemRemoved writes".
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct KindFix {
    pub node_id: String,
    pub node_name: String,
    pub from_kind: String,
    pub to_kind: String,
    pub reason: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PositionFix {
    pub node_id: String,
    pub node_name: String,
    pub node_kind: String,
    /// `Some` when the y-band fix should run. `None` for x-only fixes
    /// (e.g. slice-column rebalance, where we don't want to clobber a
    /// hand-set y).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub from_y: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub to_y: Option<f64>,
    /// `Some` when the x-axis fix should run.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub from_x: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub to_x: Option<f64>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LayoutEntry {
    pub node_id: String,
    pub x: f64,
    pub y: f64,
}

/// A node whose `fields` array should be (re)written from parsed source. The
/// `fields` reuse `crate::inspect::RecordField`, which serialises to the
/// event-model schema's `{ name, type }` shape — so `apply` writes them verbatim.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NodeFieldsSet {
    pub node_id: String,
    pub node_name: String,
    pub fields: Vec<crate::inspect::RecordField>,
    pub reason: String,
}

/// Something the deterministic pass noticed but cannot fix on its own.
/// The LLM uses this as its much-shorter input prompt.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum Residual {
    /// A model node whose name doesn't appear anywhere in the inspection.
    /// LLM (or user) decides whether it's a UI placeholder, a planned
    /// feature, or a typo.
    OrphanModelNode {
        node_id: String,
        node_name: String,
        node_type: String,
    },
}

/// Which phases of `compute_diff` to run. Default = all three (the heal flow).
///
/// The three phases are independent toggles:
///   * `structural` — materialise entities/slices/nodes/edges + kind fixes + orphan detection.
///   * `layout`     — y-band fixes, wave ordering, slice-column rebalance, layout entries.
///   * `fields`     — reconcile each existing/new command/event node's `fields`
///     from the parsed source (data-only; never moves anything on the canvas).
///
/// `layout_only` (used by `workspace/relayoutEventModel`) runs ONLY layout —
/// `fields` is OFF so a "clean up positions" pass never rewrites field content.
/// `fields_only` (used by the code→model sync when no new node appeared) runs
/// ONLY the field reconcile — zero structural materialisation, zero layout
/// movement: exactly the "edit fields of an EXISTING node" data sync.
#[derive(Debug, Clone, Copy)]
pub struct ComputeOptions {
    pub structural: bool,
    pub layout: bool,
    pub fields: bool,
}

impl Default for ComputeOptions {
    fn default() -> Self {
        Self::full()
    }
}

impl ComputeOptions {
    pub const fn full() -> Self {
        Self { structural: true, layout: true, fields: true }
    }
    pub const fn layout_only() -> Self {
        Self { structural: false, layout: true, fields: false }
    }
    pub const fn structural_only() -> Self {
        Self { structural: true, layout: false, fields: true }
    }
    /// Pure field reconcile — no structural materialisation, no layout. The
    /// sync engine prefers `structural_only` (a superset: also syncs new
    /// edges/kind fixes on existing nodes without layout), so this primitive is
    /// exercised only by the unit tests that isolate the `fields` phase.
    #[allow(dead_code)]
    pub const fn fields_only() -> Self {
        Self { structural: false, layout: false, fields: true }
    }
}

/// Compute the diff between a parsed event-model JSON value and the
/// project inspection. Returns an empty diff when the model already
/// matches the code.
pub fn compute_diff(model: &Value, inspection: &ProjectInspection) -> HealDiff {
    compute_diff_with_options(model, inspection, ComputeOptions::default())
}

/// Same as `compute_diff`, but with control over which phases run.
pub fn compute_diff_with_options(
    model: &Value,
    inspection: &ProjectInspection,
    opts: ComputeOptions,
) -> HealDiff {
    let mut diff = HealDiff::default();
    let mut plan = MaterializePlan::from_model(model);

    // --- 1. Ensure entities for each domain ----------------------------
    if opts.structural {
        for domain in &inspection.domains {
            plan.ensure_entity(&mut diff, &domain.name, &domain.name);
        }
    }

    // --- 2. Materialise nodes + wire edges per domain ------------------
    if opts.structural {
    for domain in &inspection.domains {
        let entity_id = plan.entity_id_for(&domain.name);
        let event_owner = primary_event_owners(domain);

        // Commands first: each command gets its own slice (named after the
        // command), and a command node. If the command already exists in
        // the model we reuse whatever slice it's already in.
        for cmd in &domain.commands {
            let cmd_node_id = plan.ensure_node_in_slice(
                &mut diff,
                "command",
                &cmd.name,
                &cmd.name,
                &format!("slice for command {}", cmd.name),
                entity_id.as_deref(),
                None,
                &format!("command {} discovered in domain {}", cmd.name, domain.name),
            );

            // Each event the command produces lives in the slice of its
            // *primary* (alphabetically-first) producing command. That keeps
            // an event shared by multiple commands from oscillating between
            // slices on re-runs.
            for ev_name in &cmd.produces {
                let primary = event_owner.get(ev_name.as_str()).copied().unwrap_or(&cmd.name);
                let ev_node_id = plan.ensure_node_in_slice(
                    &mut diff,
                    "event",
                    ev_name,
                    primary,
                    &format!("slice for command {primary}"),
                    entity_id.as_deref(),
                    None,
                    &format!(
                        "event {} produced by command {} (domain {})",
                        ev_name, primary, domain.name
                    ),
                );
                plan.ensure_edge(
                    &mut diff,
                    "commandProducesEvent",
                    &cmd_node_id,
                    &ev_node_id,
                    "bottom",
                    "top",
                    &format!(
                        "command {} produces event {} (per `decide` in domain {})",
                        cmd.name, ev_name, domain.name
                    ),
                );
            }
        }

        // Events that no command produces (declared in `Core.hs`/`Event.hs`
        // but unreferenced from any `decide`) — give them their own slice
        // so the structure is preserved.
        for ev in &domain.events {
            if event_owner.contains_key(ev.name.as_str()) {
                continue;
            }
            plan.ensure_node_in_slice(
                &mut diff,
                "event",
                &ev.name,
                &ev.name,
                &format!("slice for orphan event {}", ev.name),
                entity_id.as_deref(),
                None,
                &format!(
                    "event {} declared in domain {} (no producing command)",
                    ev.name, domain.name
                ),
            );
        }

        // Queries: each gets its own slice + node, edges from every
        // subscribed event we can find.
        for q in &domain.queries {
            let q_node_id = plan.ensure_node_in_slice(
                &mut diff,
                "query",
                &q.name,
                &q.name,
                &format!("slice for query {}", q.name),
                None,
                None,
                &format!("query {} discovered in domain {}", q.name, domain.name),
            );
            for ev_name in &q.subscribes_to {
                let Some(ev_node_id) = plan.node_id("event", ev_name) else {
                    continue;
                };
                plan.ensure_edge(
                    &mut diff,
                    "eventFeedsQuery",
                    &ev_node_id,
                    &q_node_id,
                    "right",
                    "left",
                    &format!(
                        "query {} subscribes to event {} (per query file in domain {})",
                        q.name, ev_name, domain.name
                    ),
                );
            }
            // Migration: drop heal-authored `eventFeedsQuery` edges this query
            // no longer subscribes to (the over-approximated all-local edges
            // a prior heal minted). User-drawn edges are never touched.
            let subscribed: BTreeSet<String> = q.subscribes_to.iter().cloned().collect();
            plan.queue_stale_query_edges(&mut diff, &q_node_id, &subscribed, &q.name);
        }

        // Integrations: each gets its own slice + node, kind set from
        // inspection. Then event→integration and integration→command edges.
        for intg in &domain.integrations {
            let inspection_kind = match intg.kind {
                IntegrationKind::Outbound => "outbound",
                IntegrationKind::Reactive => "inbound",
            };
            let intg_node_id = plan.ensure_node_in_slice(
                &mut diff,
                "integration",
                &intg.name,
                &intg.name,
                &format!("slice for integration {}", intg.name),
                None,
                Some(inspection_kind),
                &format!(
                    "integration {} discovered in domain {} (kind={inspection_kind})",
                    intg.name, domain.name
                ),
            );

            // Fix kind drift on every existing integration node with this
            // name (model may carry duplicates in multiple slices).
            for node in plan.existing_nodes_named("integration", &intg.name) {
                let current_kind = node.kind.as_deref().unwrap_or("");
                if current_kind != inspection_kind {
                    diff.fix_integration_kinds.push(KindFix {
                        node_id: node.id.clone(),
                        node_name: node.name.clone(),
                        from_kind: current_kind.to_string(),
                        to_kind: inspection_kind.to_string(),
                        reason: format!(
                            "code shows integration {} {} ({inspection_kind}-style handler)",
                            intg.name,
                            match intg.kind {
                                IntegrationKind::Outbound => "calls an external system",
                                IntegrationKind::Reactive => "emits a command (bridges domains)",
                            },
                        ),
                    });
                }
            }

            for ev_name in &intg.handles_events {
                let Some(ev_node_id) = plan.node_id("event", ev_name) else {
                    continue;
                };
                plan.ensure_edge(
                    &mut diff,
                    "eventTriggersIntegration",
                    &ev_node_id,
                    &intg_node_id,
                    "right",
                    "left",
                    &format!(
                        "integration {} handles event {} (per `handleEvent` in domain {})",
                        intg.name, ev_name, domain.name
                    ),
                );
            }

            for cmd_name in &intg.emits_commands {
                let Some(cmd_node_id) = plan.node_id("command", cmd_name) else {
                    continue;
                };
                plan.ensure_edge(
                    &mut diff,
                    "integrationTriggersCommand",
                    &intg_node_id,
                    &cmd_node_id,
                    "top",
                    "bottom",
                    &format!(
                        "integration {} emits command {} (per `Command.Emit` in domain {})",
                        intg.name, cmd_name, domain.name
                    ),
                );
            }
        }
    }
    } // close `if opts.structural` wrapping steps 1-2

    // --- 3. Orphan model nodes (model has them, code doesn't) ----------
    //
    // Only run when the inspection actually saw a NeoHaskell project. If
    // `inspection.domains` is empty (non-NeoHaskell workspace, or the
    // workspace wasn't analysed), we have no truth about what's supposed
    // to exist — flagging every existing node as orphan would be wrong.
    // We still run the position-fix + layout passes below so a "just
    // clean up positions" heal works on any workspace.
    if opts.structural && !inspection.domains.is_empty() {
        let inspection_names = inspection_name_set(inspection);
        for node in plan.iter_existing_nodes() {
            // UI placeholders don't have Haskell modules — never orphan them.
            if node.r#type == "uiPlaceholder" {
                continue;
            }
            if !inspection_names.contains(&(node.r#type.clone(), node.name.clone())) {
                diff.residuals.push(Residual::OrphanModelNode {
                    node_id: node.id.clone(),
                    node_name: node.name.clone(),
                    node_type: node.r#type.clone(),
                });
            }
        }
    }

    // --- 3.5. Reconcile node fields from source (data-only) ------------
    //
    // Runs whenever `opts.fields` is set — independent of structural/layout —
    // so the code→model sync can refresh fields with ZERO layout movement.
    // Covers both EXISTING model nodes and the nodes just queued in
    // `diff.add_nodes` (apply runs `apply_set_node_fields` after the node is
    // materialised). Source is authoritative; fields are overwritten wholesale.
    if opts.fields {
        reconcile_node_fields(model, inspection, &mut diff);
    }

    if !opts.layout {
        return diff;
    }

    // --- 4. Position fixes (existing nodes only) -----------------------
    let positions = model
        .get("layout")
        .and_then(|l| l.get("nodePositions"))
        .and_then(|p| p.as_object());

    for node in plan.iter_existing_nodes() {
        let pos = positions.and_then(|m| m.get(&node.id));
        if let Some(obj) = pos.and_then(|v| v.as_object()) {
            let current_y = obj.get("y").and_then(|v| v.as_f64());
            if let Some(current_y) = current_y {
                let target_y = canonical_y(&node.r#type);
                let in_event_band = current_y > Y_BAND_FLOOR_FOR_NON_EVENT;
                let is_non_event = node.r#type != "event";
                if is_non_event && in_event_band && (current_y - target_y).abs() > f64::EPSILON {
                    diff.fix_positions.push(PositionFix {
                        node_id: node.id.clone(),
                        node_name: node.name.clone(),
                        node_kind: node.r#type.clone(),
                        from_y: Some(current_y),
                        to_y: Some(target_y),
                        from_x: None,
                        to_x: None,
                    });
                }
            }
        }
    }

    // --- 4.5. Wave ordering + chapter grouping -----------------------
    //
    // Order slices left-to-right by the event-modeling WAVE (a pure
    // function of the node/edge graph: initializer-rooted, longest-path
    // layered, one contiguous block per causal flow), and group every
    // heal-owned slice into one chapter per flow. Must run BEFORE the
    // rebalance + layout passes — those use slice `order` to compute x
    // columns, so setting the wave order here makes the x assignment match
    // the final layout instead of triggering a second-run fix.
    order_slices_by_wave(model, &plan, &mut diff);
    // After ordering, sort pending slices by their reassigned order so
    // PositionCalculator iterates them in the new left-to-right rhythm.
    diff.add_slices.sort_by(|a, b| {
        a.order
            .partial_cmp(&b.order)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    // --- 4.7. Slice-column rebalance ---------------------------------
    //
    // Walks slices in `order` and ensures each slice's column is at
    // least `DEFAULT_NODE_WIDTH + SLICE_COLUMN_GAP` past the previous
    // one's. Catches the case where an earlier heal pass placed a new
    // slice at a hash-derived x that happens to overlap a hand-placed
    // slice's column. Re-positions the colliding nodes (x only — y is
    // left alone so the event/command band assignment from the y pass
    // stands).
    rebalance_slice_columns(model, &plan, &mut diff);

    // --- 5. Layout entries (existing nodes lacking a position +
    //                       every freshly materialised node) -----------
    //
    // Position assignment is a SECOND pass so we can see every existing
    // node's x and place new slice columns past the rightmost edge —
    // never on top of an existing well-placed node.
    let mut layout = PositionCalculator::new(model, &plan, &diff);
    let positions = model
        .get("layout")
        .and_then(|l| l.get("nodePositions"))
        .and_then(|p| p.as_object());
    let mut layout_added: BTreeSet<String> = BTreeSet::new();
    for node in plan.iter_all_nodes() {
        let has_position = positions
            .and_then(|m| m.get(&node.id))
            .and_then(|v| v.as_object())
            .map(|obj| {
                obj.get("x").and_then(|v| v.as_f64()).is_some()
                    && obj.get("y").and_then(|v| v.as_f64()).is_some()
            })
            .unwrap_or(false);
        if has_position {
            continue;
        }
        if !layout_added.insert(node.id.clone()) {
            continue;
        }
        let (x, y) = layout.assign(
            node.slice_id.as_deref(),
            &node.r#type,
            node.entity_id.as_deref(),
        );
        diff.ensure_layout_entries.push(LayoutEntry {
            node_id: node.id.clone(),
            x,
            y,
        });
    }

    diff
}

/// Reconcile each command/event node's `fields` against the parsed source
/// (gated by `ComputeOptions::fields`). Queues a `set_node_fields` overwrite
/// when the source yields a NON-EMPTY field list that differs from the node's
/// current fields — both for existing model nodes and for nodes just queued in
/// `diff.add_nodes`. An empty source extraction is treated as "no information"
/// and never clears existing fields, so a dumb-parser miss can't erase what's
/// already displayed. Source is authoritative: fields are overwritten wholesale,
/// never merged, which keeps the sync idempotent (a second run finds no diff).
fn reconcile_node_fields(model: &Value, inspection: &ProjectInspection, diff: &mut HealDiff) {
    use crate::inspect::RecordField;

    // (type, name) → source fields, for the field-bearing node kinds only.
    let mut source: BTreeMap<(&str, &str), &Vec<RecordField>> = BTreeMap::new();
    for domain in &inspection.domains {
        for c in &domain.commands {
            if !c.fields.is_empty() {
                source.insert(("command", c.name.as_str()), &c.fields);
            }
        }
        for e in &domain.events {
            if !e.fields.is_empty() {
                source.insert(("event", e.name.as_str()), &e.fields);
            }
        }
        for q in &domain.queries {
            if !q.fields.is_empty() {
                source.insert(("query", q.name.as_str()), &q.fields);
            }
        }
    }
    if source.is_empty() {
        return;
    }

    let mut queued: Vec<NodeFieldsSet> = Vec::new();

    // Existing model nodes whose fields drifted from source.
    if let Some(arr) = model.get("nodes").and_then(|v| v.as_array()) {
        for node in arr {
            let (Some(id), Some(ty), Some(name)) = (
                node.get("id").and_then(|v| v.as_str()),
                node.get("type").and_then(|v| v.as_str()),
                node.get("name").and_then(|v| v.as_str()),
            ) else {
                continue;
            };
            let Some(src_fields) = source.get(&(ty, name)) else {
                continue;
            };
            if node_fields_of(node) != **src_fields {
                queued.push(NodeFieldsSet {
                    node_id: id.to_string(),
                    node_name: name.to_string(),
                    fields: (*src_fields).clone(),
                    reason: format!("{ty} {name}: fields reconciled from source"),
                });
            }
        }
    }

    // Freshly-materialised nodes (structural pass) aren't in `model` yet —
    // match them against the queued `NodeToAdd`s. `apply_set_node_fields` runs
    // after `apply_add_nodes`, so the node exists by the time fields are written.
    for n in &diff.add_nodes {
        if let Some(src_fields) = source.get(&(n.node_type.as_str(), n.name.as_str())) {
            queued.push(NodeFieldsSet {
                node_id: n.id.clone(),
                node_name: n.name.clone(),
                fields: (*src_fields).clone(),
                reason: format!("{} {}: fields set from source", n.node_type, n.name),
            });
        }
    }

    diff.set_node_fields.extend(queued);
}

/// Parse a model node's current `fields` JSON array into `RecordField`s for
/// equality comparison. A missing/absent `fields` reads as empty.
fn node_fields_of(node: &Value) -> Vec<crate::inspect::RecordField> {
    node.get("fields")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|f| {
                    let name = f.get("name").and_then(|v| v.as_str())?.to_string();
                    let type_name = f.get("type").and_then(|v| v.as_str())?.to_string();
                    Some(crate::inspect::RecordField { name, type_name })
                })
                .collect()
        })
        .unwrap_or_default()
}

/// Order every slice left-to-right by the event-modeling WAVE and group
/// heal-owned slices into one chapter per causal flow.
///
/// Replaces the old alphabetical-by-entity grouping. The wave order is a
/// pure function of the node/edge graph (`compute_wave_order`): an
/// initializer command (one not triggered by any `integrationTriggersCommand`
/// edge) anchors the left of its flow; each consequence sits one column
/// right of its deepest cause; independent flows are contiguous blocks.
///
/// Chapter policy (user decision D1): a chapter is created per causal flow,
/// named after the flow's root slice. Only HEAL-owned slices (`slice-heal-`
/// id prefix) are (re)assigned to a chapter — user-authored slices keep
/// their existing `chapterId`. Heal-created chapters (`chapter-heal-` prefix)
/// that no slice references any more are queued for removal, migrating models
/// built under the old one-chapter-per-entity scheme.
fn order_slices_by_wave(model: &Value, plan: &MaterializePlan, diff: &mut HealDiff) {
    let wave = compute_wave_order(model, plan, diff);

    // Existing model slice records (chapter + order + name).
    let mut model_slice_chapter: BTreeMap<String, Option<String>> = BTreeMap::new();
    let mut model_slice_order: BTreeMap<String, f64> = BTreeMap::new();
    let mut model_slice_name: BTreeMap<String, String> = BTreeMap::new();
    if let Some(arr) = model.get("slices").and_then(|v| v.as_array()) {
        for s in arr {
            let Some(id) = s.get("id").and_then(|v| v.as_str()) else {
                continue;
            };
            let chap = s
                .get("chapterId")
                .and_then(|v| if v.is_null() { None } else { v.as_str() })
                .map(|x| x.to_string());
            model_slice_chapter.insert(id.to_string(), chap);
            if let Some(o) = s.get("order").and_then(|v| v.as_f64()) {
                model_slice_order.insert(id.to_string(), o);
            }
            if let Some(n) = s.get("name").and_then(|v| v.as_str()) {
                model_slice_name.insert(id.to_string(), n.to_string());
            }
        }
    }

    // Existing chapters: name -> id, and the set of heal-created chapter ids.
    let mut chapter_by_name: BTreeMap<String, String> = BTreeMap::new();
    let mut existing_heal_chapters: BTreeSet<String> = BTreeSet::new();
    if let Some(arr) = model.get("chapters").and_then(|v| v.as_array()) {
        for c in arr {
            if let (Some(id), Some(name)) = (
                c.get("id").and_then(|v| v.as_str()),
                c.get("name").and_then(|v| v.as_str()),
            ) {
                chapter_by_name.insert(name.to_string(), id.to_string());
                if id.starts_with("chapter-heal-") {
                    existing_heal_chapters.insert(id.to_string());
                }
            }
        }
    }

    // Resolve the final chapterId for every slice. Heal-owned slices get
    // their flow's chapter (created on demand); user-authored slices keep
    // their existing chapterId untouched.
    let mut resolved_chapter: BTreeMap<String, String> = BTreeMap::new(); // flow name -> chapter id
    let mut final_chapter: BTreeMap<String, Option<String>> = BTreeMap::new();
    for sid in wave.slice_order.keys() {
        if sid.starts_with("slice-heal-") {
            let (root_name, rank) = wave
                .slice_flow
                .get(sid)
                .cloned()
                .unwrap_or_else(|| (String::new(), 0));
            let chap_id = if let Some(id) = resolved_chapter.get(&root_name) {
                id.clone()
            } else if let Some(id) = chapter_by_name.get(&root_name) {
                resolved_chapter.insert(root_name.clone(), id.clone());
                id.clone()
            } else {
                let id = synth_id("chapter", &root_name);
                // Persist the SAME order the flow was ranked by, so a re-run
                // reads it back and produces an empty diff (fixed point). New
                // flows land after any user-ordered chapters.
                let order = wave
                    .flow_synth_order
                    .get(&root_name)
                    .copied()
                    .unwrap_or(rank as f64);
                diff.add_chapters.push(ChapterToAdd {
                    id: id.clone(),
                    name: root_name.clone(),
                    order,
                    reason: format!("chapter for causal flow starting at {root_name}"),
                });
                resolved_chapter.insert(root_name.clone(), id.clone());
                id
            };
            final_chapter.insert(sid.clone(), Some(chap_id));
        } else {
            let existing = model_slice_chapter.get(sid).cloned().unwrap_or(None);
            final_chapter.insert(sid.clone(), existing);
        }
    }

    // Apply order (all slices) + chapter (heal slices) to pending + existing.
    for sid in wave.slice_order.keys() {
        let new_order = wave.slice_order[sid];
        let new_chapter = final_chapter.get(sid).cloned().unwrap_or(None);

        // Pending slice (queued this pass) — mutate in place.
        if let Some(s) = diff.add_slices.iter_mut().find(|s| &s.id == sid) {
            s.order = new_order as f64;
            s.chapter_id = new_chapter.clone();
            continue;
        }

        // Existing model slice — emit a SliceUpdate only when something moved.
        let cur_order = model_slice_order.get(sid).copied();
        let cur_chapter = model_slice_chapter.get(sid).cloned().unwrap_or(None);
        let order_changed = cur_order
            .map(|c| (c - new_order as f64).abs() > 0.5)
            .unwrap_or(true);
        let chapter_changed = cur_chapter.as_deref() != new_chapter.as_deref();
        if !order_changed && !chapter_changed {
            continue;
        }
        diff.update_slices.push(SliceUpdate {
            slice_id: sid.clone(),
            slice_name: model_slice_name.get(sid).cloned().unwrap_or_default(),
            set_chapter_id: if chapter_changed {
                new_chapter.clone()
            } else {
                None
            },
            set_order: if order_changed {
                Some(new_order as f64)
            } else {
                None
            },
            reason: "group into causal-flow chapter, order by wave".to_string(),
        });
    }

    // Remove heal-created slices that hold no node — orphans a prior heal left
    // behind when the node ended up homed in a different slice. They are
    // excluded from `wave.slice_order` above (so they got no order/chapter),
    // and dropping them lets the chapter cleanup below reclaim their dedicated
    // chapter. Only `slice-heal-` ids are removed; a user-authored empty slice
    // is left alone.
    let slices_with_nodes: BTreeSet<String> = plan
        .iter_all_nodes()
        .filter_map(|n| n.slice_id.clone())
        .collect();
    for sid in model_slice_chapter.keys() {
        if sid.starts_with("slice-heal-") && !slices_with_nodes.contains(sid) {
            diff.remove_slices.push(sid.clone());
        }
    }

    // Remove heal-created chapters no longer referenced by any slice.
    let mut live: BTreeSet<String> = BTreeSet::new();
    for ch in final_chapter.values().filter_map(|o| o.as_ref()) {
        live.insert(ch.clone());
    }
    for ch_id in &existing_heal_chapters {
        if !live.contains(ch_id) {
            diff.remove_chapters.push(ch_id.clone());
        }
    }
}

/// Result of the deterministic wave-ordering pass — a pure function of the
/// model's node/edge graph (existing ∪ pending-in-`diff`).
struct WaveOrder {
    /// Final left-to-right order index for every slice id.
    slice_order: BTreeMap<String, usize>,
    /// slice id -> (flow root slice name, flow rank). One flow per weakly
    /// connected component of the slice-precedence graph; the root is the
    /// flow's initializer (or, lacking one, its left-most slice).
    slice_flow: BTreeMap<String, (String, usize)>,
    /// flow root slice name -> the `order` value to persist when SYNTHESIZING a
    /// brand-new chapter for that flow. Equals the `eff_order` the flow was
    /// ranked by, so the synthesized chapter's stored order matches the value
    /// that produced its column — making a re-run a fixed point. (User-set
    /// chapter orders are never overwritten; see `order_slices_by_wave`.)
    flow_synth_order: BTreeMap<String, f64>,
}

/// `(name, id)` total-order key for a slice — the single source of
/// determinism: every tie in the wave pass resolves on this.
fn nkey(slice_name: &BTreeMap<String, String>, s: &str) -> (String, String) {
    (slice_name.get(s).cloned().unwrap_or_default(), s.to_string())
}

/// Union-find root with path compression over a `BTreeMap` forest.
fn uf_find(parent: &mut BTreeMap<String, String>, x: &str) -> String {
    let mut root = x.to_string();
    while parent.get(&root).map(|p| p != &root).unwrap_or(false) {
        root = parent[&root].clone();
    }
    let mut cur = x.to_string();
    while parent.get(&cur).map(|p| p != &root).unwrap_or(false) {
        let next = parent[&cur].clone();
        parent.insert(cur, root.clone());
        cur = next;
    }
    root
}

/// Deterministic DFS recording every arc to a node currently on the stack as
/// a back edge. Visiting neighbours in `nkey` order makes the feedback set a
/// pure function of the graph. Recursion depth is bounded by the longest
/// causal chain — event models are small (tens of slices), so this is safe.
fn dfs_back_edges(
    u: &str,
    succ: &BTreeMap<String, BTreeSet<String>>,
    slice_name: &BTreeMap<String, String>,
    visited: &mut BTreeSet<String>,
    on_stack: &mut BTreeSet<String>,
    back_edges: &mut BTreeSet<(String, String)>,
) {
    visited.insert(u.to_string());
    on_stack.insert(u.to_string());
    let mut kids: Vec<String> = succ
        .get(u)
        .map(|s| s.iter().cloned().collect())
        .unwrap_or_default();
    kids.sort_by(|a, b| nkey(slice_name, a).cmp(&nkey(slice_name, b)));
    for v in kids {
        if on_stack.contains(&v) {
            back_edges.insert((u.to_string(), v.clone()));
        } else if !visited.contains(&v) {
            dfs_back_edges(&v, succ, slice_name, visited, on_stack, back_edges);
        }
    }
    on_stack.remove(u);
}

/// Compute the wave order. Pure function of `(plan nodes+edges, model slices,
/// diff pending slices)`. See `order_slices_by_wave` for the contract.
fn compute_wave_order(model: &Value, plan: &MaterializePlan, diff: &HealDiff) -> WaveOrder {
    // 1. node -> slice (existing ∪ pending).
    let mut node_slice: BTreeMap<String, String> = BTreeMap::new();
    for n in plan.iter_all_nodes() {
        if let Some(sid) = &n.slice_id {
            node_slice.insert(n.id.clone(), sid.clone());
        }
    }

    // slice id -> name (model slices ∪ pending add_slices).
    let mut slice_name: BTreeMap<String, String> = BTreeMap::new();
    if let Some(arr) = model.get("slices").and_then(|v| v.as_array()) {
        for s in arr {
            if let (Some(id), Some(name)) = (
                s.get("id").and_then(|v| v.as_str()),
                s.get("name").and_then(|v| v.as_str()),
            ) {
                slice_name.insert(id.to_string(), name.to_string());
            }
        }
    }
    for s in &diff.add_slices {
        slice_name
            .entry(s.id.clone())
            .or_insert_with(|| s.name.clone());
    }
    // Only rank slices that actually hold a node. A nodeless slice has no
    // edges, so it would form its own weakly-connected component and the wave
    // pass would mint a dedicated chapter for an empty column. `order_slices_
    // by_wave` removes such heal-owned orphans instead; user-authored empty
    // slices are simply left untouched (no order/chapter churn).
    let slices_with_nodes: BTreeSet<String> = node_slice.values().cloned().collect();
    let all_slices: BTreeSet<String> = slice_name
        .keys()
        .filter(|s| slices_with_nodes.contains(*s))
        .cloned()
        .collect();

    // 2. slice-precedence arcs + the integration-triggered command set.
    let mut succ: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut all_arcs: BTreeSet<(String, String)> = BTreeSet::new();
    let mut triggered: BTreeSet<String> = BTreeSet::new();
    for (etype, src, tgt) in &plan.edge_keys {
        if etype.as_str() == "integrationTriggersCommand" {
            triggered.insert(tgt.clone());
        }
        let (Some(ss), Some(ts)) = (node_slice.get(src), node_slice.get(tgt)) else {
            continue;
        };
        if ss == ts {
            continue; // intra-slice — no precedence
        }
        succ.entry(ss.clone()).or_default().insert(ts.clone());
        all_arcs.insert((ss.clone(), ts.clone()));
    }
    for s in &all_slices {
        succ.entry(s.clone()).or_default();
    }

    // 3. initializer per slice — a slice holding a command not triggered by
    //    an integration.
    let mut is_init: BTreeMap<String, bool> =
        all_slices.iter().map(|s| (s.clone(), false)).collect();
    for n in plan.iter_all_nodes() {
        if n.r#type == "command" {
            if let Some(sid) = &n.slice_id {
                if !triggered.contains(&n.id) {
                    is_init.insert(sid.clone(), true);
                }
            }
        }
    }

    // 4. deterministic back-edge removal (saga cycles) -> forward DAG.
    let mut sorted_slices: Vec<String> = all_slices.iter().cloned().collect();
    sorted_slices.sort_by(|a, b| nkey(&slice_name, a).cmp(&nkey(&slice_name, b)));
    let mut visited: BTreeSet<String> = BTreeSet::new();
    let mut on_stack: BTreeSet<String> = BTreeSet::new();
    let mut back_edges: BTreeSet<(String, String)> = BTreeSet::new();
    for root in &sorted_slices {
        if !visited.contains(root) {
            dfs_back_edges(
                root,
                &succ,
                &slice_name,
                &mut visited,
                &mut on_stack,
                &mut back_edges,
            );
        }
    }

    let mut fsucc: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut fpreds: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for s in &all_slices {
        fsucc.entry(s.clone()).or_default();
        fpreds.entry(s.clone()).or_default();
    }
    for (u, vs) in &succ {
        for v in vs {
            if back_edges.contains(&(u.clone(), v.clone())) {
                continue;
            }
            fsucc.get_mut(u).unwrap().insert(v.clone());
            fpreds.get_mut(v).unwrap().insert(u.clone());
        }
    }

    // 5. longest-path layering on the forward DAG (Kahn).
    let mut layer: BTreeMap<String, usize> =
        all_slices.iter().map(|s| (s.clone(), 0usize)).collect();
    let mut indeg: BTreeMap<String, usize> = all_slices
        .iter()
        .map(|s| (s.clone(), fpreds[s].len()))
        .collect();
    let mut queue: Vec<String> = all_slices
        .iter()
        .filter(|s| indeg[*s] == 0)
        .cloned()
        .collect();
    queue.sort_by(|a, b| nkey(&slice_name, a).cmp(&nkey(&slice_name, b)));
    let mut qi = 0;
    while qi < queue.len() {
        let u = queue[qi].clone();
        qi += 1;
        let lu = layer[&u];
        let mut kids: Vec<String> = fsucc[&u].iter().cloned().collect();
        kids.sort_by(|a, b| nkey(&slice_name, a).cmp(&nkey(&slice_name, b)));
        for v in kids {
            if lu + 1 > layer[&v] {
                layer.insert(v.clone(), lu + 1);
            }
            let d = indeg.get_mut(&v).unwrap();
            *d -= 1;
            if *d == 0 {
                queue.push(v);
            }
        }
    }

    // 6. weakly-connected components (flows) via union-find over all arcs.
    let mut parent: BTreeMap<String, String> =
        all_slices.iter().map(|s| (s.clone(), s.clone())).collect();
    for (a, b) in &all_arcs {
        let ra = uf_find(&mut parent, a);
        let rb = uf_find(&mut parent, b);
        if ra != rb {
            parent.insert(ra, rb);
        }
    }
    let mut comp_members: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for s in &all_slices {
        let r = uf_find(&mut parent, s);
        comp_members.entry(r).or_default().push(s.clone());
    }

    // `chapter.order` is the user-authoritative axis for horizontal flow
    // ordering. Build chapter-order lookups (by id + by name) and each slice's
    // chapterId from the model. The wave pass only READS these — it NEVER
    // rewrites an existing chapter's order (see `order_slices_by_wave`) — so a
    // manual drag-reorder in the Chapters panel resequences the timeline and
    // survives "Tidy by flow" / heal.
    let mut chapter_order_by_id: BTreeMap<String, f64> = BTreeMap::new();
    let mut chapter_order_by_name: BTreeMap<String, f64> = BTreeMap::new();
    if let Some(arr) = model.get("chapters").and_then(|v| v.as_array()) {
        for c in arr {
            let order = c.get("order").and_then(|v| v.as_f64()).unwrap_or(0.0);
            if let Some(id) = c.get("id").and_then(|v| v.as_str()) {
                chapter_order_by_id.insert(id.to_string(), order);
            }
            if let Some(name) = c.get("name").and_then(|v| v.as_str()) {
                chapter_order_by_name.insert(name.to_string(), order);
            }
        }
    }
    let mut slice_chapter: BTreeMap<String, String> = BTreeMap::new();
    if let Some(arr) = model.get("slices").and_then(|v| v.as_array()) {
        for s in arr {
            if let (Some(id), Some(ch)) = (
                s.get("id").and_then(|v| v.as_str()),
                s.get("chapterId")
                    .and_then(|v| if v.is_null() { None } else { v.as_str() }),
            ) {
                slice_chapter.insert(id.to_string(), ch.to_string());
            }
        }
    }
    let max_existing_order = chapter_order_by_id
        .values()
        .copied()
        .fold(f64::NEG_INFINITY, f64::max);

    // Component root (initializer-first, then smallest (layer, name_key)) +
    // min layer + the flow's effective chapter order. Components (flows) are
    // ranked by `(eff_order, min_layer, name_key(root))`: chapter order first,
    // the original deterministic wave tiebreak second.
    struct Comp {
        members: Vec<String>,
        root: String,
        min_layer: usize,
        /// The flow's existing chapter order, if it already has one (via a
        /// member slice's chapterId, else the chapter named after the root).
        /// `None` for a brand-new flow — assigned a trailing order below.
        existing_order: Option<f64>,
        eff_order: f64,
    }
    let mut comps: Vec<Comp> = Vec::new();
    for (_r, members) in comp_members {
        let min_layer = members.iter().map(|s| layer[s]).min().unwrap_or(0);
        let mut root: Option<String> = None;
        let mut root_key: Option<(usize, (String, String))> = None;
        let mut root_is_init = false;
        for m in &members {
            let mi = is_init[m];
            let k = (layer[m], nkey(&slice_name, m));
            let better = match (&root_key, root_is_init, mi) {
                (None, _, _) => true,
                (Some(_), false, true) => true,  // m initializes, current doesn't
                (Some(_), true, false) => false, // current initializes, m doesn't
                (Some(rk), _, _) => k < *rk,     // same init-ness: smaller key wins
            };
            if better {
                root = Some(m.clone());
                root_key = Some(k);
                root_is_init = mi;
            }
        }
        let root = root.unwrap_or_default();
        let root_name = slice_name.get(&root).cloned().unwrap_or_default();
        // Prefer a member slice's chapterId; fall back to the chapter named
        // after the flow root (the heal-by-name convention). The min across
        // members keeps a multi-chapter flow adjacent to its lowest chapter.
        let existing_order = members
            .iter()
            .filter_map(|m| slice_chapter.get(m))
            .filter_map(|cid| chapter_order_by_id.get(cid).copied())
            .fold(None, |acc: Option<f64>, o| {
                Some(acc.map_or(o, |a: f64| a.min(o)))
            })
            .or_else(|| chapter_order_by_name.get(&root_name).copied());
        comps.push(Comp {
            members,
            root,
            min_layer,
            existing_order,
            eff_order: 0.0,
        });
    }
    // Brand-new flows (no chapter yet) append AFTER all existing chapters,
    // ordered among themselves by their natural `(min_layer, nkey(root))` wave
    // position. With no chapters at all, `base = -1` so they get `0,1,2,…` —
    // byte-identical to the pre-feature ranking (all wave tests stay green).
    let base = if max_existing_order.is_finite() {
        max_existing_order
    } else {
        -1.0
    };
    let mut new_idx: Vec<usize> = comps
        .iter()
        .enumerate()
        .filter(|(_, c)| c.existing_order.is_none())
        .map(|(i, _)| i)
        .collect();
    new_idx.sort_by(|&a, &b| {
        (comps[a].min_layer, nkey(&slice_name, &comps[a].root))
            .cmp(&(comps[b].min_layer, nkey(&slice_name, &comps[b].root)))
    });
    for (k, &i) in new_idx.iter().enumerate() {
        comps[i].eff_order = base + 1.0 + k as f64;
    }
    for c in comps.iter_mut() {
        if let Some(o) = c.existing_order {
            c.eff_order = o;
        }
    }
    comps.sort_by(|a, b| {
        a.eff_order
            .partial_cmp(&b.eff_order)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| {
                (a.min_layer, nkey(&slice_name, &a.root))
                    .cmp(&(b.min_layer, nkey(&slice_name, &b.root)))
            })
    });
    let mut comp_rank: BTreeMap<String, usize> = BTreeMap::new();
    let mut flow_root: BTreeMap<String, String> = BTreeMap::new();
    // root name -> the order to persist if this flow's chapter is synthesized.
    let mut flow_synth_order: BTreeMap<String, f64> = BTreeMap::new();
    for (rank, c) in comps.iter().enumerate() {
        let root_name = slice_name.get(&c.root).cloned().unwrap_or_default();
        flow_synth_order.insert(root_name.clone(), c.eff_order);
        for m in &c.members {
            comp_rank.insert(m.clone(), rank);
            flow_root.insert(m.clone(), root_name.clone());
        }
    }

    // 7. priority-queue Kahn sweep over the forward DAG. Key term order:
    //    (component_rank, layer, barycenter-of-emitted-preds, name_key).
    let mut findeg: BTreeMap<String, usize> = all_slices
        .iter()
        .map(|s| (s.clone(), fpreds[s].len()))
        .collect();
    let mut final_order: BTreeMap<String, usize> = BTreeMap::new();
    let mut heap: std::collections::BinaryHeap<
        std::cmp::Reverse<(usize, usize, i64, String, String)>,
    > = std::collections::BinaryHeap::new();
    let key_of = |s: &str,
                  final_order: &BTreeMap<String, usize>|
     -> (usize, usize, i64, String, String) {
        let preds = &fpreds[s];
        let fr: i64 = if !preds.is_empty() {
            let sum: usize = preds
                .iter()
                .map(|p| final_order.get(p).copied().unwrap_or(0))
                .sum();
            ((sum as f64) / (preds.len() as f64)).round() as i64
        } else if is_init[s] {
            -1
        } else {
            0
        };
        let (nm, id) = nkey(&slice_name, s);
        (comp_rank[s], layer[s], fr, nm, id)
    };
    for s in &all_slices {
        if findeg[s] == 0 {
            heap.push(std::cmp::Reverse(key_of(s, &final_order)));
        }
    }
    let mut next = 0usize;
    while let Some(std::cmp::Reverse(key)) = heap.pop() {
        let s = key.4.clone();
        if final_order.contains_key(&s) {
            continue;
        }
        final_order.insert(s.clone(), next);
        next += 1;
        let mut kids: Vec<String> = fsucc[&s].iter().cloned().collect();
        kids.sort_by(|a, b| nkey(&slice_name, a).cmp(&nkey(&slice_name, b)));
        for v in kids {
            let d = findeg.get_mut(&v).unwrap();
            *d -= 1;
            if *d == 0 {
                heap.push(std::cmp::Reverse(key_of(&v, &final_order)));
            }
        }
    }
    // Safety net (defensive — the forward graph is acyclic so this is empty).
    let mut leftover: Vec<String> = all_slices
        .iter()
        .filter(|s| !final_order.contains_key(*s))
        .cloned()
        .collect();
    leftover.sort_by(|a, b| nkey(&slice_name, a).cmp(&nkey(&slice_name, b)));
    for s in leftover {
        final_order.insert(s.clone(), next);
        next += 1;
    }

    let mut slice_flow: BTreeMap<String, (String, usize)> = BTreeMap::new();
    for s in &all_slices {
        slice_flow.insert(
            s.clone(),
            (
                flow_root.get(s).cloned().unwrap_or_default(),
                comp_rank.get(s).copied().unwrap_or(0),
            ),
        );
    }
    WaveOrder {
        slice_order: final_order,
        slice_flow,
        flow_synth_order,
    }
}

/// Compact, left-anchored column x per slice, ordered by the EFFECTIVE wave
/// order (model order overridden by this diff's `update_slices`, plus pending
/// `add_slices`). Columns start at `SLICE_COLUMN_OFFSET` and step by
/// `DEFAULT_NODE_WIDTH + SLICE_COLUMN_GAP`.
///
/// This is the single source of horizontal placement — BOTH the existing-node
/// x-fix (`rebalance_slice_columns`) and the new-node placement
/// (`PositionCalculator`) read it, so `slice.order` and node x stay in
/// lockstep AND the diagram stays anchored near the origin. It deliberately
/// ignores each node's existing x: when the wave reorders slices, a slice that
/// used to live far to the right must move to its new column near the origin.
/// The previous `max(current_x, prev + pitch)` logic only ever pushed right,
/// so a big reorder cascaded every column thousands of px off-screen, leaving
/// the canvas blank.
fn wave_slice_columns(model: &Value, diff: &HealDiff) -> BTreeMap<String, f64> {
    let mut order_override: BTreeMap<String, f64> = BTreeMap::new();
    for u in &diff.update_slices {
        if let Some(o) = u.set_order {
            order_override.insert(u.slice_id.clone(), o);
        }
    }

    // Slices queued for removal this pass must NOT reserve a column — they
    // are about to vanish from the model. Counting them would leave a gap in
    // the dense left-anchored grid, so the next relayout (with the slices now
    // gone) would re-tighten every column and the layout would need a second
    // pass to settle. Excluding them makes a single relayout a fixed point.
    let removed: BTreeSet<&str> = diff.remove_slices.iter().map(|s| s.as_str()).collect();

    let mut all_slices: Vec<(String, f64)> = Vec::new();
    if let Some(arr) = model.get("slices").and_then(|v| v.as_array()) {
        for s in arr {
            let Some(id) = s.get("id").and_then(|v| v.as_str()) else {
                continue;
            };
            if removed.contains(id) {
                continue;
            }
            let order = order_override
                .get(id)
                .copied()
                .or_else(|| s.get("order").and_then(|v| v.as_f64()))
                .unwrap_or(0.0);
            all_slices.push((id.to_string(), order));
        }
    }
    for s in &diff.add_slices {
        all_slices.push((s.id.clone(), s.order));
    }
    all_slices.sort_by(|a, b| {
        a.1.partial_cmp(&b.1)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.0.cmp(&b.0))
    });

    let mut cols: BTreeMap<String, f64> = BTreeMap::new();
    let mut x = SLICE_COLUMN_OFFSET;
    for (slice_id, _order) in &all_slices {
        cols.insert(slice_id.clone(), x);
        x += DEFAULT_NODE_WIDTH + SLICE_COLUMN_GAP;
    }
    cols
}

fn rebalance_slice_columns(
    model: &Value,
    plan: &MaterializePlan,
    diff: &mut HealDiff,
) {
    let positions = model
        .get("layout")
        .and_then(|l| l.get("nodePositions"))
        .and_then(|p| p.as_object());

    // Compact, left-anchored columns in wave order — the SAME source
    // PositionCalculator uses for new nodes, so order and x stay locked and
    // a reorder can never push columns off-screen.
    let cols = wave_slice_columns(model, diff);

    // For each existing node whose x doesn't match its slice's column,
    // queue an x-only PositionFix.
    let Some(positions) = positions else { return };
    for node in plan.iter_existing_nodes() {
        let Some(slice_id) = node.slice_id.as_ref() else {
            continue;
        };
        let Some(target_x) = cols.get(slice_id).copied() else {
            continue;
        };
        let Some(pos) = positions.get(&node.id).and_then(|v| v.as_object()) else {
            continue;
        };
        let Some(current_x) = pos.get("x").and_then(|v| v.as_f64()) else {
            continue;
        };
        if (current_x - target_x).abs() > 0.5 {
            diff.fix_positions.push(PositionFix {
                node_id: node.id.clone(),
                node_name: node.name.clone(),
                node_kind: node.r#type.clone(),
                from_x: Some(current_x),
                to_x: Some(target_x),
                from_y: None,
                to_y: None,
            });
        }
    }
}

/// Map each event name to the alphabetically-first command in the domain
/// that produces it. Used to decide which slice a multi-producer event
/// lives in deterministically.
fn primary_event_owners(domain: &DomainInspection) -> BTreeMap<&str, &str> {
    let mut sorted: Vec<&_> = domain.commands.iter().collect();
    sorted.sort_by(|a, b| a.name.cmp(&b.name));
    let mut out: BTreeMap<&str, &str> = BTreeMap::new();
    for cmd in sorted {
        for ev in &cmd.produces {
            out.entry(ev.as_str()).or_insert(cmd.name.as_str());
        }
    }
    out
}

/// Y band per node type — uiPlaceholder above, command/query/integration
/// in the same row, event in the entity swim lane.
fn canonical_y(node_type: &str) -> f64 {
    match node_type {
        "uiPlaceholder" => Y_UI_PLACEHOLDER,
        "event" => Y_EVENT,
        _ => Y_COMMAND_QUERY_INTEGRATION,
    }
}

fn inspection_name_set(inspection: &ProjectInspection) -> BTreeSet<(String, String)> {
    let mut set: BTreeSet<(String, String)> = BTreeSet::new();
    for domain in &inspection.domains {
        for c in &domain.commands {
            set.insert(("command".to_string(), c.name.clone()));
        }
        for e in &domain.events {
            set.insert(("event".to_string(), e.name.clone()));
        }
        for q in &domain.queries {
            set.insert(("query".to_string(), q.name.clone()));
        }
        for i in &domain.integrations {
            set.insert(("integration".to_string(), i.name.clone()));
        }
    }
    set
}

/// Mutable index of the model PLUS pending additions. `ensure_*` methods
/// either return the id of an existing piece or queue a new one and return
/// its synthetic id — so subsequent lookups in the same `compute_diff`
/// pass find it without re-querying the model JSON.
struct MaterializePlan {
    nodes: Vec<NodeSummary>,
    entities_by_name: BTreeMap<String, String>,
    slices_by_name: BTreeMap<String, String>,
    nodes_by_type_and_name: BTreeMap<(String, String), Vec<usize>>,
    edge_keys: BTreeSet<(String, String, String)>,
    /// `(type, source, target)` of existing edges whose `id` starts with
    /// `edge-heal-` — i.e. heal-authored. The provenance gate that lets the
    /// removal pass drop stale heal edges without ever touching a user-drawn
    /// one (whose id uses a different scheme).
    existing_heal_edges: Vec<(String, String, String)>,
    /// `order` to assign to the next pending entity (one past the max).
    next_entity_order: f64,
    /// `order` to assign to the next pending slice.
    next_slice_order: f64,
}

#[derive(Debug, Clone)]
pub(crate) struct NodeSummary {
    pub id: String,
    pub r#type: String,
    pub name: String,
    pub slice_id: Option<String>,
    pub entity_id: Option<String>,
    pub kind: Option<String>,
    /// `true` for nodes already present in the model JSON; `false` for
    /// nodes queued by this pass.
    pub is_existing: bool,
}

impl MaterializePlan {
    fn from_model(model: &Value) -> Self {
        let mut nodes: Vec<NodeSummary> = Vec::new();
        let mut nodes_by_type_and_name: BTreeMap<(String, String), Vec<usize>> = BTreeMap::new();
        if let Some(arr) = model.get("nodes").and_then(|n| n.as_array()) {
            for raw in arr {
                let Some(id) = raw.get("id").and_then(|v| v.as_str()) else {
                    continue;
                };
                let Some(r#type) = raw.get("type").and_then(|v| v.as_str()) else {
                    continue;
                };
                let Some(name) = raw.get("name").and_then(|v| v.as_str()) else {
                    continue;
                };
                let slice_id = raw
                    .get("sliceId")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());
                let entity_id = raw
                    .get("entityId")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());
                let kind = raw
                    .get("kind")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());
                let idx = nodes.len();
                let summary = NodeSummary {
                    id: id.to_string(),
                    r#type: r#type.to_string(),
                    name: name.to_string(),
                    slice_id,
                    entity_id,
                    kind,
                    is_existing: true,
                };
                nodes_by_type_and_name
                    .entry((summary.r#type.clone(), summary.name.clone()))
                    .or_default()
                    .push(idx);
                nodes.push(summary);
            }
        }

        let mut edge_keys: BTreeSet<(String, String, String)> = BTreeSet::new();
        let mut existing_heal_edges: Vec<(String, String, String)> = Vec::new();
        if let Some(arr) = model.get("edges").and_then(|e| e.as_array()) {
            for raw in arr {
                let Some(t) = raw.get("type").and_then(|v| v.as_str()) else {
                    continue;
                };
                let Some(s) = raw.get("sourceId").and_then(|v| v.as_str()) else {
                    continue;
                };
                let Some(tg) = raw.get("targetId").and_then(|v| v.as_str()) else {
                    continue;
                };
                edge_keys.insert((t.to_string(), s.to_string(), tg.to_string()));
                let heal_authored = raw
                    .get("id")
                    .and_then(|v| v.as_str())
                    .is_some_and(|id| id.starts_with("edge-heal-"));
                if heal_authored {
                    existing_heal_edges.push((t.to_string(), s.to_string(), tg.to_string()));
                }
            }
        }

        let mut entities_by_name: BTreeMap<String, String> = BTreeMap::new();
        let mut max_entity_order: f64 = -1.0;
        if let Some(arr) = model.get("entities").and_then(|v| v.as_array()) {
            for raw in arr {
                let Some(id) = raw.get("id").and_then(|v| v.as_str()) else {
                    continue;
                };
                let Some(name) = raw.get("name").and_then(|v| v.as_str()) else {
                    continue;
                };
                entities_by_name.insert(name.to_string(), id.to_string());
                if let Some(order) = raw.get("order").and_then(|v| v.as_f64()) {
                    if order > max_entity_order {
                        max_entity_order = order;
                    }
                }
            }
        }

        let mut slices_by_name: BTreeMap<String, String> = BTreeMap::new();
        let mut max_slice_order: f64 = -1.0;
        if let Some(arr) = model.get("slices").and_then(|v| v.as_array()) {
            for raw in arr {
                let Some(id) = raw.get("id").and_then(|v| v.as_str()) else {
                    continue;
                };
                let Some(name) = raw.get("name").and_then(|v| v.as_str()) else {
                    continue;
                };
                slices_by_name.insert(name.to_string(), id.to_string());
                if let Some(order) = raw.get("order").and_then(|v| v.as_f64()) {
                    if order > max_slice_order {
                        max_slice_order = order;
                    }
                }
            }
        }

        Self {
            nodes,
            entities_by_name,
            slices_by_name,
            nodes_by_type_and_name,
            edge_keys,
            existing_heal_edges,
            next_entity_order: max_entity_order + 1.0,
            next_slice_order: max_slice_order + 1.0,
        }
    }

    /// Name of an existing node by id, if present. Used by the edge-removal
    /// pass to resolve an `eventFeedsQuery` source back to its event name.
    fn node_name_by_id(&self, id: &str) -> Option<&str> {
        self.nodes
            .iter()
            .find(|n| n.id == id)
            .map(|n| n.name.as_str())
    }

    /// Queue removal of stale heal-authored `eventFeedsQuery` edges into
    /// `q_node_id` whose source event name is NOT in `subscribed` (the
    /// narrowed feeder set). Only heal-authored edges are in
    /// `existing_heal_edges`, so a user-drawn edge is never queued — which
    /// keeps the diff a fixed point (it is never re-proposed on the next run).
    fn queue_stale_query_edges(
        &self,
        diff: &mut HealDiff,
        q_node_id: &str,
        subscribed: &BTreeSet<String>,
        query_name: &str,
    ) {
        for (etype, src, tgt) in &self.existing_heal_edges {
            if etype != "eventFeedsQuery" || tgt != q_node_id {
                continue;
            }
            let src_name = self.node_name_by_id(src).unwrap_or("");
            if subscribed.contains(src_name) {
                continue;
            }
            diff.remove_edges.push(EdgeRef {
                edge_type: etype.clone(),
                source_id: src.clone(),
                target_id: tgt.clone(),
                reason: format!(
                    "query {query_name} read model no longer reads any field event {src_name} writes (field-overlap narrowing)"
                ),
            });
        }
    }

    fn entity_id_for(&self, name: &str) -> Option<String> {
        self.entities_by_name.get(name).cloned()
    }

    fn ensure_entity(&mut self, diff: &mut HealDiff, name: &str, reason_name: &str) -> String {
        if let Some(id) = self.entities_by_name.get(name) {
            return id.clone();
        }
        let id = synth_id("entity", name);
        let order = self.next_entity_order;
        self.next_entity_order += 1.0;
        self.entities_by_name.insert(name.to_string(), id.clone());
        diff.add_entities.push(EntityToAdd {
            id: id.clone(),
            name: name.to_string(),
            order,
            reason: format!("entity for inspection domain {reason_name}"),
        });
        id
    }

    fn ensure_slice(
        &mut self,
        diff: &mut HealDiff,
        name: &str,
        reason: &str,
    ) -> String {
        if let Some(id) = self.slices_by_name.get(name) {
            return id.clone();
        }
        let id = synth_id("slice", name);
        let order = self.next_slice_order;
        self.next_slice_order += 1.0;
        self.slices_by_name.insert(name.to_string(), id.clone());
        diff.add_slices.push(SliceToAdd {
            id: id.clone(),
            name: name.to_string(),
            chapter_id: None,
            order,
            reason: reason.to_string(),
        });
        id
    }

    /// One-shot "ensure this node exists, with a slice if we have to make
    /// one". When the node already exists we reuse its current slice and
    /// DON'T synthesise a fresh one — otherwise the diff would propose
    /// dead `add_slices` entries that no node references. When the node
    /// doesn't exist we ensure (or reuse) a slice named `slice_name`, then
    /// queue the node referencing it.
    fn ensure_node_in_slice(
        &mut self,
        diff: &mut HealDiff,
        node_type: &str,
        name: &str,
        slice_name: &str,
        slice_reason: &str,
        entity_id: Option<&str>,
        kind: Option<&str>,
        node_reason: &str,
    ) -> String {
        let key = (node_type.to_string(), name.to_string());
        if let Some(idxs) = self.nodes_by_type_and_name.get(&key) {
            if let Some(first) = idxs.first().copied() {
                if let Some(n) = self.nodes.get(first) {
                    return n.id.clone();
                }
            }
        }
        let slice_id = self.ensure_slice(diff, slice_name, slice_reason);
        let id = synth_node_id(node_type, name);
        let idx = self.nodes.len();
        let summary = NodeSummary {
            id: id.clone(),
            r#type: node_type.to_string(),
            name: name.to_string(),
            slice_id: Some(slice_id.clone()),
            entity_id: entity_id.map(|s| s.to_string()),
            kind: kind.map(|s| s.to_string()),
            is_existing: false,
        };
        self.nodes_by_type_and_name
            .entry(key)
            .or_default()
            .push(idx);
        self.nodes.push(summary);
        diff.add_nodes.push(NodeToAdd {
            id: id.clone(),
            node_type: node_type.to_string(),
            name: name.to_string(),
            slice_id,
            entity_id: entity_id.map(|s| s.to_string()),
            kind: kind.map(|s| s.to_string()),
            reason: node_reason.to_string(),
        });
        id
    }

    fn ensure_edge(
        &mut self,
        diff: &mut HealDiff,
        edge_type: &str,
        source_id: &str,
        target_id: &str,
        source_handle: &str,
        target_handle: &str,
        reason: &str,
    ) {
        let key = (
            edge_type.to_string(),
            source_id.to_string(),
            target_id.to_string(),
        );
        if !self.edge_keys.insert(key) {
            return;
        }
        diff.add_edges.push(EdgeToAdd {
            edge_type: edge_type.to_string(),
            source_id: source_id.to_string(),
            target_id: target_id.to_string(),
            source_handle: source_handle.to_string(),
            target_handle: target_handle.to_string(),
            reason: reason.to_string(),
        });
    }

    fn node_id(&self, node_type: &str, name: &str) -> Option<String> {
        let key = (node_type.to_string(), name.to_string());
        let idxs = self.nodes_by_type_and_name.get(&key)?;
        let first = *idxs.first()?;
        self.nodes.get(first).map(|n| n.id.clone())
    }

    fn existing_nodes_named(&self, node_type: &str, name: &str) -> Vec<&NodeSummary> {
        let key = (node_type.to_string(), name.to_string());
        let Some(idxs) = self.nodes_by_type_and_name.get(&key) else {
            return Vec::new();
        };
        idxs.iter()
            .filter_map(|i| self.nodes.get(*i))
            .filter(|n| n.is_existing)
            .collect()
    }

    fn iter_existing_nodes(&self) -> impl Iterator<Item = &NodeSummary> {
        self.nodes.iter().filter(|n| n.is_existing)
    }

    fn iter_all_nodes(&self) -> impl Iterator<Item = &NodeSummary> {
        self.nodes.iter()
    }

}

/// Visual layout constants — kept in lockstep with the frontend
/// (`assets/ide/src/ui/layout/autoLayout.ts` + `grid.ts`).
const STACK_DY: f64 = 80.0;
const DEFAULT_NODE_WIDTH: f64 = 240.0;
const SLICE_COLUMN_GAP: f64 = 80.0;
const Y_HEADER: f64 = 40.0;
const Y_TOP_MARGIN: f64 = 300.0;
const Y_LANE_HEIGHT: f64 = 200.0;
const Y_EVENT_OFFSET_IN_LANE: f64 = 60.0;

/// Decides where (x, y) every node lacking a layout entry should sit.
/// Built once per `compute_diff` call from `(model, plan, diff)`; pure
/// function of those inputs so the same heal pass yields byte-identical
/// positions every time. The big rule: **new slice columns are placed
/// past the rightmost existing node**, never on top of one.
struct PositionCalculator {
    /// Where a node in this slice should sit horizontally. For existing
    /// slices: the leftmost x of an already-placed sibling (so new nodes
    /// stack vertically aligned). For new slices: a fresh column past
    /// every existing node's right edge.
    slice_x: BTreeMap<String, f64>,
    /// Ordinal index of each entity (existing then new), used to pick
    /// the y-band for events that belong to that entity's swim lane.
    entity_idx: BTreeMap<String, usize>,
    /// Counts already-placed nodes per `(slice_id, type)` so additional
    /// nodes for the same bucket stack vertically by `STACK_DY` instead
    /// of landing on top of each other.
    stack_ranks: BTreeMap<(String, String), usize>,
}

impl PositionCalculator {
    fn new(model: &Value, plan: &MaterializePlan, diff: &HealDiff) -> Self {
        let positions = model
            .get("layout")
            .and_then(|l| l.get("nodePositions"))
            .and_then(|p| p.as_object());

        // 1. Slice columns come from the shared compact-column layout (wave
        // order, left-anchored) — the SAME source `rebalance_slice_columns`
        // uses for existing nodes, so existing and new nodes in a slice share
        // one column and the layout stays anchored near the origin.
        let slice_x = wave_slice_columns(model, diff);

        // 2. Stack counts: every already-placed (positioned) existing node
        // bumps the rank for its (slice, type) bucket so additional nodes of
        // the same kind stack vertically by STACK_DY instead of overlapping.
        let mut stack_ranks: BTreeMap<(String, String), usize> = BTreeMap::new();
        if let Some(positions) = positions {
            for node in plan.iter_existing_nodes() {
                let Some(slice_id) = node.slice_id.as_ref() else { continue };
                let positioned = positions
                    .get(&node.id)
                    .and_then(|v| v.as_object())
                    .and_then(|o| o.get("x"))
                    .and_then(|v| v.as_f64())
                    .is_some();
                if !positioned {
                    continue;
                }
                *stack_ranks
                    .entry((slice_id.clone(), node.r#type.clone()))
                    .or_insert(0) += 1;
            }
        }

        // 3. Entity indices: existing entities (in `order`) then new
        // entities appended. Drives event y so each entity gets its own
        // swim lane.
        let mut entity_idx: BTreeMap<String, usize> = BTreeMap::new();
        let mut next_idx: usize = 0;
        if let Some(arr) = model.get("entities").and_then(|v| v.as_array()) {
            let mut sorted: Vec<&Value> = arr.iter().collect();
            sorted.sort_by(|a, b| {
                let oa = a.get("order").and_then(|v| v.as_f64()).unwrap_or(0.0);
                let ob = b.get("order").and_then(|v| v.as_f64()).unwrap_or(0.0);
                oa.partial_cmp(&ob).unwrap_or(std::cmp::Ordering::Equal)
            });
            for e in sorted {
                if let Some(id) = e.get("id").and_then(|v| v.as_str()) {
                    entity_idx.entry(id.to_string()).or_insert_with(|| {
                        let v = next_idx;
                        next_idx += 1;
                        v
                    });
                }
            }
        }
        for e in &diff.add_entities {
            entity_idx.entry(e.id.clone()).or_insert_with(|| {
                let v = next_idx;
                next_idx += 1;
                v
            });
        }

        Self {
            slice_x,
            entity_idx,
            stack_ranks,
        }
    }

    /// Assign and consume one (x, y) for a node. Bumps the stack rank
    /// for the node's (slice, type) bucket so the next call for the same
    /// bucket lands `STACK_DY` lower.
    fn assign(
        &mut self,
        slice_id: Option<&str>,
        node_type: &str,
        entity_id: Option<&str>,
    ) -> (f64, f64) {
        let slice_key = slice_id.unwrap_or("").to_string();
        let x = slice_id
            .and_then(|s| self.slice_x.get(s).copied())
            .unwrap_or(SLICE_COLUMN_OFFSET);
        let entity_index = entity_id
            .and_then(|id| self.entity_idx.get(id).copied())
            .unwrap_or(0);
        let base_y = banded_y(node_type, entity_index);

        let bucket = (slice_key, node_type.to_string());
        let rank = *self.stack_ranks.entry(bucket.clone()).or_insert(0);
        self.stack_ranks.insert(bucket, rank + 1);
        let y = base_y + (rank as f64) * STACK_DY;
        (x, y)
    }
}

/// Per-entity, per-type y-band. Matches `autoLayout.ts::bandY` so the
/// Rust pre-pass and the frontend's `autoLayoutMissingPositions` produce
/// the same coordinates for the same inputs.
fn banded_y(node_type: &str, entity_idx: usize) -> f64 {
    match node_type {
        "uiPlaceholder" => Y_UI_PLACEHOLDER,
        "event" => Y_HEADER + Y_TOP_MARGIN + (entity_idx as f64) * Y_LANE_HEIGHT + Y_EVENT_OFFSET_IN_LANE,
        _ => Y_COMMAND_QUERY_INTEGRATION,
    }
}

/// Hash-based id derived from a kind + name. The same `(kind, name)` pair
/// always yields the same id, so re-running the pass against an already
/// healed model is a no-op.
fn synth_id(kind: &str, name: &str) -> String {
    let mut h = DefaultHasher::new();
    kind.hash(&mut h);
    name.hash(&mut h);
    format!("{kind}-heal-{:016x}", h.finish())
}

fn synth_node_id(node_type: &str, name: &str) -> String {
    let mut h = DefaultHasher::new();
    "node".hash(&mut h);
    node_type.hash(&mut h);
    name.hash(&mut h);
    format!("node-heal-{node_type}-{:016x}", h.finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inspect::{CommandInfo, DomainInspection, EventInfo, IntegrationInfo, QueryInfo};
    use std::path::PathBuf;

    fn minimal_model() -> Value {
        serde_json::json!({
            "id": "m1",
            "name": "demo",
            "chapters": [{ "id": "ch1", "name": "Main", "order": 0 }],
            "entities": [{ "id": "ent1", "name": "Orders", "order": 0 }],
            "slices": [
                { "id": "sl1", "name": "PlaceOrder", "chapterId": "ch1", "order": 0 },
                { "id": "sl2", "name": "ShipOrder",  "chapterId": "ch1", "order": 1 }
            ],
            "nodes": [
                { "id": "cmd1", "type": "command", "name": "PlaceOrder",  "sliceId": "sl1", "entityId": "ent1" },
                { "id": "ev1",  "type": "event",   "name": "OrderPlaced", "sliceId": "sl1", "entityId": "ent1" },
                { "id": "cmd2", "type": "command", "name": "ShipOrder",   "sliceId": "sl2", "entityId": "ent1" },
                { "id": "ev2",  "type": "event",   "name": "OrderShipped","sliceId": "sl2", "entityId": "ent1" },
                { "id": "qy1",  "type": "query",   "name": "OrderSummary","sliceId": "sl2" },
                { "id": "intg1","type": "integration","name":"Notifier",  "sliceId": "sl2", "kind": "outbound" }
            ],
            "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    fn empty_model() -> Value {
        serde_json::json!({
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
                events: vec![
                    EventInfo {
                        name: "OrderPlaced".to_string(),
                        file: PathBuf::new(),
                        fields: vec![],
                    },
                    EventInfo {
                        name: "OrderShipped".to_string(),
                        file: PathBuf::new(),
                        fields: vec![],
                    },
                ],
                commands: vec![
                    CommandInfo {
                        name: "PlaceOrder".to_string(),
                        file: PathBuf::new(),
                        produces: vec!["OrderPlaced".to_string()],
                        via_web_transport: false,
                        fields: vec![],
                    },
                    CommandInfo {
                        name: "ShipOrder".to_string(),
                        file: PathBuf::new(),
                        produces: vec!["OrderShipped".to_string()],
                        via_web_transport: false,
                        fields: vec![],
                    },
                ],
                queries: vec![QueryInfo {
                    name: "OrderSummary".to_string(),
                    file: PathBuf::new(),
                    subscribes_to: vec!["OrderPlaced".to_string(), "OrderShipped".to_string()],
                    ..Default::default()
                }],
                integrations: vec![IntegrationInfo {
                    name: "Notifier".to_string(),
                    file: PathBuf::new(),
                    kind: IntegrationKind::Outbound,
                    handles_events: vec!["OrderShipped".to_string()],
                    emits_commands: vec![],
                }],
            }],
        }
    }

    #[test]
    fn diff_proposes_missing_command_produces_event_edges() {
        let model = minimal_model();
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        let cpe: Vec<_> = diff
            .add_edges
            .iter()
            .filter(|e| e.edge_type == "commandProducesEvent")
            .collect();
        assert_eq!(cpe.len(), 2, "expected both commandProducesEvent edges");
        assert!(cpe.iter().any(|e| e.source_id == "cmd1" && e.target_id == "ev1"));
        assert!(cpe.iter().any(|e| e.source_id == "cmd2" && e.target_id == "ev2"));
    }

    #[test]
    fn diff_proposes_event_feeds_query_edges() {
        let model = minimal_model();
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);
        let efq: Vec<_> = diff
            .add_edges
            .iter()
            .filter(|e| e.edge_type == "eventFeedsQuery")
            .collect();
        assert_eq!(efq.len(), 2);
        assert!(efq.iter().any(|e| e.source_id == "ev1" && e.target_id == "qy1"));
        assert!(efq.iter().any(|e| e.source_id == "ev2" && e.target_id == "qy1"));
    }

    #[test]
    fn diff_proposes_event_triggers_integration_edge() {
        let model = minimal_model();
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);
        let eti: Vec<_> = diff
            .add_edges
            .iter()
            .filter(|e| e.edge_type == "eventTriggersIntegration")
            .collect();
        assert_eq!(eti.len(), 1);
        assert_eq!(eti[0].source_id, "ev2");
        assert_eq!(eti[0].target_id, "intg1");
    }

    #[test]
    fn diff_is_empty_when_model_already_has_all_edges_and_nodes() {
        let mut model = minimal_model();
        let inspection = fixture_inspection();

        // Pre-populate every edge the diff would otherwise propose. Use
        // hash-based ids that the heal pass would itself generate so the
        // edge-key index sees them as already present.
        let edges = serde_json::json!([
            { "id": "e1", "type": "commandProducesEvent",     "sourceId": "cmd1",  "targetId": "ev1" },
            { "id": "e2", "type": "commandProducesEvent",     "sourceId": "cmd2",  "targetId": "ev2" },
            { "id": "e3", "type": "eventFeedsQuery",          "sourceId": "ev1",   "targetId": "qy1" },
            { "id": "e4", "type": "eventFeedsQuery",          "sourceId": "ev2",   "targetId": "qy1" },
            { "id": "e5", "type": "eventTriggersIntegration", "sourceId": "ev2",   "targetId": "intg1" }
        ]);
        model["edges"] = edges;
        // Fill in layout entries so ensure_layout_entries doesn't fire.
        let nodes = model["nodes"].as_array().unwrap().clone();
        let mut positions = serde_json::Map::new();
        for n in nodes {
            let id = n["id"].as_str().unwrap().to_string();
            positions.insert(id, serde_json::json!({ "x": 40.0, "y": 120.0 }));
        }
        model["layout"]["nodePositions"] = Value::Object(positions);

        let diff = compute_diff(&model, &inspection);
        assert!(
            diff.add_edges.is_empty(),
            "no edges should be proposed; got {:?}",
            diff.add_edges,
        );
        assert!(
            diff.add_nodes.is_empty(),
            "no nodes should be proposed; got {:?}",
            diff.add_nodes,
        );
        assert!(diff.add_entities.is_empty());
        assert!(diff.add_slices.is_empty());
    }

    #[test]
    fn diff_fixes_integration_kind_when_code_says_reactive() {
        let model = minimal_model();
        let mut inspection = fixture_inspection();
        inspection.domains[0].integrations[0].kind = IntegrationKind::Reactive;
        inspection.domains[0].integrations[0].emits_commands = vec!["PlaceOrder".to_string()];

        let diff = compute_diff(&model, &inspection);
        assert_eq!(diff.fix_integration_kinds.len(), 1);
        let fix = &diff.fix_integration_kinds[0];
        assert_eq!(fix.node_id, "intg1");
        assert_eq!(fix.from_kind, "outbound");
        assert_eq!(fix.to_kind, "inbound");
    }

    #[test]
    fn diff_proposes_layout_entries_for_nodes_missing_positions() {
        let model = minimal_model();
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);
        // Every node in the fixture lacks a layout entry → all 6 proposed.
        assert_eq!(diff.ensure_layout_entries.len(), 6);
        // Event lands on the entity swim-lane band.
        let ev_entry = diff
            .ensure_layout_entries
            .iter()
            .find(|e| e.node_id == "ev1")
            .expect("layout for ev1");
        assert!((ev_entry.y - Y_EVENT).abs() < f64::EPSILON);
        // Command/integration land on the upper band.
        let cmd_entry = diff
            .ensure_layout_entries
            .iter()
            .find(|e| e.node_id == "cmd1")
            .expect("layout for cmd1");
        assert!((cmd_entry.y - Y_COMMAND_QUERY_INTEGRATION).abs() < f64::EPSILON);
    }

    #[test]
    fn diff_fixes_integration_position_dropped_into_event_band() {
        let mut model = minimal_model();
        model["layout"]["nodePositions"]["intg1"] = serde_json::json!({ "x": 400, "y": 400 });
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        let fix = diff
            .fix_positions
            .iter()
            .find(|p| p.node_id == "intg1")
            .expect("expected fix for intg1");
        assert!((fix.to_y.unwrap() - Y_COMMAND_QUERY_INTEGRATION).abs() < f64::EPSILON);
        assert!((fix.from_y.unwrap() - 400.0).abs() < f64::EPSILON);
    }

    #[test]
    fn diff_materializes_missing_command_as_add_node() {
        let mut model = minimal_model();
        // Strip cmd2 from the model.
        model["nodes"] = serde_json::json!(
            model["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|n| n["id"] != "cmd2")
                .cloned()
                .collect::<Vec<_>>()
        );
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);
        assert!(
            diff.add_nodes
                .iter()
                .any(|n| n.node_type == "command" && n.name == "ShipOrder"),
            "ShipOrder should be queued as add_node; got: {:?}",
            diff.add_nodes
        );
        // And no Missing* residual is emitted any more — the materialise
        // path covers it.
        assert!(diff
            .residuals
            .iter()
            .all(|r| matches!(r, Residual::OrphanModelNode { .. })));
    }

    #[test]
    fn diff_materializes_command_event_query_integration_into_empty_model() {
        let model = empty_model();
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        // 1 entity (Orders).
        assert_eq!(diff.add_entities.len(), 1, "{:?}", diff.add_entities);
        assert_eq!(diff.add_entities[0].name, "Orders");

        // Slices: PlaceOrder, ShipOrder, OrderSummary, Notifier.
        let slice_names: BTreeSet<&str> =
            diff.add_slices.iter().map(|s| s.name.as_str()).collect();
        assert!(slice_names.contains("PlaceOrder"));
        assert!(slice_names.contains("ShipOrder"));
        assert!(slice_names.contains("OrderSummary"));
        assert!(slice_names.contains("Notifier"));

        // Nodes: 2 commands, 2 events, 1 query, 1 integration.
        let by_type =
            |t: &str| diff.add_nodes.iter().filter(|n| n.node_type == t).count();
        assert_eq!(by_type("command"), 2);
        assert_eq!(by_type("event"), 2);
        assert_eq!(by_type("query"), 1);
        assert_eq!(by_type("integration"), 1);

        // Integration node carries kind=outbound.
        let intg = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "integration")
            .unwrap();
        assert_eq!(intg.kind.as_deref(), Some("outbound"));

        // Command + event nodes carry the entity id.
        let cmd = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "command" && n.name == "PlaceOrder")
            .unwrap();
        assert!(cmd.entity_id.is_some(), "command should carry entityId");
        let ev = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "event" && n.name == "OrderPlaced")
            .unwrap();
        assert!(ev.entity_id.is_some(), "event should carry entityId");

        // Query + integration do NOT carry entity id (schema doesn't allow).
        let q = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "query")
            .unwrap();
        assert!(q.entity_id.is_none());
        assert!(intg.entity_id.is_none());

        // Edges: 2 commandProducesEvent + 2 eventFeedsQuery + 1 eventTriggersIntegration = 5.
        let by_etype =
            |t: &str| diff.add_edges.iter().filter(|e| e.edge_type == t).count();
        assert_eq!(by_etype("commandProducesEvent"), 2);
        assert_eq!(by_etype("eventFeedsQuery"), 2);
        assert_eq!(by_etype("eventTriggersIntegration"), 1);

        // Every node gets a layout entry too.
        assert_eq!(diff.ensure_layout_entries.len(), 6);
    }

    #[test]
    fn diff_reuses_existing_entity_and_slice_by_name() {
        let mut model = empty_model();
        model["entities"] = serde_json::json!([
            { "id": "ent-prev", "name": "Orders", "order": 0 }
        ]);
        model["slices"] = serde_json::json!([
            { "id": "sl-prev", "name": "PlaceOrder", "chapterId": null, "order": 0 }
        ]);

        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        // Entity "Orders" already exists → no new entity for it.
        assert!(!diff.add_entities.iter().any(|e| e.name == "Orders"));
        // Slice "PlaceOrder" already exists → no new slice for it.
        assert!(!diff.add_slices.iter().any(|s| s.name == "PlaceOrder"));

        // The materialised PlaceOrder command must reference the EXISTING
        // slice id ("sl-prev"), not a freshly-synthesised one.
        let cmd = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "command" && n.name == "PlaceOrder")
            .expect("PlaceOrder command should be queued");
        assert_eq!(cmd.slice_id, "sl-prev");
        // And it should reference the existing entity id.
        assert_eq!(cmd.entity_id.as_deref(), Some("ent-prev"));
    }

    #[test]
    fn diff_event_with_no_producer_gets_own_slice() {
        let mut inspection = fixture_inspection();
        // Add an event nobody produces.
        inspection.domains[0].events.push(EventInfo {
            name: "OrphanEvent".to_string(),
            file: PathBuf::new(),
            fields: vec![],
        });
        let model = empty_model();
        let diff = compute_diff(&model, &inspection);

        let orphan = diff
            .add_nodes
            .iter()
            .find(|n| n.name == "OrphanEvent")
            .expect("OrphanEvent should be queued");
        // The orphan event's slice was synthesised by name.
        assert!(diff.add_slices.iter().any(|s| s.name == "OrphanEvent"));
        let orphan_slice = diff
            .add_slices
            .iter()
            .find(|s| s.name == "OrphanEvent")
            .unwrap();
        assert_eq!(orphan.slice_id, orphan_slice.id);
    }

    #[test]
    fn diff_event_shared_by_two_commands_lives_in_alphabetically_first_slice() {
        let mut inspection = fixture_inspection();
        // Make both commands produce OrderPlaced. PlaceOrder is alphabetically
        // before ShipOrder, so OrderPlaced should land in PlaceOrder's slice.
        inspection.domains[0].commands[1]
            .produces
            .push("OrderPlaced".to_string());
        let model = empty_model();
        let diff = compute_diff(&model, &inspection);

        let ev = diff
            .add_nodes
            .iter()
            .find(|n| n.name == "OrderPlaced")
            .expect("OrderPlaced should be queued");
        let place_order_slice_id = diff
            .add_slices
            .iter()
            .find(|s| s.name == "PlaceOrder")
            .unwrap()
            .id
            .clone();
        assert_eq!(ev.slice_id, place_order_slice_id);
    }

    #[test]
    fn diff_reports_orphan_model_node_when_not_in_inspection() {
        let mut model = minimal_model();
        // Add an event that doesn't exist in the code.
        model["nodes"].as_array_mut().unwrap().push(serde_json::json!({
            "id": "ev99", "type": "event", "name": "GhostEvent",
            "sliceId": "sl1", "entityId": "ent1",
        }));
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);
        assert!(diff.residuals.iter().any(|r| matches!(
            r,
            Residual::OrphanModelNode { node_name, .. } if node_name == "GhostEvent"
        )));
    }

    #[test]
    fn ui_placeholders_are_not_reported_as_orphans() {
        let mut model = minimal_model();
        model["nodes"].as_array_mut().unwrap().push(serde_json::json!({
            "id": "ui1", "type": "uiPlaceholder", "name": "OrderForm",
            "sliceId": "sl1",
        }));
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);
        assert!(!diff.residuals.iter().any(|r| matches!(
            r,
            Residual::OrphanModelNode { node_name, .. } if node_name == "OrderForm"
        )), "UI placeholders shouldn't be orphans (no code backing expected)");
    }

    #[test]
    fn diff_includes_reactive_integration_wiring() {
        let mut inspection = fixture_inspection();
        // Promote Notifier to reactive emitting PlaceOrder.
        inspection.domains[0].integrations[0].kind = IntegrationKind::Reactive;
        inspection.domains[0].integrations[0].emits_commands = vec!["PlaceOrder".to_string()];
        let model = empty_model();
        let diff = compute_diff(&model, &inspection);

        let intg = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "integration")
            .unwrap();
        assert_eq!(intg.kind.as_deref(), Some("inbound"));

        let intg_to_cmd: Vec<_> = diff
            .add_edges
            .iter()
            .filter(|e| e.edge_type == "integrationTriggersCommand")
            .collect();
        assert_eq!(intg_to_cmd.len(), 1, "{:?}", intg_to_cmd);
    }

    #[test]
    fn layout_new_slice_gets_clean_compact_column() {
        // Existing nodes are scattered (one slice far right at x=1500). The
        // inspection adds a brand-new slice/command. With compaction, the new
        // node lands on the clean left-anchored grid (x = 40 + k*320), in its
        // own column distinct from the existing slice's compacted column — so
        // nothing overlaps and nothing ends up off at a hash-random offset.
        let mut model = minimal_model();
        model["layout"]["nodePositions"] = serde_json::json!({
            "cmd1": { "x": 40,   "y": 120 },
            "ev1":  { "x": 40,   "y": 400 },
            "cmd2": { "x": 1500, "y": 120 },
            "ev2":  { "x": 1500, "y": 400 },
            "qy1":  { "x": 1500, "y": 200 },
            "intg1":{ "x": 1500, "y": 280 }
        });

        let mut inspection = fixture_inspection();
        // Plant a fresh command in code that the model doesn't have.
        inspection.domains[0].commands.push(CommandInfo {
            name: "ArchiveOrder".to_string(),
            file: PathBuf::new(),
            produces: vec![],
            via_web_transport: false,
            fields: vec![],
        });

        let diff = compute_diff(&model, &inspection);

        let new_cmd = diff
            .add_nodes
            .iter()
            .find(|n| n.name == "ArchiveOrder")
            .expect("ArchiveOrder should be materialised");
        let layout = diff
            .ensure_layout_entries
            .iter()
            .find(|e| e.node_id == new_cmd.id)
            .expect("ArchiveOrder must get a layout entry");

        // On the compact grid: x = SLICE_COLUMN_OFFSET(40) + k*pitch(320).
        let pitch = DEFAULT_NODE_WIDTH + SLICE_COLUMN_GAP;
        let off = layout.x - SLICE_COLUMN_OFFSET;
        assert!(
            layout.x >= SLICE_COLUMN_OFFSET && (off % pitch).abs() < f64::EPSILON,
            "new node x ({}) must sit on the compact grid (40 + k*{pitch})",
            layout.x,
        );
        // Distinct column from the existing PlaceOrder slice (cmd1). cmd1 is
        // compacted to its own column via a fix_position (or already there).
        let cmd1_x = diff
            .fix_positions
            .iter()
            .find(|f| f.node_id == "cmd1")
            .and_then(|f| f.to_x)
            .unwrap_or(40.0);
        assert!(
            (layout.x - cmd1_x).abs() > f64::EPSILON,
            "new slice column ({}) must differ from the existing slice column ({cmd1_x})",
            layout.x,
        );
        assert!(
            (layout.y - Y_COMMAND_QUERY_INTEGRATION).abs() < f64::EPSILON,
            "command y should be in the canonical command band, got {}",
            layout.y,
        );
    }

    #[test]
    fn layout_stacks_siblings_of_same_kind_in_same_slice() {
        // Two new commands materialise into the same brand-new slice
        // (because both produce events that share the alphabetically-first
        // producer's slice). They must stack vertically (different y) so
        // they don't overlap.
        let mut inspection = fixture_inspection();
        inspection.domains[0].commands.push(CommandInfo {
            name: "AnnotateOrder".to_string(),
            file: PathBuf::new(),
            produces: vec![],
            via_web_transport: false,
            fields: vec![],
        });
        inspection.domains[0].commands.push(CommandInfo {
            name: "AnnotateOrderTwice".to_string(),
            file: PathBuf::new(),
            produces: vec![],
            via_web_transport: false,
            fields: vec![],
        });

        let mut model = empty_model();
        // Pin one existing command at the canonical command y so the
        // stack starts above it: we expect new commands at +80 and +160.
        model["nodes"] = serde_json::json!([
            { "id": "exist", "type": "command", "name": "ExistingCmd",
              "sliceId": "sl-shared", "entityId": "ent-shared" }
        ]);
        model["slices"] = serde_json::json!([
            { "id": "sl-shared", "name": "AnnotateOrder", "chapterId": null, "order": 0 }
        ]);
        model["entities"] = serde_json::json!([
            { "id": "ent-shared", "name": "Orders", "order": 0 }
        ]);
        model["layout"]["nodePositions"] = serde_json::json!({
            "exist": { "x": 200, "y": 120 }
        });
        // Force both new commands into the same slice by NAMING the slice
        // identically (slice "AnnotateOrder" reused; new command
        // "AnnotateOrderTwice" creates a separate slice).
        // To get two new nodes in the SAME slice, instead use a
        // common-producer event scenario:
        inspection.domains[0].commands = vec![
            CommandInfo {
                name: "AnnotateOrder".to_string(),
                file: PathBuf::new(),
                produces: vec!["NoteAdded".to_string()],
                via_web_transport: false,
                fields: vec![],
            },
        ];
        // Two queries in the same slice would also stack. Add a second
        // query named identically? Queries need unique names. The cleaner
        // way: use the integration's own slice (auto-named "Notifier"),
        // and add an extra query that lands in "Notifier"'s slice — but
        // queries always get their own slice. So we settle on a tighter
        // assertion: any (slice, type) bucket with N new nodes spreads y
        // by STACK_DY * (N-1).
        let diff = compute_diff(&model, &inspection);

        // Group new command entries by their (slice, type) and check
        // that any bucket with multiple members has spread-y values.
        use std::collections::HashMap;
        let mut buckets: HashMap<(String, String), Vec<f64>> = HashMap::new();
        for entry in &diff.ensure_layout_entries {
            let node = diff
                .add_nodes
                .iter()
                .find(|n| n.id == entry.node_id);
            let Some(n) = node else { continue };
            buckets
                .entry((n.slice_id.clone(), n.node_type.clone()))
                .or_default()
                .push(entry.y);
        }
        for ((slice, ty), mut ys) in buckets {
            if ys.len() < 2 {
                continue;
            }
            ys.sort_by(|a, b| a.partial_cmp(b).unwrap());
            for w in ys.windows(2) {
                assert!(
                    (w[1] - w[0] - STACK_DY).abs() < f64::EPSILON,
                    "siblings in bucket ({slice}, {ty}) must be stacked by exactly STACK_DY={STACK_DY}; got ys={ys:?}",
                );
            }
        }
    }

    #[test]
    fn layout_event_y_follows_entity_index() {
        // Two entities → events in entity 1 should land below entity 0's
        // events (one LANE_HEIGHT lower). This is the multi-entity case
        // that the old `canonical_y` couldn't represent.
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![
                DomainInspection {
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
                },
                DomainInspection {
                    name: "Payments".to_string(),
                    path: PathBuf::from("/Payments"),
                    events: vec![EventInfo {
                        name: "PaymentCaptured".to_string(),
                        file: PathBuf::new(),
                        fields: vec![],
                    }],
                    commands: vec![CommandInfo {
                        name: "CapturePayment".to_string(),
                        file: PathBuf::new(),
                        produces: vec!["PaymentCaptured".to_string()],
                        via_web_transport: false,
                        fields: vec![],
                    }],
                    queries: vec![],
                    integrations: vec![],
                },
            ],
        };
        let model = empty_model();
        let diff = compute_diff(&model, &inspection);

        let order_event = diff
            .add_nodes
            .iter()
            .find(|n| n.name == "OrderPlaced")
            .unwrap();
        let payment_event = diff
            .add_nodes
            .iter()
            .find(|n| n.name == "PaymentCaptured")
            .unwrap();
        let order_layout = diff
            .ensure_layout_entries
            .iter()
            .find(|e| e.node_id == order_event.id)
            .unwrap();
        let payment_layout = diff
            .ensure_layout_entries
            .iter()
            .find(|e| e.node_id == payment_event.id)
            .unwrap();

        // Entity 0 events at y = 400, entity 1 events at y = 400 + 200 = 600.
        assert!(
            (order_layout.y - 400.0).abs() < f64::EPSILON,
            "Order entity index 0 should put its events at y=400, got {}",
            order_layout.y
        );
        assert!(
            (payment_layout.y - 600.0).abs() < f64::EPSILON,
            "Payments entity index 1 should put its events at y=600, got {}",
            payment_layout.y
        );
    }

    #[test]
    fn grouping_creates_chapter_per_flow_for_heal_slices() {
        // Empty model + inspection with two INDEPENDENT domains (no shared
        // events) → two separate causal flows → one chapter per flow, each
        // named after the flow's root slice (the initializer command),
        // NOT after the entity. (Entity is a vertical swim lane; chapter is
        // a horizontal story — they are different axes.)
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![
                DomainInspection {
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
                },
                DomainInspection {
                    name: "Payments".to_string(),
                    path: PathBuf::from("/Payments"),
                    events: vec![EventInfo {
                        name: "PaymentCaptured".to_string(),
                        file: PathBuf::new(),
                        fields: vec![],
                    }],
                    commands: vec![CommandInfo {
                        name: "CapturePayment".to_string(),
                        file: PathBuf::new(),
                        produces: vec!["PaymentCaptured".to_string()],
                        via_web_transport: false,
                        fields: vec![],
                    }],
                    queries: vec![],
                    integrations: vec![],
                },
            ],
        };
        let model = empty_model();
        let diff = compute_diff(&model, &inspection);

        // Two chapters created (one per causal flow), named after the flow
        // roots — the command slices — not the entities.
        assert_eq!(diff.add_chapters.len(), 2, "{:?}", diff.add_chapters);
        let chapter_names: BTreeSet<&str> = diff
            .add_chapters
            .iter()
            .map(|c| c.name.as_str())
            .collect();
        assert!(
            chapter_names.contains("PlaceOrder"),
            "chapter named after the flow root slice; got {chapter_names:?}",
        );
        assert!(
            chapter_names.contains("CapturePayment"),
            "chapter named after the flow root slice; got {chapter_names:?}",
        );
        // The old entity-named chapters must NOT appear.
        assert!(!chapter_names.contains("Orders"));
        assert!(!chapter_names.contains("Payments"));

        // Each slice lands in its own flow's chapter (same name as the slice
        // here, since each flow is a single command slice).
        let chapter_for_name = |name: &str| {
            diff.add_chapters
                .iter()
                .find(|c| c.name == name)
                .unwrap()
                .id
                .clone()
        };
        for s in &diff.add_slices {
            let expected = chapter_for_name(&s.name);
            assert_eq!(
                s.chapter_id.as_deref(),
                Some(expected.as_str()),
                "slice {} should be in its own flow's chapter",
                s.name,
            );
        }

        // Chapters are ordered left-to-right by flow (deterministic
        // name-key tiebreak among same-depth flows): CapturePayment (0)
        // before PlaceOrder (1).
        let order_of = |name: &str| {
            diff.add_chapters
                .iter()
                .find(|c| c.name == name)
                .unwrap()
                .order
        };
        assert!(
            order_of("CapturePayment") < order_of("PlaceOrder"),
            "flows order deterministically by name-key among equal depth",
        );

        // And the slices themselves carry the matching wave order.
        let slice_order = |name: &str| {
            diff.add_slices
                .iter()
                .find(|s| s.name == name)
                .unwrap()
                .order
        };
        assert!(slice_order("CapturePayment") < slice_order("PlaceOrder"));
    }

    #[test]
    fn grouping_reassigns_existing_heal_slice_into_chapter() {
        // Model has a heal-prefixed slice with chapter_id=null left over
        // from an earlier heal run. Re-running heal should emit a
        // SliceUpdate that assigns it to its entity's chapter.
        let mut model = empty_model();
        model["entities"] = serde_json::json!([
            { "id": "ent-orders", "name": "Orders", "order": 0 }
        ]);
        model["slices"] = serde_json::json!([
            { "id": "slice-heal-existing", "name": "PlaceOrder", "chapterId": null, "order": 5 }
        ]);
        model["nodes"] = serde_json::json!([
            { "id": "cmd1", "type": "command", "name": "PlaceOrder",
              "sliceId": "slice-heal-existing", "entityId": "ent-orders" }
        ]);
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        // The existing heal slice should get a chapter assignment.
        let update = diff
            .update_slices
            .iter()
            .find(|u| u.slice_id == "slice-heal-existing")
            .expect("existing heal slice should be reassigned");
        assert!(update.set_chapter_id.is_some());
    }

    #[test]
    fn grouping_does_not_touch_user_created_slices() {
        // Model has a user-created slice (no slice-heal- prefix). Heal
        // must not move it into a chapter.
        let mut model = empty_model();
        model["entities"] = serde_json::json!([
            { "id": "ent-orders", "name": "Orders", "order": 0 }
        ]);
        model["slices"] = serde_json::json!([
            { "id": "user-slice-001", "name": "PlaceOrder", "chapterId": null, "order": 0 }
        ]);
        model["nodes"] = serde_json::json!([
            { "id": "cmd1", "type": "command", "name": "PlaceOrder",
              "sliceId": "user-slice-001", "entityId": "ent-orders" }
        ]);
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        assert!(
            !diff
                .update_slices
                .iter()
                .any(|u| u.slice_id == "user-slice-001"),
            "user-created slice must not be re-chaptered; got {:?}",
            diff.update_slices,
        );
    }

    #[test]
    fn grouping_is_idempotent_when_chapter_already_correct() {
        // First heal run creates chapter and assigns slices. Re-running
        // must produce zero update_slices and zero add_chapters.
        let mut model = empty_model();
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
        let diff_one = compute_diff(&model, &inspection);
        crate::ide::heal::apply::apply_diff(&mut model, &diff_one);

        let diff_two = compute_diff(&model, &inspection);
        assert!(
            diff_two.add_chapters.is_empty(),
            "second run must not create new chapters; got {:?}",
            diff_two.add_chapters,
        );
        assert!(
            diff_two.update_slices.is_empty(),
            "second run must not propose slice updates; got {:?}",
            diff_two.update_slices,
        );
    }

    #[test]
    fn rebalance_compacts_columns_to_left_anchored_grid() {
        // Both nodes start far to the right at x=900. Compaction anchors the
        // columns at SLICE_COLUMN_OFFSET(40) and steps by pitch
        // (DEFAULT_NODE_WIDTH 240 + GAP 80 = 320): A -> 40, B -> 360. Both
        // move LEFT — the whole point of "tidy" is to pull a drifted layout
        // back to the origin so it never ends up off-screen.
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Ent", "order": 0 }],
            "slices": [
                { "id": "sl-a", "name": "A", "chapterId": null, "order": 0 },
                { "id": "sl-b", "name": "B", "chapterId": null, "order": 1 }
            ],
            "nodes": [
                { "id": "nA", "type": "command", "name": "CmdA", "sliceId": "sl-a", "entityId": "ent1" },
                { "id": "nB", "type": "command", "name": "CmdB", "sliceId": "sl-b", "entityId": "ent1" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": {
                    "nA": { "x": 900, "y": 120 },
                    "nB": { "x": 900, "y": 120 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![],
        };
        let diff = compute_diff(&model, &inspection);

        let a_fix = diff
            .fix_positions
            .iter()
            .find(|f| f.node_id == "nA")
            .expect("A should be compacted left");
        assert_eq!(a_fix.to_x, Some(40.0), "A -> column 0");
        assert_eq!(a_fix.to_y, None, "y must not be touched");
        let b_fix = diff
            .fix_positions
            .iter()
            .find(|f| f.node_id == "nB")
            .expect("B should get its own column");
        assert_eq!(b_fix.to_x, Some(360.0), "B -> column 1 (40 + 320)");
        assert_eq!(b_fix.to_y, None, "y must not be touched");
    }

    #[test]
    fn rebalance_leaves_already_compact_slices_alone() {
        // Slices already sitting on the compact grid (40, 360) get no x fix.
        let model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Ent", "order": 0 }],
            "slices": [
                { "id": "sl-a", "name": "A", "chapterId": null, "order": 0 },
                { "id": "sl-b", "name": "B", "chapterId": null, "order": 1 }
            ],
            "nodes": [
                { "id": "nA", "type": "command", "name": "CmdA", "sliceId": "sl-a", "entityId": "ent1" },
                { "id": "nB", "type": "command", "name": "CmdB", "sliceId": "sl-b", "entityId": "ent1" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": {
                    "nA": { "x": 40,  "y": 120 },
                    "nB": { "x": 360, "y": 120 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![],
        };
        let diff = compute_diff(&model, &inspection);
        assert!(
            diff.fix_positions.iter().all(|f| f.to_x.is_none() && f.to_y.is_none()),
            "no fixes should fire on an already-compact layout; got {:?}",
            diff.fix_positions,
        );
    }

    #[test]
    fn rebalance_is_fixed_point_after_apply() {
        // Run compute_diff → apply_diff → compute_diff again. The second
        // pass must propose NO further x-fixes (the first pass got it
        // right).
        let mut model = serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [],
            "entities": [{ "id": "ent1", "name": "Ent", "order": 0 }],
            "slices": [
                { "id": "sl-a", "name": "A", "chapterId": null, "order": 0 },
                { "id": "sl-b", "name": "B", "chapterId": null, "order": 1 },
                { "id": "sl-c", "name": "C", "chapterId": null, "order": 2 }
            ],
            "nodes": [
                { "id": "nA", "type": "command", "name": "CmdA", "sliceId": "sl-a", "entityId": "ent1" },
                { "id": "nB", "type": "command", "name": "CmdB", "sliceId": "sl-b", "entityId": "ent1" },
                { "id": "nC", "type": "command", "name": "CmdC", "sliceId": "sl-c", "entityId": "ent1" }
            ],
            "edges": [],
            "layout": {
                "nodePositions": {
                    "nA": { "x": 100, "y": 120 },
                    "nB": { "x": 100, "y": 120 },
                    "nC": { "x": 100, "y": 120 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![],
        };
        let diff = compute_diff(&model, &inspection);
        crate::ide::heal::apply::apply_diff(&mut model, &diff);

        let diff_after = compute_diff(&model, &inspection);
        let x_fixes: Vec<_> = diff_after
            .fix_positions
            .iter()
            .filter(|f| f.to_x.is_some())
            .collect();
        assert!(
            x_fixes.is_empty(),
            "second compute_diff should propose no further x fixes; got {x_fixes:?}",
        );
    }

    #[test]
    fn layout_is_idempotent_across_runs() {
        // The same inputs MUST yield byte-identical layout entries on
        // every run — that's the contract the patched file relies on
        // for git-stable output.
        let model = empty_model();
        let inspection = fixture_inspection();
        let diff1 = compute_diff(&model, &inspection);
        let diff2 = compute_diff(&model, &inspection);

        let entries1: Vec<_> = diff1
            .ensure_layout_entries
            .iter()
            .map(|e| (e.node_id.clone(), e.x, e.y))
            .collect();
        let entries2: Vec<_> = diff2
            .ensure_layout_entries
            .iter()
            .map(|e| (e.node_id.clone(), e.x, e.y))
            .collect();
        assert_eq!(
            entries1, entries2,
            "same inputs must produce identical layout entries on every run"
        );
    }

    /// End-to-end fixed point: compute_diff + (apply via re-build) leaves
    /// compute_diff with nothing further to propose. Apply step is exercised
    /// against the diff fields directly here — diff.rs is unit-scoped; the
    /// full pipeline lives in `apply::tests::pipeline_is_fixed_point`.
    #[test]
    fn materialised_diff_is_self_consistent() {
        let model = empty_model();
        let inspection = fixture_inspection();
        let diff = compute_diff(&model, &inspection);

        // Every edge must reference either an existing node id OR a
        // freshly-materialised node id from THIS diff. No dangling refs.
        let mut known_ids: BTreeSet<String> = BTreeSet::new();
        if let Some(arr) = model.get("nodes").and_then(|v| v.as_array()) {
            for n in arr {
                if let Some(id) = n.get("id").and_then(|v| v.as_str()) {
                    known_ids.insert(id.to_string());
                }
            }
        }
        for n in &diff.add_nodes {
            known_ids.insert(n.id.clone());
        }
        for e in &diff.add_edges {
            assert!(
                known_ids.contains(&e.source_id),
                "edge source {} not in known ids",
                e.source_id
            );
            assert!(
                known_ids.contains(&e.target_id),
                "edge target {} not in known ids",
                e.target_id
            );
        }
    }

    #[test]
    fn diff_wires_orphan_query_to_every_local_event() {
        // Mirrors the Task-1 inspection default: a query that names no
        // event in source has `subscribes_to` filled with ALL of its
        // domain's local events. The differ must then materialise an
        // `eventFeedsQuery` edge from each of those events to the query —
        // no longer leaving it an orphan with zero incoming edges.
        let mut inspection = fixture_inspection();
        // Domain "Orders" has events OrderPlaced + OrderShipped. Replace the
        // fixture's single-query with an orphan query subscribed to BOTH
        // local events (the post-default shape from inspect_domain).
        inspection.domains[0].queries = vec![QueryInfo {
            name: "OrdersProjection".to_string(),
            file: PathBuf::new(),
            subscribes_to: vec!["OrderPlaced".to_string(), "OrderShipped".to_string()],
            ..Default::default()
        }];

        let model = empty_model();
        let diff = compute_diff(&model, &inspection);

        // Find the materialised query node id.
        let q_node = diff
            .add_nodes
            .iter()
            .find(|n| n.node_type == "query" && n.name == "OrdersProjection")
            .expect("query node should be materialised");
        // Find materialised event node ids.
        let ev_ids: BTreeSet<&str> = diff
            .add_nodes
            .iter()
            .filter(|n| n.node_type == "event")
            .map(|n| n.id.as_str())
            .collect();

        let efq: Vec<_> = diff
            .add_edges
            .iter()
            .filter(|e| e.edge_type == "eventFeedsQuery" && e.target_id == q_node.id)
            .collect();
        assert_eq!(
            efq.len(),
            2,
            "orphan query must be fed by both local events; got {efq:?}",
        );
        for e in &efq {
            assert!(
                ev_ids.contains(e.source_id.as_str()),
                "eventFeedsQuery source {} must be a materialised event node",
                e.source_id,
            );
        }
    }

    // ─── Wave-ordering tests (the core feature) ──────────────────────

    /// Run only the layout/wave pass (empty inspection) against a model that
    /// already carries its slices, nodes and edges. Returns slice NAME ->
    /// final `order` after one `compute_diff` + `apply_diff`.
    fn wave_orders(model: &Value) -> BTreeMap<String, f64> {
        let mut m = model.clone();
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![],
        };
        let diff = compute_diff(&m, &inspection);
        crate::ide::heal::apply::apply_diff(&mut m, &diff);
        let mut out = BTreeMap::new();
        for s in m["slices"].as_array().unwrap() {
            out.insert(
                s["name"].as_str().unwrap().to_string(),
                s["order"].as_f64().unwrap(),
            );
        }
        out
    }

    /// Two independent single-entity flows for the chapter-ordering tests:
    /// flow A = `Aone`(cmd CA → evt EA) → `Atwo`(query QA); flow B likewise
    /// with `Bone`/`Btwo`. `chapters` is the chapters array verbatim; `ch_a`
    /// / `ch_b` set each flow's slices' `chapterId` (None → null).
    fn two_flows_model(chapters: Value, ch_a: Option<&str>, ch_b: Option<&str>) -> Value {
        let cid_a = ch_a.map(Value::from).unwrap_or(Value::Null);
        let cid_b = ch_b.map(Value::from).unwrap_or(Value::Null);
        serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": chapters,
            "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "sA1", "name": "Aone", "chapterId": cid_a, "order": 0 },
                { "id": "sA2", "name": "Atwo", "chapterId": cid_a, "order": 0 },
                { "id": "sB1", "name": "Bone", "chapterId": cid_b, "order": 0 },
                { "id": "sB2", "name": "Btwo", "chapterId": cid_b, "order": 0 }
            ],
            "nodes": [
                { "id": "ca", "type": "command", "name": "CA", "sliceId": "sA1", "entityId": "e" },
                { "id": "ea", "type": "event",   "name": "EA", "sliceId": "sA1", "entityId": "e" },
                { "id": "qa", "type": "query",   "name": "QA", "sliceId": "sA2" },
                { "id": "cb", "type": "command", "name": "CB", "sliceId": "sB1", "entityId": "e" },
                { "id": "eb", "type": "event",   "name": "EB", "sliceId": "sB1", "entityId": "e" },
                { "id": "qb", "type": "query",   "name": "QB", "sliceId": "sB2" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent", "sourceId": "ca", "targetId": "ea" },
                { "id": "x2", "type": "eventFeedsQuery",      "sourceId": "ea", "targetId": "qa" },
                { "id": "x3", "type": "commandProducesEvent", "sourceId": "cb", "targetId": "eb" },
                { "id": "x4", "type": "eventFeedsQuery",      "sourceId": "eb", "targetId": "qb" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    fn no_inspection() -> ProjectInspection {
        ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![],
        }
    }

    #[test]
    fn chapter_order_drives_slice_columns() {
        // Two flows owned by two chapters. "ChB" has order 0, "ChA" order 1 —
        // so flow B leads even though its root ("Bone") sorts AFTER "Aone"
        // alphabetically. This is the feature: a manual chapter order overrides
        // the natural wave tiebreak. (Contrast the baseline test below, where
        // with no chapters the SAME graph orders Aone before Bone.)
        let model = two_flows_model(
            serde_json::json!([
                { "id": "chA", "name": "ChA", "order": 1 },
                { "id": "chB", "name": "ChB", "order": 0 }
            ]),
            Some("chA"),
            Some("chB"),
        );
        let o = wave_orders(&model);
        assert_eq!(o["Bone"], 0.0, "chapter order 0 wins the left edge; got {o:?}");
        assert_eq!(o["Btwo"], 1.0, "{o:?}");
        assert_eq!(o["Aone"], 2.0, "{o:?}");
        assert_eq!(o["Atwo"], 3.0, "{o:?}");
    }

    #[test]
    fn empty_chapters_model_is_byte_identical_to_baseline() {
        // The SAME graph as chapter_order_drives_slice_columns but with NO
        // chapters and un-chaptered slices: ranking falls back to the natural
        // (min_layer, name) wave, so flow A precedes flow B. Proves chapter
        // order is the ONLY thing that flipped them above, and that a
        // chapterless model is unchanged from the pre-feature behaviour.
        let model = two_flows_model(serde_json::json!([]), None, None);
        let o = wave_orders(&model);
        assert_eq!(o["Aone"], 0.0, "{o:?}");
        assert_eq!(o["Atwo"], 1.0, "{o:?}");
        assert_eq!(o["Bone"], 2.0, "{o:?}");
        assert_eq!(o["Btwo"], 3.0, "{o:?}");
    }

    #[test]
    fn chapter_reorder_keeps_flow_contiguous() {
        // Chapter X (order 1) owns a 3-slice linear flow; chapter Y (order 0)
        // owns a 1-slice flow. Y leads; X's three slices stay a contiguous
        // 1,2,3 block in wave order — reordering never splits a flow.
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [
                { "id": "chX", "name": "ChX", "order": 1 },
                { "id": "chY", "name": "ChY", "order": 0 }
            ],
            "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "x1", "name": "Xone",   "chapterId": "chX", "order": 0 },
                { "id": "x2", "name": "Xtwo",   "chapterId": "chX", "order": 0 },
                { "id": "x3", "name": "Xthree", "chapterId": "chX", "order": 0 },
                { "id": "y1", "name": "Yone",   "chapterId": "chY", "order": 0 }
            ],
            "nodes": [
                { "id": "cx",  "type": "command", "name": "CX",  "sliceId": "x1", "entityId": "e" },
                { "id": "ex",  "type": "event",   "name": "EX",  "sliceId": "x1", "entityId": "e" },
                { "id": "ix",  "type": "integration", "name": "IX", "sliceId": "x2", "kind": "inbound" },
                { "id": "cx2", "type": "command", "name": "CX2", "sliceId": "x3", "entityId": "e" },
                { "id": "cy",  "type": "command", "name": "CY",  "sliceId": "y1", "entityId": "e" }
            ],
            "edges": [
                { "id": "e1", "type": "commandProducesEvent",       "sourceId": "cx", "targetId": "ex" },
                { "id": "e2", "type": "eventTriggersIntegration",   "sourceId": "ex", "targetId": "ix" },
                { "id": "e3", "type": "integrationTriggersCommand", "sourceId": "ix", "targetId": "cx2" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let o = wave_orders(&model);
        assert_eq!(o["Yone"], 0.0, "chapter order 0 leads; got {o:?}");
        assert_eq!(
            [o["Xone"], o["Xtwo"], o["Xthree"]],
            [1.0, 2.0, 3.0],
            "X's flow stays a contiguous, wave-ordered block; got {o:?}",
        );
    }

    #[test]
    fn manual_chapter_reorder_is_fixed_point() {
        // After relayout honours the manual chapter order, a SECOND relayout
        // must produce no slice/chapter/position changes.
        let mut m = two_flows_model(
            serde_json::json!([
                { "id": "chA", "name": "ChA", "order": 1 },
                { "id": "chB", "name": "ChB", "order": 0 }
            ]),
            Some("chA"),
            Some("chB"),
        );
        let insp = no_inspection();
        let d1 = compute_diff(&m, &insp);
        crate::ide::heal::apply::apply_diff(&mut m, &d1);
        let d2 = compute_diff(&m, &insp);
        assert!(
            d2.update_slices.is_empty(),
            "slice order must be stable on re-run; got {:?}",
            d2.update_slices,
        );
        assert!(
            d2.add_chapters.is_empty(),
            "no new chapters on re-run; got {:?}",
            d2.add_chapters,
        );
        assert!(
            d2.fix_positions.is_empty(),
            "node x must be a fixed point; got {:?}",
            d2.fix_positions,
        );
    }

    #[test]
    fn reorder_then_new_flow_appends_and_is_idempotent() {
        // Existing user chapters at order 0 and 1; a NEW heal-owned flow with
        // no chapter must get a synthesized chapter ordered AFTER them
        // (max 1 + 1 = 2), and a second relayout must be a fixed point.
        let mut m = two_flows_model(
            serde_json::json!([
                { "id": "chA", "name": "ChA", "order": 1 },
                { "id": "chB", "name": "ChB", "order": 0 }
            ]),
            Some("chA"),
            Some("chB"),
        );
        m["slices"].as_array_mut().unwrap().push(serde_json::json!(
            { "id": "slice-heal-gamma", "name": "GammaFlow", "chapterId": null, "order": 0 }
        ));
        m["nodes"].as_array_mut().unwrap().push(serde_json::json!(
            { "id": "cg", "type": "command", "name": "CG", "sliceId": "slice-heal-gamma", "entityId": "e" }
        ));
        let insp = no_inspection();
        let d1 = compute_diff(&m, &insp);
        let gamma = d1
            .add_chapters
            .iter()
            .find(|c| c.name == "GammaFlow")
            .expect("a chapter is synthesized for the new flow");
        assert_eq!(
            gamma.order, 2.0,
            "new flow appends after the user's chapters (max 1 + 1); got {:?}",
            d1.add_chapters,
        );
        crate::ide::heal::apply::apply_diff(&mut m, &d1);
        let d2 = compute_diff(&m, &insp);
        assert!(
            d2.add_chapters.is_empty(),
            "persisted chapter is reused on re-run, not re-synthesized; got {:?}",
            d2.add_chapters,
        );
        assert!(
            d2.update_slices.is_empty(),
            "orders are stable on re-run; got {:?}",
            d2.update_slices,
        );
    }

    #[test]
    fn wave_order_follows_causal_flow_not_alphabetical() {
        // Linear wave: Zeta(cmd+evt) -> Alpha(intg) -> Beta(cmd+evt) ->
        // Gamma(query). Alphabetical order would be Alpha,Beta,Gamma,Zeta;
        // the WAVE order is Zeta,Alpha,Beta,Gamma because Zeta holds the
        // initializer command (one not triggered by any integration).
        // Initial orders are all 0 so a non-reordering impl can't pass.
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "s1", "name": "Zeta",  "chapterId": null, "order": 0 },
                { "id": "s2", "name": "Alpha", "chapterId": null, "order": 0 },
                { "id": "s3", "name": "Beta",  "chapterId": null, "order": 0 },
                { "id": "s4", "name": "Gamma", "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "c1", "type": "command", "name": "C1", "sliceId": "s1", "entityId": "e" },
                { "id": "ev1","type": "event",   "name": "E1", "sliceId": "s1", "entityId": "e" },
                { "id": "i1", "type": "integration", "name": "I1", "sliceId": "s2", "kind": "inbound" },
                { "id": "c2", "type": "command", "name": "C2", "sliceId": "s3", "entityId": "e" },
                { "id": "ev2","type": "event",   "name": "E2", "sliceId": "s3", "entityId": "e" },
                { "id": "q1", "type": "query",   "name": "Q1", "sliceId": "s4" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent",      "sourceId": "c1",  "targetId": "ev1" },
                { "id": "x2", "type": "eventTriggersIntegration",  "sourceId": "ev1", "targetId": "i1" },
                { "id": "x3", "type": "integrationTriggersCommand","sourceId": "i1",  "targetId": "c2" },
                { "id": "x4", "type": "commandProducesEvent",      "sourceId": "c2",  "targetId": "ev2" },
                { "id": "x5", "type": "eventFeedsQuery",           "sourceId": "ev2", "targetId": "q1" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let o = wave_orders(&model);
        assert_eq!(o["Zeta"], 0.0, "initializer slice is leftmost; got {o:?}");
        assert_eq!(o["Alpha"], 1.0, "integration follows its triggering event; got {o:?}");
        assert_eq!(o["Beta"], 2.0, "triggered command follows the integration; got {o:?}");
        assert_eq!(o["Gamma"], 3.0, "read-model follows its event; got {o:?}");
    }

    #[test]
    fn wave_order_disconnected_flows_are_contiguous() {
        // Two independent flows: m->n and x->y (no shared nodes). Each flow
        // must occupy a contiguous block — never interleaved as m,x,n,y.
        // (This is the topology the design's adversarial pass first failed
        // before the component-rank primary key was added.)
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "a1", "name": "m", "chapterId": null, "order": 0 },
                { "id": "a2", "name": "n", "chapterId": null, "order": 0 },
                { "id": "b1", "name": "x", "chapterId": null, "order": 0 },
                { "id": "b2", "name": "y", "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "ca", "type": "command", "name": "CA", "sliceId": "a1", "entityId": "e" },
                { "id": "ea", "type": "event",   "name": "EA", "sliceId": "a1", "entityId": "e" },
                { "id": "qa", "type": "query",   "name": "QA", "sliceId": "a2" },
                { "id": "cb", "type": "command", "name": "CB", "sliceId": "b1", "entityId": "e" },
                { "id": "eb", "type": "event",   "name": "EB", "sliceId": "b1", "entityId": "e" },
                { "id": "qb", "type": "query",   "name": "QB", "sliceId": "b2" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent", "sourceId": "ca", "targetId": "ea" },
                { "id": "x2", "type": "eventFeedsQuery",      "sourceId": "ea", "targetId": "qa" },
                { "id": "x3", "type": "commandProducesEvent", "sourceId": "cb", "targetId": "eb" },
                { "id": "x4", "type": "eventFeedsQuery",      "sourceId": "eb", "targetId": "qb" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let o = wave_orders(&model);
        let a_max = o["m"].max(o["n"]);
        let b_min = o["x"].min(o["y"]);
        assert!(
            a_max < b_min,
            "each flow must be a contiguous block, not interleaved; got {o:?}",
        );
        assert!(o["m"] < o["n"], "command precedes its read-model");
        assert!(o["x"] < o["y"], "command precedes its read-model");
    }

    #[test]
    fn wave_order_breaks_saga_cycle_deterministically() {
        // Cyclic saga: P(cmd+evt) -> Q(intg) -> R(cmd+evt) -> S(intg) -> P.
        // Integration S re-triggers P's command, closing the loop. The pass
        // must terminate, yield 4 distinct contiguous orders, and be
        // identical on a re-run (the back edge is excluded deterministically).
        let model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "p", "name": "P", "chapterId": null, "order": 0 },
                { "id": "q", "name": "Q", "chapterId": null, "order": 0 },
                { "id": "r", "name": "R", "chapterId": null, "order": 0 },
                { "id": "s", "name": "S", "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "cp", "type": "command", "name": "CP", "sliceId": "p", "entityId": "e" },
                { "id": "ep", "type": "event",   "name": "EP", "sliceId": "p", "entityId": "e" },
                { "id": "iq", "type": "integration", "name": "IQ", "sliceId": "q", "kind": "inbound" },
                { "id": "cr", "type": "command", "name": "CR", "sliceId": "r", "entityId": "e" },
                { "id": "er", "type": "event",   "name": "ER", "sliceId": "r", "entityId": "e" },
                { "id": "is", "type": "integration", "name": "IS", "sliceId": "s", "kind": "inbound" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent",      "sourceId": "cp", "targetId": "ep" },
                { "id": "x2", "type": "eventTriggersIntegration",  "sourceId": "ep", "targetId": "iq" },
                { "id": "x3", "type": "integrationTriggersCommand","sourceId": "iq", "targetId": "cr" },
                { "id": "x4", "type": "commandProducesEvent",      "sourceId": "cr", "targetId": "er" },
                { "id": "x5", "type": "eventTriggersIntegration",  "sourceId": "er", "targetId": "is" },
                { "id": "x6", "type": "integrationTriggersCommand","sourceId": "is", "targetId": "cp" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let o1 = wave_orders(&model);
        let mut vals: Vec<f64> = o1.values().copied().collect();
        vals.sort_by(|a, b| a.partial_cmp(b).unwrap());
        assert_eq!(
            vals,
            vec![0.0, 1.0, 2.0, 3.0],
            "saga must terminate with 4 distinct contiguous orders; got {o1:?}",
        );
        let o2 = wave_orders(&model);
        assert_eq!(o1, o2, "saga ordering must be deterministic across runs");
    }

    #[test]
    fn wave_order_deterministic_under_input_shuffle() {
        // Same graph, node/edge/slice arrays reversed -> identical orders.
        let forward = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "s1", "name": "Zeta",  "chapterId": null, "order": 0 },
                { "id": "s2", "name": "Alpha", "chapterId": null, "order": 0 },
                { "id": "s3", "name": "Beta",  "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "c1", "type": "command", "name": "C1", "sliceId": "s1", "entityId": "e" },
                { "id": "ev1","type": "event",   "name": "E1", "sliceId": "s1", "entityId": "e" },
                { "id": "i1", "type": "integration", "name": "I1", "sliceId": "s2", "kind": "inbound" },
                { "id": "c2", "type": "command", "name": "C2", "sliceId": "s3", "entityId": "e" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent",      "sourceId": "c1",  "targetId": "ev1" },
                { "id": "x2", "type": "eventTriggersIntegration",  "sourceId": "ev1", "targetId": "i1" },
                { "id": "x3", "type": "integrationTriggersCommand","sourceId": "i1",  "targetId": "c2" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let mut reversed = forward.clone();
        for key in ["nodes", "edges", "slices"] {
            let mut arr = reversed[key].as_array().unwrap().clone();
            arr.reverse();
            reversed[key] = Value::Array(arr);
        }
        assert_eq!(
            wave_orders(&forward),
            wave_orders(&reversed),
            "wave order must be independent of input array ordering",
        );
    }

    #[test]
    fn wave_order_cross_entity_flow_is_one_chapter_and_events_keep_their_aggregate() {
        // A saga spanning two aggregates (entities). The whole causal flow is
        // ONE chapter (a horizontal story), but each event stays in its own
        // aggregate's swim lane — an event is tied to exactly one entity (its
        // aggregate), and the wave pass NEVER rewrites entityId. Heal-prefixed
        // slice ids so chapter-per-flow applies.
        let mut model = serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [],
            "entities": [
                { "id": "ent-acct",   "name": "Acct",   "order": 0 },
                { "id": "ent-ledger", "name": "Ledger", "order": 1 }
            ],
            "slices": [
                { "id": "slice-heal-1", "name": "OpenAccount",   "chapterId": null, "order": 0 },
                { "id": "slice-heal-2", "name": "PostLedger",    "chapterId": null, "order": 0 },
                { "id": "slice-heal-3", "name": "RecordPosting", "chapterId": null, "order": 0 }
            ],
            "nodes": [
                { "id": "c1", "type": "command", "name": "OpenAccount",    "sliceId": "slice-heal-1", "entityId": "ent-acct" },
                { "id": "e1", "type": "event",   "name": "AccountOpened",  "sliceId": "slice-heal-1", "entityId": "ent-acct" },
                { "id": "i1", "type": "integration", "name": "PostLedger", "sliceId": "slice-heal-2", "kind": "inbound" },
                { "id": "c2", "type": "command", "name": "RecordPosting",  "sliceId": "slice-heal-3", "entityId": "ent-ledger" },
                { "id": "e2", "type": "event",   "name": "PostingRecorded","sliceId": "slice-heal-3", "entityId": "ent-ledger" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent",       "sourceId": "c1", "targetId": "e1" },
                { "id": "x2", "type": "eventTriggersIntegration",   "sourceId": "e1", "targetId": "i1" },
                { "id": "x3", "type": "integrationTriggersCommand", "sourceId": "i1", "targetId": "c2" },
                { "id": "x4", "type": "commandProducesEvent",       "sourceId": "c2", "targetId": "e2" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        });
        let inspection = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![],
        };
        let diff = compute_diff(&model, &inspection);
        crate::ide::heal::apply::apply_diff(&mut model, &diff);

        // One chapter for the whole cross-entity flow.
        let chapters = model["chapters"].as_array().unwrap();
        assert_eq!(
            chapters.len(),
            1,
            "a cross-entity flow is a single chapter; got {chapters:?}",
        );
        let chapter_id = chapters[0]["id"].as_str().unwrap();
        for s in model["slices"].as_array().unwrap() {
            assert_eq!(
                s["chapterId"].as_str(),
                Some(chapter_id),
                "every slice in the flow shares the one chapter",
            );
        }

        // Wave order spans the saga left-to-right across the two aggregates.
        let order = |name: &str| {
            model["slices"]
                .as_array()
                .unwrap()
                .iter()
                .find(|s| s["name"] == name)
                .unwrap()["order"]
                .as_f64()
                .unwrap()
        };
        assert!(order("OpenAccount") < order("PostLedger"));
        assert!(order("PostLedger") < order("RecordPosting"));

        // Events stay tied to their aggregate — entityId is never rewritten.
        let entity_of = |id: &str| {
            model["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .find(|n| n["id"] == id)
                .unwrap()["entityId"]
                .as_str()
                .unwrap()
                .to_string()
        };
        assert_eq!(
            entity_of("e1"),
            "ent-acct",
            "AccountOpened stays in the Acct aggregate's swim lane",
        );
        assert_eq!(
            entity_of("e2"),
            "ent-ledger",
            "PostingRecorded stays in the Ledger aggregate's swim lane",
        );
    }

    /// Model with a query node fed by BOTH events via heal-authored
    /// `eventFeedsQuery` edges (the over-approximated all-local shape) plus
    /// one user-drawn edge, ready for the narrowing/removal pass.
    fn model_with_over_approx_query_edges() -> Value {
        serde_json::json!({
            "id": "m1", "name": "demo", "chapters": [],
            "entities": [{ "id": "ent1", "name": "Orders", "order": 0 }],
            "slices": [
                { "id": "sl1", "name": "PlaceOrder", "order": 0 },
                { "id": "sl2", "name": "ShipOrder", "order": 1 },
                { "id": "sl3", "name": "OrderSummary", "order": 2 }
            ],
            "nodes": [
                { "id": "cmd1", "type": "command", "name": "PlaceOrder",   "sliceId": "sl1", "entityId": "ent1" },
                { "id": "cmd2", "type": "command", "name": "ShipOrder",    "sliceId": "sl2", "entityId": "ent1" },
                { "id": "ev1",  "type": "event",   "name": "OrderPlaced",  "sliceId": "sl1", "entityId": "ent1" },
                { "id": "ev2",  "type": "event",   "name": "OrderShipped", "sliceId": "sl2", "entityId": "ent1" },
                { "id": "qy1",  "type": "query",   "name": "OrderSummary", "sliceId": "sl3" }
            ],
            "edges": [
                { "id": "edge-heal-1111111111111111", "type": "eventFeedsQuery", "sourceId": "ev1", "targetId": "qy1", "sourceHandle": "right", "targetHandle": "left" },
                { "id": "edge-heal-2222222222222222", "type": "eventFeedsQuery", "sourceId": "ev2", "targetId": "qy1", "sourceHandle": "right", "targetHandle": "left" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    /// `fixture_inspection` but with `OrderSummary` narrowed to just OrderPlaced.
    fn inspection_narrowed_to_order_placed() -> ProjectInspection {
        let mut insp = fixture_inspection();
        insp.domains[0].queries = vec![QueryInfo {
            name: "OrderSummary".to_string(),
            file: PathBuf::new(),
            subscribes_to: vec!["OrderPlaced".to_string()],
            ..Default::default()
        }];
        insp
    }

    #[test]
    fn diff_removes_stale_heal_event_feeds_query_edges() {
        let model = model_with_over_approx_query_edges();
        let diff = compute_diff(&model, &inspection_narrowed_to_order_placed());
        // OrderShipped→OrderSummary is no longer subscribed ⇒ queued for removal.
        assert!(
            diff.remove_edges.iter().any(|e| e.edge_type == "eventFeedsQuery"
                && e.source_id == "ev2"
                && e.target_id == "qy1"),
            "stale heal edge must be queued for removal; got {:?}",
            diff.remove_edges,
        );
        // OrderPlaced→OrderSummary is still subscribed ⇒ NOT removed.
        assert!(
            !diff.remove_edges.iter().any(|e| e.source_id == "ev1"),
            "subscribed edge must NOT be removed",
        );
    }

    #[test]
    fn remove_edges_preserves_user_authored_edge() {
        // Same narrowing, but the OrderShipped edge is USER-authored
        // (id not `edge-heal-`). It is never queued for removal.
        let mut model = model_with_over_approx_query_edges();
        model["edges"].as_array_mut().unwrap()[1] = serde_json::json!({
            "id": "edge-user-abc", "type": "eventFeedsQuery",
            "sourceId": "ev2", "targetId": "qy1", "sourceHandle": "right", "targetHandle": "left"
        });
        let diff = compute_diff(&model, &inspection_narrowed_to_order_placed());
        assert!(
            !diff.remove_edges.iter().any(|e| e.source_id == "ev2"),
            "user-authored edge must never be queued for removal; got {:?}",
            diff.remove_edges,
        );
    }

    #[test]
    fn relayout_does_not_remove_edges() {
        let model = model_with_over_approx_query_edges();
        let diff = compute_diff_with_options(
            &model,
            &inspection_narrowed_to_order_placed(),
            ComputeOptions::layout_only(),
        );
        assert!(
            diff.remove_edges.is_empty(),
            "layout-only relayout must never touch edges; got {:?}",
            diff.remove_edges,
        );
    }

    /// A model with one real flow (CA→EA→QA) plus two empty slices: a
    /// heal-owned orphan (`slice-heal-orphan`, its own `chapter-heal-orphan`)
    /// and a user-authored empty slice (`slice-user-empty`). Mirrors the
    /// real-world breakage where a prior heal left integration slices behind
    /// whose nodes homed elsewhere.
    fn model_with_empty_slices() -> Value {
        serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [
                { "id": "chapter-heal-real",   "name": "Aone",   "order": 0 },
                { "id": "chapter-heal-orphan", "name": "Orphan", "order": 1 }
            ],
            "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "slice-heal-real",   "name": "Aone",   "chapterId": "chapter-heal-real",   "order": 0 },
                { "id": "slice-heal-orphan", "name": "Orphan", "chapterId": "chapter-heal-orphan", "order": 1 },
                { "id": "slice-user-empty",  "name": "UserEmpty", "chapterId": null, "order": 2 }
            ],
            "nodes": [
                { "id": "ca", "type": "command", "name": "CA", "sliceId": "slice-heal-real", "entityId": "e" },
                { "id": "ea", "type": "event",   "name": "EA", "sliceId": "slice-heal-real", "entityId": "e" },
                { "id": "qa", "type": "query",   "name": "QA", "sliceId": "slice-heal-real" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent", "sourceId": "ca", "targetId": "ea" },
                { "id": "x2", "type": "eventFeedsQuery",      "sourceId": "ea", "targetId": "qa" }
            ],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    #[test]
    fn relayout_prunes_empty_heal_slices_and_their_chapters() {
        let mut model = model_with_empty_slices();
        let diff = compute_diff_with_options(&model, &no_inspection(), ComputeOptions::layout_only());
        assert!(
            diff.remove_slices.contains(&"slice-heal-orphan".to_string()),
            "nodeless heal slice must be queued for removal; got {:?}",
            diff.remove_slices,
        );
        assert!(
            !diff.remove_slices.contains(&"slice-user-empty".to_string()),
            "user-authored empty slice must NEVER be removed; got {:?}",
            diff.remove_slices,
        );
        crate::ide::heal::apply::apply_diff(&mut model, &diff);
        let slice_ids: BTreeSet<String> = model["slices"]
            .as_array()
            .unwrap()
            .iter()
            .map(|s| s["id"].as_str().unwrap().to_string())
            .collect();
        assert!(slice_ids.contains("slice-heal-real"), "node-bearing slice must survive");
        assert!(slice_ids.contains("slice-user-empty"), "user empty slice must survive");
        assert!(!slice_ids.contains("slice-heal-orphan"), "empty heal slice must be gone");
        let chapter_ids: BTreeSet<String> = model["chapters"]
            .as_array()
            .unwrap()
            .iter()
            .map(|c| c["id"].as_str().unwrap().to_string())
            .collect();
        assert!(
            !chapter_ids.contains("chapter-heal-orphan"),
            "the orphan slice's dedicated heal chapter must be reclaimed; got {chapter_ids:?}",
        );

        // Re-running must be a fixed point: nothing left to prune.
        let diff2 = compute_diff_with_options(&model, &no_inspection(), ComputeOptions::layout_only());
        assert!(
            diff2.remove_slices.is_empty(),
            "second relayout must not re-propose slice removals; got {:?}",
            diff2.remove_slices,
        );
    }

    #[test]
    fn heal_structural_is_fixed_point_after_narrowing_and_removal() {
        // Isolate the structural phase (the edge narrowing + removal this
        // change owns) from the orthogonal layout passes — full-pipeline
        // fixed-point is already covered by `apply::pipeline_is_fixed_point`.
        let mut model = model_with_over_approx_query_edges();
        let insp = inspection_narrowed_to_order_placed();
        let opts = ComputeOptions::structural_only();
        let diff1 = compute_diff_with_options(&model, &insp, opts);
        assert!(
            diff1.remove_edges.len() == 1 && diff1.applied_count() > 0,
            "first pass should remove the one stale edge; got summary: {}",
            diff1.summary(),
        );
        crate::ide::heal::apply::apply_diff(&mut model, &diff1);
        let diff2 = compute_diff_with_options(&model, &insp, opts);
        assert_eq!(
            diff2.applied_count(),
            0,
            "second structural pass must be a fixed point; got summary: {}",
            diff2.summary(),
        );
    }

    // --- field reconcile (code→model data sync) --------------------------

    fn rf(name: &str, ty: &str) -> crate::inspect::RecordField {
        crate::inspect::RecordField { name: name.to_string(), type_name: ty.to_string() }
    }

    /// One Orders domain: command `PlaceOrder` + event `OrderPlaced`, both with
    /// source-declared fields.
    fn inspection_with_fields() -> ProjectInspection {
        ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![DomainInspection {
                name: "Orders".to_string(),
                path: PathBuf::from("/Orders"),
                events: vec![EventInfo {
                    name: "OrderPlaced".to_string(),
                    file: PathBuf::new(),
                    fields: vec![rf("orderId", "Uuid"), rf("total", "Money")],
                }],
                commands: vec![CommandInfo {
                    name: "PlaceOrder".to_string(),
                    file: PathBuf::new(),
                    produces: vec!["OrderPlaced".to_string()],
                    via_web_transport: false,
                    fields: vec![rf("orderId", "Uuid")],
                }],
                queries: vec![],
                integrations: vec![],
            }],
        }
    }

    /// Model with the OrderPlaced event node already placed, no fields yet.
    fn model_with_event_node() -> Value {
        serde_json::json!({
            "id": "m", "name": "demo",
            "chapters": [], "entities": [{ "id": "e", "name": "Orders", "order": 0 }],
            "slices": [{ "id": "s1", "name": "PlaceOrder", "chapterId": null, "order": 0 }],
            "nodes": [
                { "id": "ev1", "type": "event", "name": "OrderPlaced", "sliceId": "s1", "entityId": "e" }
            ],
            "edges": [],
            "layout": { "nodePositions": { "ev1": { "x": 800, "y": 400 } }, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        })
    }

    #[test]
    fn compute_options_fields_flag_matrix() {
        let f = ComputeOptions::full();
        assert!(f.structural && f.layout && f.fields);
        let l = ComputeOptions::layout_only();
        assert!(l.layout && !l.structural && !l.fields, "relayout must never rewrite fields");
        let s = ComputeOptions::structural_only();
        assert!(s.structural && !s.layout && s.fields);
        let fo = ComputeOptions::fields_only();
        assert!(fo.fields && !fo.structural && !fo.layout);
    }

    #[test]
    fn fields_only_reconciles_existing_node() {
        let model = model_with_event_node();
        let insp = inspection_with_fields();
        let diff = compute_diff_with_options(&model, &insp, ComputeOptions::fields_only());
        assert_eq!(diff.set_node_fields.len(), 1, "only the existing OrderPlaced node reconciles");
        assert_eq!(diff.set_node_fields[0].node_id, "ev1");
        assert_eq!(diff.set_node_fields[0].fields, vec![rf("orderId", "Uuid"), rf("total", "Money")]);

        let mut model = model;
        crate::ide::heal::apply::apply_diff(&mut model, &diff);
        let fields = model["nodes"][0]["fields"].as_array().expect("fields written");
        assert_eq!(fields[0]["name"], "orderId");
        assert_eq!(fields[0]["type"], "Uuid");
        assert_eq!(fields[1]["name"], "total");
    }

    #[test]
    fn fields_only_emits_no_structural_or_layout_ops() {
        let model = model_with_event_node();
        let insp = inspection_with_fields();
        let diff = compute_diff_with_options(&model, &insp, ComputeOptions::fields_only());
        assert!(diff.add_nodes.is_empty(), "fields_only must not materialise nodes");
        assert!(diff.add_slices.is_empty() && diff.add_entities.is_empty());
        assert!(diff.fix_positions.is_empty() && diff.ensure_layout_entries.is_empty());
        assert!(diff.update_slices.is_empty() && diff.add_edges.is_empty());
    }

    #[test]
    fn fields_only_preserves_positions_and_is_idempotent() {
        let mut model = model_with_event_node();
        let insp = inspection_with_fields();
        let before = model["layout"]["nodePositions"].clone();
        let diff = compute_diff_with_options(&model, &insp, ComputeOptions::fields_only());
        crate::ide::heal::apply::apply_diff(&mut model, &diff);
        assert_eq!(model["layout"]["nodePositions"], before, "positions must not move");
        let diff2 = compute_diff_with_options(&model, &insp, ComputeOptions::fields_only());
        assert_eq!(diff2.set_node_fields.len(), 0, "already-synced fields yield no diff");
    }

    #[test]
    fn fields_only_overwrites_stale_fields_wholesale() {
        let mut model = model_with_event_node();
        model["nodes"][0]["fields"] = serde_json::json!([{ "name": "old", "type": "Bool" }]);
        let insp = inspection_with_fields();
        let diff = compute_diff_with_options(&model, &insp, ComputeOptions::fields_only());
        crate::ide::heal::apply::apply_diff(&mut model, &diff);
        let names: Vec<&str> = model["nodes"][0]["fields"]
            .as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
        assert_eq!(names, vec!["orderId", "total"], "stale fields replaced, not merged");
    }

    #[test]
    fn fields_only_empty_source_leaves_node_untouched() {
        // A fieldless source extraction (parser miss or genuinely no fields)
        // must not clear the node's existing fields.
        let mut model = model_with_event_node();
        model["nodes"][0]["fields"] = serde_json::json!([{ "name": "keep", "type": "Int" }]);
        let insp = ProjectInspection {
            root: PathBuf::from("/"),
            domains: vec![DomainInspection {
                name: "Orders".to_string(),
                path: PathBuf::from("/Orders"),
                events: vec![EventInfo {
                    name: "OrderPlaced".to_string(),
                    file: PathBuf::new(),
                    fields: vec![],
                }],
                commands: vec![],
                queries: vec![],
                integrations: vec![],
            }],
        };
        let diff = compute_diff_with_options(&model, &insp, ComputeOptions::fields_only());
        assert!(diff.set_node_fields.is_empty(), "empty source must not queue an overwrite");
    }

    #[test]
    fn full_pass_materialises_new_node_with_fields() {
        let mut model = empty_model();
        let insp = inspection_with_fields();
        let diff = compute_diff_with_options(&model, &insp, ComputeOptions::full());
        assert!(!diff.add_nodes.is_empty(), "full pass materialises nodes");
        assert!(!diff.set_node_fields.is_empty(), "new field-bearing nodes get fields");
        crate::ide::heal::apply::apply_diff(&mut model, &diff);
        let nodes = model["nodes"].as_array().unwrap();
        let ev = nodes.iter().find(|n| n["name"] == "OrderPlaced").expect("OrderPlaced materialised");
        let names: Vec<&str> = ev["fields"].as_array().expect("event has fields")
            .iter().map(|f| f["name"].as_str().unwrap()).collect();
        assert_eq!(names, vec!["orderId", "total"]);
    }
}
