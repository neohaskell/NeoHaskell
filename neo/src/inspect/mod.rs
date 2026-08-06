//! Cheap, dumb-parser introspection of a NeoHaskell project layout.
//!
//! The conventions encoded here mirror what the heal-event-model prompt
//! used to ask the agent to grep for at runtime — but doing the grepping
//! in Rust here means:
//!
//!   * the heal prompt can ship with the answers PRE-COMPUTED, so the
//!     agent stops burning tool calls on file discovery and we can demote
//!     from opus to sonnet;
//!   * humans can run `neo inspect` to see the same view themselves;
//!   * the parser is grep-level brittle on purpose — a NeoHaskell project
//!     that follows the testbed convention parses cleanly; one that
//!     doesn't is signalling a deeper problem.
//!
//! What we extract from each domain (`src/<App>/<Domain>/`):
//!
//!   * Commands (`Commands/<Name>.hs`) — their name, the event constructors
//!     they produce (found by scanning the `decide` body), whether they're
//!     reachable from a `WebTransport` (i.e. behind an HTTP route).
//!   * Events — constructor names from `<Domain>Event` data declaration in
//!     `Core.hs` or `Event.hs`.
//!   * Queries (`Queries/<Name>.hs`) — name + the event constructors that
//!     appear in the file (best-guess subscriber set).
//!   * Integrations (`Integrations/<Name>.hs`) — name, kind (outbound vs
//!     reactive), events handled in `handleEvent`, downstream commands
//!     emitted via `Command.Emit`.
//!
//! Output is `serde_json`-serialisable so the heal prompt can splice it
//! in directly without re-formatting.

pub mod parse;

use std::path::{Path, PathBuf};

use serde::Serialize;
use walkdir::WalkDir;

/// Top-level inspection result for a NeoHaskell project root.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ProjectInspection {
    pub root: PathBuf,
    pub domains: Vec<DomainInspection>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DomainInspection {
    pub name: String,
    pub path: PathBuf,
    pub events: Vec<EventInfo>,
    pub commands: Vec<CommandInfo>,
    pub queries: Vec<QueryInfo>,
    pub integrations: Vec<IntegrationInfo>,
}

/// One record field of an event/command payload — `name :: Type` in source.
/// Serialises as `{ "name": ..., "type": ... }` so it drops straight into the
/// event-model schema's `Field` shape (the IDE renders these read-only; source
/// is their author). Extraction is best-effort/"dumb" — see `parse.rs`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RecordField {
    pub name: String,
    #[serde(rename = "type")]
    pub type_name: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EventInfo {
    /// Constructor name (e.g. `OrderPlaced`).
    pub name: String,
    /// File where the constructor was found — usually `Core.hs` or `Event.hs`.
    pub file: PathBuf,
    /// Record fields of this event constructor's payload, in source order.
    /// Empty when the constructor has no inline record (e.g. a payload-module
    /// arm `Foo Bar.Event`) or the dumb parser couldn't extract them.
    #[serde(default)]
    pub fields: Vec<RecordField>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CommandInfo {
    pub name: String,
    pub file: PathBuf,
    /// Event constructor names that appear in the `decide` body.
    /// Cross-referenced against the domain's known event set, so noise
    /// like `Decider`, `Maybe`, etc. is filtered out.
    pub produces: Vec<String>,
    /// `true` if the command file has a `TransportsOf <Cmd> = '[WebTransport ...]`
    /// declaration — i.e. it can be invoked over HTTP.
    pub via_web_transport: bool,
    /// Record fields of the command's payload (`data <Cmd> = <Cmd> { … }`), in
    /// source order. Empty when there's no inline record or the dumb parser
    /// couldn't extract them.
    #[serde(default)]
    pub fields: Vec<RecordField>,
}

#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct QueryInfo {
    pub name: String,
    pub file: PathBuf,
    /// Final subscriber set: explicit event-constructor hits found in the file
    /// body, else the field-overlap / value-level feeders computed in
    /// `resolve_feeders`, else the all-local fallback. One `eventFeedsQuery`
    /// edge is drawn per entry.
    pub subscribes_to: Vec<String>,
    /// Record fields of the query's read-model (`data <Query> = <Query> { … }`),
    /// in source order. Empty when the parser couldn't extract them.
    #[serde(default)]
    pub fields: Vec<RecordField>,
    /// Entity fields the `combine` reads (`entity.<field>`). The read model's
    /// true data dependency on the aggregate. Not serialised — internal to
    /// inspection (no `neo inspect` consumer needs it).
    #[serde(skip)]
    pub reads_entity_fields: Vec<String>,
    /// Scrutinee field when `combine` is `case entity.<field> of …`.
    #[serde(skip)]
    pub case_field: Option<String>,
    /// Pattern values whose `combine` branch is a definitive `NoOp`
    /// (`Some(empty)` = flat case parsed but no NoOp branch; `None` = no
    /// parseable flat case ⇒ caller must not narrow a hub dependency).
    #[serde(skip)]
    pub noop_values: Option<std::collections::BTreeSet<String>>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IntegrationInfo {
    pub name: String,
    pub file: PathBuf,
    pub kind: IntegrationKind,
    /// Event constructors matched in the `handleEvent` case arms.
    pub handles_events: Vec<String>,
    /// Command names emitted via `Command.Emit { command = X { ... } }`.
    /// Empty for plain outbound integrations.
    pub emits_commands: Vec<String>,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum IntegrationKind {
    /// Pure side-effecting integration (HTTP call to an external system, etc.).
    /// `emits_commands` is empty.
    Outbound,
    /// Bridges domains: listens to an event in this domain and raises a
    /// command in another. `emits_commands` is non-empty.
    Reactive,
}

/// Discover and parse every domain under `<root>/src/`. Returns a
/// `ProjectInspection` even when nothing is found (empty domains) so
/// callers can render a stable shape.
///
/// Two phases:
///   1. Collect events per domain (local set per domain).
///   2. Build a GLOBAL event-name set across all domains and pass it to
///      the query + integration parsers. That way an integration whose
///      `handleEvent` references a cross-domain event constructor still
///      reports it as handled — without this, every cross-domain
///      reactive integration loses its incoming edge.
///
/// Commands stay strict (same-domain events only). Their `decide` body
/// is well-scoped and scanning the whole file risks false positives from
/// imports.
pub fn inspect_project(root: &Path) -> ProjectInspection {
    let src = root.join("src");
    if !src.is_dir() {
        return ProjectInspection {
            root: root.to_path_buf(),
            domains: Vec::new(),
        };
    }

    let dirs = discover_domains(&src);
    // Phase 1: events per domain.
    let with_events: Vec<(PathBuf, Vec<EventInfo>)> = dirs
        .into_iter()
        .map(|d| {
            let events = parse::events_in_domain(&d);
            (d, events)
        })
        .collect();

    // Phase 2: build a deduplicated global event name set.
    let mut global_seen: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut global_event_names: Vec<String> = Vec::new();
    for (_, events) in &with_events {
        for e in events {
            if global_seen.insert(e.name.clone()) {
                global_event_names.push(e.name.clone());
            }
        }
    }

    // Build a GLOBAL command name set too, so the integration parser can
    // recognise emitted commands — including cross-domain ones (e.g.
    // `NotifyProposalOfMetricCompletion` emits the Proposal `RecordMetricScore`)
    // — by their record construction.
    let mut cmd_seen: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut global_command_names: Vec<String> = Vec::new();
    for (dir, _) in &with_events {
        for name in parse::command_names_in_domain(dir) {
            if cmd_seen.insert(name.clone()) {
                global_command_names.push(name);
            }
        }
    }

    let domains = with_events
        .into_iter()
        .map(|(dir, events)| {
            inspect_domain(dir, events, &global_event_names, &global_command_names)
        })
        .collect();
    ProjectInspection {
        root: root.to_path_buf(),
        domains,
    }
}

/// A "domain" is any directory that contains `Core.hs` AND at least one of
/// `Commands/`, `Events/`, `Queries/`, `Integrations/`. We walk `src/`
/// depth-first looking for such directories. Nested domains (e.g.
/// `Datalake/Payment/`, `Datalake/Proposal/`) are returned as siblings.
fn discover_domains(src: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in WalkDir::new(src).max_depth(6).into_iter().flatten() {
        if !entry.file_type().is_dir() {
            continue;
        }
        let p = entry.path();
        if !p.join("Core.hs").is_file() && !p.join("Event.hs").is_file() {
            continue;
        }
        let has_subdir = ["Commands", "Events", "Queries", "Integrations"]
            .iter()
            .any(|s| p.join(s).is_dir());
        if has_subdir {
            out.push(p.to_path_buf());
        }
    }
    out.sort();
    out
}

fn inspect_domain(
    dir: PathBuf,
    events: Vec<EventInfo>,
    global_event_names: &[String],
    global_command_names: &[String],
) -> DomainInspection {
    let name = dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("?")
        .to_string();

    let local_event_names: Vec<String> = events.iter().map(|e| e.name.clone()).collect();

    // Commands stay strict (decide body, local events).
    let mut commands = parse::commands_in_domain(&dir, &local_event_names);
    // Queries + integrations get the GLOBAL event set so they catch
    // cross-domain wiring.
    let mut queries = parse::queries_in_domain(&dir, global_event_names);
    let mut integrations =
        parse::integrations_in_domain(&dir, global_event_names, global_command_names);

    // Infer subscriptions for queries that name no event constructor in
    // source. A NeoHaskell read model reads ENTITY FIELDS in its `combine`
    // (never event constructors), so the token scan above comes back empty.
    // Rather than the old "subscribe to ALL local events" over-approximation
    // (which wired every event of an aggregate to every query — a spaghetti
    // hairball), we derive the true feeders by FIELD-LEVEL data flow: an
    // event feeds a query iff it writes an entity field the query reads, with
    // a value-level refinement for the lifecycle/status "hub" field. See
    // `resolve_feeders`. A query that DID name a specific event keeps that
    // scan result — we only fill the gap, never overwrite a real hit.
    if !local_event_names.is_empty() {
        let write_analysis = parse::event_write_sets_in_domain(&dir, &local_event_names);
        let local_set: std::collections::BTreeSet<&str> =
            local_event_names.iter().map(String::as_str).collect();
        for q in &mut queries {
            // A query's raw token scan can catch event names mentioned in
            // COMMENTS, not just real subscriptions — so we do not trust it for
            // LOCAL events. Local feeders come from field-level data flow
            // (`resolve_feeders`). Cross-domain event names, however, are
            // genuine explicit subscriptions the local-only overlap can't see,
            // so we preserve those and union them in.
            let cross_domain: Vec<String> = q
                .subscribes_to
                .iter()
                .filter(|e| !local_set.contains(e.as_str()))
                .cloned()
                .collect();
            let mut feeders = resolve_feeders(q, &local_event_names, &write_analysis);
            for c in cross_domain {
                if !feeders.contains(&c) {
                    feeders.push(c);
                }
            }
            q.subscribes_to = feeders;
        }
    }

    commands.sort_by(|a, b| a.name.cmp(&b.name));
    queries.sort_by(|a, b| a.name.cmp(&b.name));
    integrations.sort_by(|a, b| a.name.cmp(&b.name));

    DomainInspection {
        name,
        path: dir,
        events,
        commands,
        queries,
        integrations,
    }
}

/// Fraction of an aggregate's events that must write a field for it to count
/// as a "hub" (e.g. a `lifecycle`/`status` field bumped on nearly every
/// event). Hub fields don't discriminate between events under plain field
/// overlap, so they get the value-level treatment instead.
const HUB_FIELD_FRACTION: f64 = 0.6;

/// Derive the precise event feeders for a read-model query from field-level
/// data flow, falling back to the all-local over-approximation whenever the
/// source can't be parsed with confidence. Pure function of the parsed
/// inputs — deterministic and order-stable (filters source-ordered
/// `local_event_names`).
///
/// Rules, in order:
///   * No parseable fold OR no entity-field reads ⇒ all-local (no evidence).
///   * `nonhub` feeders = events writing a NON-hub field the query reads.
///   * If the query reads a hub field: it must be the field the `combine`
///     cases on AND that case must be flat (a parseable NoOp set); then a
///     hub-writing event feeds unless its set value is a definitive `NoOp`.
///     Otherwise we cannot prove which hub-writers matter ⇒ all-local (never
///     under-connect a status view).
///   * Empty result ⇒ all-local (never orphan a query).
fn resolve_feeders(
    q: &QueryInfo,
    local_event_names: &[String],
    analysis: &parse::EntityWriteAnalysis,
) -> Vec<String> {
    use std::collections::BTreeSet;
    let all_local = || local_event_names.to_vec();

    if !analysis.fold_found || q.reads_entity_fields.is_empty() {
        return all_local();
    }
    let reads: BTreeSet<&str> = q.reads_entity_fields.iter().map(String::as_str).collect();

    // Hub fields: written by ≥ HUB_FIELD_FRACTION of the aggregate's events.
    let event_count = local_event_names.len().max(1);
    let mut write_count: std::collections::BTreeMap<&str, usize> = std::collections::BTreeMap::new();
    for fields in analysis.writes.values() {
        for f in fields {
            *write_count.entry(f.as_str()).or_insert(0) += 1;
        }
    }
    let threshold = (HUB_FIELD_FRACTION * event_count as f64).ceil() as usize;
    let hubs: BTreeSet<&str> = write_count
        .iter()
        .filter_map(|(f, c)| if *c >= threshold { Some(*f) } else { None })
        .collect();

    let reads_hub: BTreeSet<&str> = reads.intersection(&hubs).copied().collect();

    // Non-hub field witnesses: an event genuinely providing a field the query
    // reads, excluding hub fields.
    let mut feeders: BTreeSet<&str> = BTreeSet::new();
    for name in local_event_names {
        if let Some(written) = analysis.writes.get(name) {
            let provides_nonhub = written
                .iter()
                .any(|f| reads.contains(f.as_str()) && !hubs.contains(f.as_str()));
            if provides_nonhub {
                feeders.insert(name.as_str());
            }
        }
    }

    if !reads_hub.is_empty() {
        // The query depends on a hub field. We can only narrow it when the
        // combine cases on exactly that hub field with a parseable flat NoOp
        // set; otherwise fall back rather than risk under-connecting.
        let narrowable = match (&q.case_field, &q.noop_values) {
            (Some(cf), Some(noop)) => {
                reads_hub.len() == 1 && reads_hub.contains(cf.as_str()) && {
                    let cf = cf.as_str();
                    for name in local_event_names {
                        let writes_hub = analysis
                            .writes
                            .get(name)
                            .is_some_and(|w| w.contains(cf));
                        if !writes_hub {
                            continue;
                        }
                        // Feeds unless its set value is a definitive NoOp.
                        let value = analysis.enum_values.get(name).and_then(|m| m.get(cf));
                        let is_noop = value.is_some_and(|v| noop.contains(v));
                        if !is_noop {
                            feeders.insert(name.as_str());
                        }
                    }
                    true
                }
            }
            _ => false,
        };
        if !narrowable {
            return all_local();
        }
    }

    if feeders.is_empty() {
        return all_local();
    }
    // Preserve source order of `local_event_names`.
    local_event_names
        .iter()
        .filter(|n| feeders.contains(n.as_str()))
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write(root: &Path, rel: &str, body: &str) {
        let full = root.join(rel);
        std::fs::create_dir_all(full.parent().unwrap()).unwrap();
        std::fs::write(full, body).unwrap();
    }

    #[test]
    fn inspect_project_returns_empty_when_no_src() {
        let dir = tempfile::tempdir().unwrap();
        let out = inspect_project(dir.path());
        assert!(out.domains.is_empty());
    }

    #[test]
    fn inspect_discovers_a_domain_with_core_hs_and_commands_dir() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(dir.path(), "src/App/Cart/Commands/AddItem.hs", CMD_ADD_ITEM);
        let out = inspect_project(dir.path());
        assert_eq!(out.domains.len(), 1, "expected one domain, got {out:?}");
        let cart = &out.domains[0];
        assert_eq!(cart.name, "Cart");
        assert_eq!(cart.events.iter().map(|e| &e.name).collect::<Vec<_>>(), ["CartCreated", "ItemAdded"]);
        assert_eq!(cart.commands.len(), 1);
        assert_eq!(cart.commands[0].name, "AddItem");
        assert_eq!(cart.commands[0].produces, vec!["ItemAdded".to_string()]);
    }

    #[test]
    fn inspect_classifies_reactive_integration_when_emits_command() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Integrations/ReserveStock.hs",
            INTEGRATION_REACTIVE,
        );
        let out = inspect_project(dir.path());
        let cart = &out.domains[0];
        assert_eq!(cart.integrations.len(), 1);
        let intg = &cart.integrations[0];
        assert_eq!(intg.name, "ReserveStock");
        assert_eq!(intg.kind, IntegrationKind::Reactive);
        assert_eq!(intg.handles_events, vec!["ItemAdded".to_string()]);
        assert_eq!(intg.emits_commands, vec!["ReserveStockOnAdded".to_string()]);
    }

    /// CIOS callback idiom: the command is built as a record literal in an
    /// `onSuccess` handler whose return type is the command — NO
    /// `Command.Emit`/`emitCommand` keyword. Detected via the known-command
    /// record-construction scan (the command file makes it a known command).
    const INTEGRATION_CALLBACK_EMIT: &str = r#"
module App.Cart.Integrations.Evaluator where
evaluate :: CartEntity -> CartEvent -> Integration.Outbound
evaluate _entity event =
  Integration.batch
    [ OpenRouter.Request
        { onSuccess = onSuccess event
        , onError = onError event
        }
        |> Integration.outbound
    ]

onSuccess :: CartEvent -> Response -> RecordThing
onSuccess event response =
  RecordThing
    { thingId = event.entityId
    , value = 1
    }
"#;

    #[test]
    fn inspect_detects_callback_idiom_emitted_command_via_record_construction() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        // The command file makes `RecordThing` a known command name.
        write(
            dir.path(),
            "src/App/Cart/Commands/RecordThing.hs",
            "module App.Cart.Commands.RecordThing where\ndata RecordThing = RecordThing { thingId :: Uuid, value :: Int }\n",
        );
        write(
            dir.path(),
            "src/App/Cart/Integrations/Evaluator.hs",
            INTEGRATION_CALLBACK_EMIT,
        );
        let out = inspect_project(dir.path());
        let intg = out.domains[0]
            .integrations
            .iter()
            .find(|i| i.name == "Evaluator")
            .expect("Evaluator integration must survive (it emits a command)");
        assert_eq!(intg.kind, IntegrationKind::Reactive);
        assert_eq!(
            intg.emits_commands,
            vec!["RecordThing".to_string()],
            "callback-idiom command built as a record must be detected",
        );
    }

    #[test]
    fn inspect_catches_dispatcher_routed_integration() {
        // CIOS Proposal/ProposalMetricEvaluation pattern: the domain has
        // an `Integrations.hs` dispatcher module that routes events to
        // handler functions imported from `Integrations/<Name>.hs`. The
        // handler files (e.g. EvaluateMetric.hs) themselves don't have
        // `handleEvent` — they have custom-named functions. The dispatcher
        // is the only place that maps events to integrations.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Metric/Core.hs", METRIC_CORE_HS);
        write(
            dir.path(),
            "src/App/Metric/Events/Started.hs",
            METRIC_STARTED_HS,
        );
        write(
            dir.path(),
            "src/App/Metric/Integrations/EvaluateMetric.hs",
            INTEGRATION_CUSTOM_FN,
        );
        write(
            dir.path(),
            "src/App/Metric/Integrations.hs",
            METRIC_DISPATCHER,
        );
        let out = inspect_project(dir.path());
        assert_eq!(out.domains.len(), 1);
        let dom = &out.domains[0];
        let intg = dom
            .integrations
            .iter()
            .find(|i| i.name == "EvaluateMetric")
            .expect("EvaluateMetric integration should be parsed");
        assert!(
            intg.handles_events.contains(&"MetricStarted".to_string()),
            "dispatcher arm `MetricStarted e -> evaluateMetric ...` should record \
             EvaluateMetric as handling MetricStarted; got: {:?}",
            intg.handles_events,
        );
    }

    #[test]
    fn inspect_passes_global_event_set_to_query_parser() {
        // Domain A has event AEvent. Domain B's query mentions AEvent
        // by name. Cross-domain subscription was previously invisible
        // (the query parser saw only B's events); now the GLOBAL event
        // set covers it.
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path(),
            "src/App/A/Core.hs",
            "module App.A.Core where\ndata AEvent = AHappened {} deriving (Generic)\n",
        );
        write(
            dir.path(),
            "src/App/A/Commands/DoA.hs",
            "module App.A.Commands.DoA where\ndecide _ _ _ = Decider.acceptExisting [AHappened {}]\n",
        );
        write(
            dir.path(),
            "src/App/B/Core.hs",
            "module App.B.Core where\ndata BEvent = BDone {} deriving (Generic)\n",
        );
        write(
            dir.path(),
            "src/App/B/Queries/CrossView.hs",
            "module App.B.Queries.CrossView where\n-- references AHappened from another domain\nview = AHappened\n",
        );
        let out = inspect_project(dir.path());
        let b_query = out
            .domains
            .iter()
            .find(|d| d.name == "B")
            .and_then(|d| d.queries.iter().find(|q| q.name == "CrossView"))
            .expect("CrossView query should be parsed");
        assert!(
            b_query.subscribes_to.contains(&"AHappened".to_string()),
            "cross-domain event must be in subscribes_to; got: {:?}",
            b_query.subscribes_to,
        );
    }

    #[test]
    fn inspect_defaults_orphan_query_subscriptions_to_local_events() {
        // A query that reads an entity projection (CIOS EvaluatedProposal
        // pattern) names no event constructor in source. Its `subscribes_to`
        // would be empty after the token scan — rendering it as an orphan.
        // We default it to the domain's own local event constructors so the
        // differ can wire `eventFeedsQuery` edges automatically.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Queries/CartView.hs",
            // Reads entity-projection fields, names no event.
            "module App.Cart.Queries.CartView where\nview entity = entity.itemCount\n",
        );
        let out = inspect_project(dir.path());
        let cart = &out.domains[0];
        let q = cart
            .queries
            .iter()
            .find(|q| q.name == "CartView")
            .expect("CartView query should be parsed");
        assert_eq!(
            q.subscribes_to,
            vec!["CartCreated".to_string(), "ItemAdded".to_string()],
            "orphan query must default to the domain's local events; got {:?}",
            q.subscribes_to,
        );
    }

    #[test]
    fn inspect_local_comment_mention_does_not_pin_subscription() {
        // A LOCAL event name appearing only in a comment must NOT pin the
        // subscription — query files describe their projection in prose that
        // references events, and that is not a real data dependency. With no
        // parseable entity fold here, the query falls back to all-local (the
        // safe over-approximation), NOT to just the comment-mentioned event.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Queries/AddedItems.hs",
            "module App.Cart.Queries.AddedItems where\n-- driven by ItemAdded\nview = ItemAdded\n",
        );
        let out = inspect_project(dir.path());
        let cart = &out.domains[0];
        let q = cart
            .queries
            .iter()
            .find(|q| q.name == "AddedItems")
            .expect("AddedItems query should be parsed");
        assert_eq!(
            q.subscribes_to,
            vec!["CartCreated".to_string(), "ItemAdded".to_string()],
            "a local comment mention must not pin the subscription; got {:?}",
            q.subscribes_to,
        );
    }

    /// A Cart aggregate with an `update` fold: `CartCreated` writes
    /// `ownerId` + `itemCount`; `ItemAdded` writes only `itemCount`. Lets the
    /// field-overlap path narrow precisely.
    const ENTITY_HS_WITH_FOLD: &str = r#"
module App.Cart.Entity where
data CartEntity = CartEntity { ownerId :: Text, itemCount :: Int }
update :: CartEvent -> CartEntity -> CartEntity
update event entity =
  case event of
    CartCreated e ->
      CartEntity
        { ownerId = e.ownerId
        , itemCount = 0
        }
    ItemAdded e ->
      entity
        { itemCount = entity.itemCount + e.quantity
        }
"#;

    #[test]
    fn inspect_field_overlap_narrows_to_writing_events() {
        // OwnerView reads only `entity.ownerId`, which only `CartCreated`
        // writes — so field-overlap narrows it to that single event instead
        // of the all-local pair. `ItemAdded` (writes only `itemCount`) drops.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(dir.path(), "src/App/Cart/Entity.hs", ENTITY_HS_WITH_FOLD);
        write(
            dir.path(),
            "src/App/Cart/Queries/OwnerView.hs",
            "module App.Cart.Queries.OwnerView where\n  combine entity _ = Update OwnerView { owner = entity.ownerId }\n",
        );
        let out = inspect_project(dir.path());
        let q = out.domains[0]
            .queries
            .iter()
            .find(|q| q.name == "OwnerView")
            .expect("OwnerView parsed");
        assert_eq!(
            q.subscribes_to,
            vec!["CartCreated".to_string()],
            "field-overlap must wire only the event writing a read field; got {:?}",
            q.subscribes_to,
        );
    }

    #[test]
    fn inspect_falls_back_to_all_local_when_no_entity_fold() {
        // A read model whose domain has no parseable `update`/`evolve` fold
        // cannot be narrowed (no positive evidence) — keep all-local.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Queries/ItemView.hs",
            "module App.Cart.Queries.ItemView where\n  combine entity _ = Update ItemView { n = entity.itemCount }\n",
        );
        let out = inspect_project(dir.path());
        let q = out.domains[0]
            .queries
            .iter()
            .find(|q| q.name == "ItemView")
            .expect("ItemView parsed");
        assert_eq!(
            q.subscribes_to,
            vec!["CartCreated".to_string(), "ItemAdded".to_string()],
            "no entity fold ⇒ all-local fallback; got {:?}",
            q.subscribes_to,
        );
    }

    #[test]
    fn inspect_value_level_drops_noop_states() {
        // A status read model that `Update`s only on `Open` and `NoOp`s on
        // `Closed` is fed only by the event that opens it — even though the
        // `status` hub field is written by every event.
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path(),
            "src/App/Ticket/Core.hs",
            "module App.Ticket.Core where\ndata TicketEvent = Opened {} | Closed {} deriving (Generic)\n",
        );
        write(
            dir.path(),
            "src/App/Ticket/Entity.hs",
            "module App.Ticket.Entity where\nupdate :: TicketEvent -> TicketEntity -> TicketEntity\nupdate event entity =\n  case event of\n    Opened e -> entity { status = Open }\n    Closed e -> entity { status = Closed }\n",
        );
        write(
            dir.path(),
            "src/App/Ticket/Queries/OpenTickets.hs",
            "module App.Ticket.Queries.OpenTickets where\n  combine entity _ =\n    case entity.status of\n      Open -> Update OpenTickets { id = entity.ticketId }\n      Closed -> NoOp\n",
        );
        let out = inspect_project(dir.path());
        let q = out
            .domains
            .iter()
            .find(|d| d.name == "Ticket")
            .and_then(|d| d.queries.iter().find(|q| q.name == "OpenTickets"))
            .expect("OpenTickets parsed");
        assert_eq!(
            q.subscribes_to,
            vec!["Opened".to_string()],
            "value-level must drop the NoOp (Closed) state's event; got {:?}",
            q.subscribes_to,
        );
    }

    #[test]
    fn inspect_value_level_is_deterministic_under_event_reorder() {
        // Same project, events declared in a different order ⇒ identical feeders.
        let build = |evt_decl: &str| {
            let dir = tempfile::tempdir().unwrap();
            write(
                dir.path(),
                "src/App/Ticket/Core.hs",
                &format!("module App.Ticket.Core where\ndata TicketEvent = {evt_decl} deriving (Generic)\n"),
            );
            write(
                dir.path(),
                "src/App/Ticket/Entity.hs",
                "module App.Ticket.Entity where\nupdate :: TicketEvent -> TicketEntity -> TicketEntity\nupdate event entity =\n  case event of\n    Opened e -> entity { status = Open }\n    Closed e -> entity { status = Closed }\n",
            );
            write(
                dir.path(),
                "src/App/Ticket/Queries/OpenTickets.hs",
                "module App.Ticket.Queries.OpenTickets where\n  combine entity _ =\n    case entity.status of\n      Open -> Update OpenTickets { id = entity.ticketId }\n      Closed -> NoOp\n",
            );
            let out = inspect_project(dir.path());
            out.domains
                .iter()
                .find(|d| d.name == "Ticket")
                .and_then(|d| d.queries.iter().find(|q| q.name == "OpenTickets"))
                .unwrap()
                .subscribes_to
                .clone()
        };
        assert_eq!(build("Opened {} | Closed {}"), build("Closed {} | Opened {}"));
    }

    #[test]
    fn inspect_drops_plumbing_only_integration() {
        // An integration that handles no event and emits no command is a
        // pure helper module (HTTP client, codec) — not an event-model
        // integration. The healer must not emit a node for it.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Integrations/HttpHelper.hs",
            INTEGRATION_PLUMBING_ONLY,
        );
        // A real reactive integration alongside it, to prove we drop only
        // the plumbing module and keep the genuine one.
        write(
            dir.path(),
            "src/App/Cart/Integrations/ReserveStock.hs",
            INTEGRATION_REACTIVE,
        );
        let out = inspect_project(dir.path());
        let cart = &out.domains[0];
        let names: Vec<&str> = cart.integrations.iter().map(|i| i.name.as_str()).collect();
        assert!(
            !names.contains(&"HttpHelper"),
            "plumbing-only integration must be dropped; got {names:?}",
        );
        assert!(
            names.contains(&"ReserveStock"),
            "genuine reactive integration must be kept; got {names:?}",
        );
    }

    #[test]
    fn inspect_keeps_event_handling_integration_that_emits_no_command() {
        // An outbound integration that HANDLES an event but emits no command
        // (e.g. a Brevo email call triggered by an event) has non-empty
        // handles_events and MUST be kept — only BOTH-empty plumbing drops.
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Integrations/EmailCart.hs",
            INTEGRATION_OUTBOUND,
        );
        let out = inspect_project(dir.path());
        let cart = &out.domains[0];
        let intg = cart
            .integrations
            .iter()
            .find(|i| i.name == "EmailCart")
            .expect("event-handling outbound integration must be kept");
        assert!(intg.emits_commands.is_empty());
        assert_eq!(intg.handles_events, vec!["ItemAdded".to_string()]);
    }

    #[test]
    fn inspect_classifies_outbound_integration_when_no_command_emit() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "src/App/Cart/Core.hs", CORE_HS);
        write(
            dir.path(),
            "src/App/Cart/Integrations/EmailCart.hs",
            INTEGRATION_OUTBOUND,
        );
        let out = inspect_project(dir.path());
        let intg = &out.domains[0].integrations[0];
        assert_eq!(intg.kind, IntegrationKind::Outbound);
        assert!(intg.emits_commands.is_empty());
        assert_eq!(intg.handles_events, vec!["ItemAdded".to_string()]);
    }

    const CORE_HS: &str = r#"
module App.Cart.Core where
data CartEvent
  = CartCreated { entityId :: Uuid, ownerId :: Text }
  | ItemAdded { entityId :: Uuid, stockId :: Uuid, quantity :: Int }
  deriving (Generic)
"#;

    const CMD_ADD_ITEM: &str = r#"
module App.Cart.Commands.AddItem where
data AddItem = AddItem { cartId :: Uuid, stockId :: Uuid, quantity :: Int }
decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide cmd entity _ctx = case entity of
  Nothing -> Decider.reject "Cart not found!"
  Just cart -> Decider.acceptExisting
    [ ItemAdded
        { entityId = cart.cartId
        , stockId = cmd.stockId
        , quantity = cmd.quantity
        }
    ]
type instance TransportsOf AddItem = '[WebTransport]
command ''AddItem
"#;

    const INTEGRATION_REACTIVE: &str = r#"
module App.Cart.Integrations.ReserveStock where
handleEvent :: CartEntity -> CartEvent -> Integration.Outbound
handleEvent cart event = case event of
  ItemAdded { stockId, quantity } ->
    Integration.batch
      [ Integration.outbound
          Command.Emit
            { command = ReserveStockOnAdded { stockId = stockId } }
      ]
  _ -> Integration.none
outboundIntegration ''ReserveStock
"#;

    const METRIC_CORE_HS: &str = r#"
module App.Metric.Core where
data MetricEvent
  = MetricStarted Started.Event
  | MetricCompleted Completed.Event
  deriving (Generic)
"#;

    const METRIC_STARTED_HS: &str = r#"
module App.Metric.Events.Started (Event (..)) where
data Event = Event { entityId :: Uuid } deriving (Generic)
"#;

    const INTEGRATION_CUSTOM_FN: &str = r#"
module App.Metric.Integrations.EvaluateMetric (evaluateMetric) where
import App.Metric.Events.Started qualified as Started

evaluateMetric :: MetricEntity -> Started.Event -> Integration.Outbound
evaluateMetric _entity event =
  Integration.send (httpPost "/eval" event)
"#;

    const METRIC_DISPATCHER: &str = r#"
module App.Metric.Integrations where
import App.Metric.Integrations.EvaluateMetric (evaluateMetric)

metricIntegrations :: MetricEntity -> MetricEvent -> Integration.Outbound
metricIntegrations entity event = case event of
  MetricStarted e ->
    evaluateMetric entity e
  MetricCompleted _ ->
    Integration.none
"#;

    const INTEGRATION_OUTBOUND: &str = r#"
module App.Cart.Integrations.EmailCart where
handleEvent :: CartEntity -> CartEvent -> Integration.Outbound
handleEvent _cart event = case event of
  ItemAdded { stockId } -> Integration.send (postJson "/email" stockId)
  _ -> Integration.none
"#;

    // Pure HTTP-helper module: no `handleEvent`, no `Command.Emit`, no
    // `Integration.emitCommand`. Mirrors CIOS Payment's `BankHttp`/`EvocaBank`.
    const INTEGRATION_PLUMBING_ONLY: &str = r#"
module App.Cart.Integrations.HttpHelper (postForm) where
import Network.HTTP.Client qualified as HttpClient

postForm :: Text -> Array (Text, Text) -> Task err Response
postForm url pairs = Task.fromIO (runRequest url pairs)
"#;
}
