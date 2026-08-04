//! Dumb-string-search parser for a single NeoHaskell domain directory.
//!
//! We treat `.hs` files as text. The NeoHaskell convention puts each
//! command/query/integration in its own file with a known function name
//! (`decide`, `handleEvent`) — we extract a "function body" by finding
//! the function header and slurping until the next top-level definition
//! (next line starting at column 0 with a non-comment identifier or
//! a module-level keyword). Inside that body, constructor matching is
//! a plain-substring scan filtered against the known event/command name
//! set, so we don't need a real Haskell lexer.
//!
//! False positives ARE possible (e.g. `ItemAdded` appearing in a doc
//! comment) but our scope is "give the heal prompt a 90%-accurate
//! cross-reference table" — not a verifier. The agent can still spot-
//! check anything that looks off.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use regex::Regex;

use super::{CommandInfo, EventInfo, IntegrationInfo, IntegrationKind, QueryInfo, RecordField};

/// Parse `<dir>/Core.hs` and `<dir>/Event.hs` (whichever exists) for the
/// event sum constructors. Returns them in source order so the heal
/// prompt can list events the way the developer arranged them.
pub fn events_in_domain(dir: &Path) -> Vec<EventInfo> {
    let mut out = Vec::new();
    // Payload-module names referenced by the event sum type's arms. In the
    // CIOS convention an arm reads `ProposalPdfTranscribed PdfTranscribed.Event`
    // — `ProposalPdfTranscribed` is the canonical event constructor and
    // `PdfTranscribed` is merely the payload module living at
    // `Events/PdfTranscribed.hs`. We collect those payload names so the
    // `Events/` directory scan below does NOT mint them as phantom events
    // (which used to double every such event: once as the constructor, once
    // as the bare file stem, with one copy left dangling as a dead-end leaf).
    let mut payload_modules: BTreeSet<String> = BTreeSet::new();

    for fname in ["Core.hs", "Event.hs"] {
        let path = dir.join(fname);
        if !path.is_file() {
            continue;
        }
        let Ok(body) = std::fs::read_to_string(&path) else {
            continue;
        };
        let ctor_fields = extract_event_constructor_fields(&body);
        let ctor_payloads = extract_event_constructor_payload_modules(&body);
        for name in extract_event_constructors(&body) {
            // Dedup across Core.hs + Event.hs without disturbing source order.
            if !out.iter().any(|e: &EventInfo| e.name == name) {
                let mut fields = ctor_fields.get(&name).cloned().unwrap_or_default();
                // Payload-module arm (`Ctor Module.Event`) — the real fields live
                // in `Events/<Module>.hs` as `data Event = Event { … }`.
                if fields.is_empty() {
                    if let Some(module) = ctor_payloads.get(&name) {
                        let payload = dir.join("Events").join(format!("{module}.hs"));
                        if let Ok(pbody) = std::fs::read_to_string(&payload) {
                            fields = first_data_record_fields(&pbody);
                        }
                    }
                }
                out.push(EventInfo {
                    name,
                    file: path.clone(),
                    fields,
                });
            }
        }
        payload_modules.extend(extract_event_payload_modules(&body));
    }

    let constructor_set: BTreeSet<String> = out.iter().map(|e| e.name.clone()).collect();

    // Also scan one-event-per-file layout: Events/<Name>.hs. A file stem is
    // only a standalone event when it is NOT a payload module of an existing
    // constructor. When `constructor == payload module` (e.g.
    // `EvaluationTriggered EvaluationTriggered.Event`) the stem matches a
    // constructor and is kept (the exact-name dedup keeps it single). When
    // there is no sum type at all (older one-event-per-file projects),
    // `payload_modules` is empty and every stem is kept — preserving the
    // original behaviour.
    let events_dir = dir.join("Events");
    if events_dir.is_dir() {
        let mut entries: Vec<PathBuf> = std::fs::read_dir(&events_dir)
            .into_iter()
            .flatten()
            .flatten()
            .map(|e| e.path())
            .collect();
        // Deterministic order regardless of filesystem enumeration order.
        entries.sort();
        for path in entries {
            if path.extension().and_then(|s| s.to_str()) != Some("hs") {
                continue;
            }
            let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };
            if payload_modules.contains(stem) && !constructor_set.contains(stem) {
                // It's the payload of an entity-prefixed constructor we
                // already recorded — not its own event.
                continue;
            }
            if !out.iter().any(|e| e.name == stem) {
                // One-event-per-file layout: pull the fields from the file's own
                // `data Event = Event { … }` declaration.
                let fields = std::fs::read_to_string(&path)
                    .ok()
                    .map(|b| first_data_record_fields(&b))
                    .unwrap_or_default();
                out.push(EventInfo {
                    name: stem.to_string(),
                    file: path,
                    fields,
                });
            }
        }
    }

    out
}

pub fn commands_in_domain(dir: &Path, known_events: &[String]) -> Vec<CommandInfo> {
    list_dot_hs(dir.join("Commands").as_path())
        .into_iter()
        .filter_map(|path| parse_command_file(&path, known_events))
        .collect()
}

/// Just the command constructor names under `<dir>/Commands/` (recursively,
/// so `Commands/Internal/Foo.hs` counts). Cheap pre-pass used to build the
/// GLOBAL command set so the integration parser can recognise emitted
/// commands by their record construction — including cross-domain ones.
pub fn command_names_in_domain(dir: &Path) -> Vec<String> {
    list_dot_hs(dir.join("Commands").as_path())
        .into_iter()
        .filter_map(|p| p.file_stem().and_then(|s| s.to_str()).map(String::from))
        .collect()
}

pub fn queries_in_domain(dir: &Path, known_events: &[String]) -> Vec<QueryInfo> {
    list_dot_hs(dir.join("Queries").as_path())
        .into_iter()
        .filter_map(|path| parse_query_file(&path, known_events))
        .collect()
}

pub fn integrations_in_domain(
    dir: &Path,
    known_events: &[String],
    known_commands: &[String],
) -> Vec<IntegrationInfo> {
    // Two passes; results UNIONed by integration name:
    //
    // PASS A — Per-file `handleEvent`. Each `Integrations/<Name>.hs` file
    // that defines its OWN `handleEvent :: ... -> Integration.Outbound`
    // (Payment-style in CIOS, also the testbed convention) is parsed
    // standalone — strict arm-by-arm scan of the `case event of` block,
    // arms that resolve to `Integration.none` are not counted.
    //
    // PASS B — Domain dispatcher `<Domain>/Integrations.hs`. When this
    // module exists (Proposal / ProposalMetricEvaluation style in CIOS),
    // it imports each handler function from its sub-module and routes
    // events to handlers via a single `case event of` block. We map the
    // import statements (`fnName` → `IntgName`) and walk the dispatcher's
    // case arms to figure out which integration handles which event,
    // without depending on the dispatcher function's name.
    let mut by_name: std::collections::BTreeMap<String, IntegrationInfo> =
        std::collections::BTreeMap::new();
    for path in list_dot_hs(dir.join("Integrations").as_path()) {
        if let Some(info) = parse_integration_file(&path, known_events, known_commands) {
            by_name.insert(info.name.clone(), info);
        }
    }
    augment_from_dispatcher(dir, known_events, known_commands, &mut by_name);
    // Drop plumbing-only modules. An `Integrations/<Name>.hs` that handles
    // NO event AND emits NO command is a pure helper (HTTP client, JSON
    // codec, request builder — e.g. CIOS Payment's `BankHttp`/`EvocaBank`),
    // not an event-model integration. Emitting a node for it shows up as a
    // dead orphan in the healer's graph. We keep an integration the moment
    // EITHER list is non-empty: an outbound integration that HANDLES an
    // event but emits no command (e.g. a Brevo email call triggered by an
    // event) has non-empty `handles_events` and MUST be kept.
    by_name.retain(|_, info| !info.handles_events.is_empty() || !info.emits_commands.is_empty());
    by_name.into_values().collect()
}

/// Read `<dir>/Integrations.hs` if present; merge its event-to-integration
/// mapping into `by_name`. Integrations that ONLY appear in the dispatcher
/// (no `Integrations/<Name>.hs` file) get an entry synthesised from the
/// dispatcher import — kind defaults to `Outbound`, emits derived from
/// the file body if it exists.
fn augment_from_dispatcher(
    dir: &Path,
    known_events: &[String],
    known_commands: &[String],
    by_name: &mut std::collections::BTreeMap<String, IntegrationInfo>,
) {
    let dispatcher_path = dir.join("Integrations.hs");
    let Ok(body) = std::fs::read_to_string(&dispatcher_path) else {
        return;
    };
    let import_map = extract_integration_import_map(&body);
    if import_map.is_empty() {
        return;
    }
    let arm_map = extract_dispatcher_arms(&body, known_events, &import_map);
    for (intg_name, events) in arm_map {
        let entry = by_name.entry(intg_name.clone()).or_insert_with(|| {
            let file = dir.join(format!("Integrations/{intg_name}.hs"));
            let emits = if file.is_file() {
                std::fs::read_to_string(&file)
                    .map(|b| extract_emitted_commands(&b, known_commands))
                    .unwrap_or_default()
            } else {
                Vec::new()
            };
            let kind = if emits.is_empty() {
                IntegrationKind::Outbound
            } else {
                IntegrationKind::Reactive
            };
            IntegrationInfo {
                name: intg_name.clone(),
                file,
                kind,
                handles_events: Vec::new(),
                emits_commands: emits,
            }
        });
        for e in events {
            if !entry.handles_events.contains(&e) {
                entry.handles_events.push(e);
            }
        }
    }
}

/// Parse `import …Integrations.<IntgName> (fn1, fn2)` lines from the
/// dispatcher module, returning `fnName → IntgName`. Qualified imports
/// (`qualified as Alias`) are skipped — the dispatcher arms reference
/// the bare function name in CIOS convention, not an alias.
fn extract_integration_import_map(body: &str) -> std::collections::BTreeMap<String, String> {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        Regex::new(
            // `import <…>.Integrations.<IntgName> ( fn1, fn2, ... )`
            r"(?m)^\s*import\s+(?:[A-Z]\w*\.)*Integrations\.([A-Z]\w*)\s*\(([^)]*)\)",
        )
        .unwrap()
    });
    let mut out = std::collections::BTreeMap::new();
    for cap in re.captures_iter(body) {
        let Some(intg) = cap.get(1) else { continue };
        let intg_name = intg.as_str().to_string();
        let imports = cap.get(2).map(|m| m.as_str()).unwrap_or("");
        for raw in imports.split(',') {
            let trimmed = raw.trim();
            // Function names start with a lowercase letter; data types
            // and constructors (uppercase) live elsewhere.
            let fn_name: String = trimmed
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if let Some(first) = fn_name.chars().next() {
                if first.is_ascii_lowercase() {
                    out.insert(fn_name, intg_name.clone());
                }
            }
        }
    }
    out
}

/// Walk the dispatcher body looking for `case <var> of …` arms whose
/// head is a known event constructor. For each such arm, scan its RHS
/// for handler-function-name tokens that appear in `import_map`. Records
/// `IntgName → [EventCtor]` for every match. Arms whose RHS contains
/// only `Integration.none` (or no handler-fn match at all) contribute
/// nothing — that's how Payment-style "don't react to this event"
/// declarations are correctly excluded.
fn extract_dispatcher_arms(
    body: &str,
    known_events: &[String],
    import_map: &std::collections::BTreeMap<String, String>,
) -> std::collections::BTreeMap<String, Vec<String>> {
    let candidate_set: BTreeSet<&str> = known_events.iter().map(String::as_str).collect();
    let mut result: std::collections::BTreeMap<String, Vec<String>> =
        std::collections::BTreeMap::new();
    for (ctor, arm) in split_case_arms(body, &candidate_set) {
        for (fn_name, intg_name) in import_map {
            if contains_word(&arm, fn_name) {
                let bucket = result.entry(intg_name.clone()).or_default();
                if !bucket.contains(&ctor) {
                    bucket.push(ctor.clone());
                }
            }
        }
    }
    result
}

fn list_dot_hs(dir: &Path) -> Vec<PathBuf> {
    if !dir.is_dir() {
        return Vec::new();
    }
    let mut out = Vec::new();
    walk_hs(dir, &mut out);
    out.sort();
    out
}

fn walk_hs(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(rd) = std::fs::read_dir(dir) else { return };
    for entry in rd.flatten() {
        let p = entry.path();
        if p.is_dir() {
            walk_hs(&p, out);
        } else if p.extension().and_then(|s| s.to_str()) == Some("hs") {
            out.push(p);
        }
    }
}

fn parse_command_file(path: &Path, known_events: &[String]) -> Option<CommandInfo> {
    let body = std::fs::read_to_string(path).ok()?;
    let name = path.file_stem()?.to_str()?.to_string();
    let decide_body = extract_function_body(&body, "decide").unwrap_or_default();
    let produces = filter_present(&decide_body, known_events);
    let via_web_transport = body.contains("WebTransport");
    let fields = data_type_record_fields(&body, &name);
    Some(CommandInfo {
        name,
        file: path.to_path_buf(),
        produces,
        via_web_transport,
        fields,
    })
}

fn parse_query_file(path: &Path, known_events: &[String]) -> Option<QueryInfo> {
    let body = std::fs::read_to_string(path).ok()?;
    let name = path.file_stem()?.to_str()?.to_string();
    let subscribes_to = filter_present(&body, known_events);
    // A NeoHaskell read model is a `QueryOf <Entity> <ReadModel>` whose
    // `combine` reads ENTITY fields (`entity.<field>`) — never event
    // constructors. We extract the real data dependency from the combine so
    // the wiring layer can compute precise event→query edges instead of the
    // all-local over-approximation. See `event_write_sets_in_domain` for the
    // matching writes side.
    let combine = extract_method_body(&body, "combine").unwrap_or_default();
    let reads_entity_fields = extract_entity_field_reads(&combine);
    let (case_field, noop_values) = extract_combine_noop_values(&combine);
    // Read-model fields: `data <Query> = <Query> { … }` in the query file (the
    // type name follows the `deriveQuery ''<Query>` convention == file stem),
    // falling back to the first data record in the file.
    let fields = {
        let named = data_type_record_fields(&body, &name);
        if named.is_empty() {
            first_data_record_fields(&body)
        } else {
            named
        }
    };
    Some(QueryInfo {
        name,
        file: path.to_path_buf(),
        subscribes_to,
        fields,
        reads_entity_fields,
        case_field,
        noop_values,
    })
}

/// Every `entity.<field>` accessor read inside a `combine` body, in source
/// order, deduped. This is the read model's true data dependency on the
/// aggregate's projection. Read-model fields that happen to share a name are
/// not captured — only the `entity.`-qualified accessors.
fn extract_entity_field_reads(combine: &str) -> Vec<String> {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r"\bentity\.([a-z]\w*)").unwrap());
    let mut out = Vec::new();
    let mut seen = BTreeSet::new();
    for cap in re.captures_iter(combine) {
        if let Some(m) = cap.get(1) {
            let f = m.as_str().to_string();
            if seen.insert(f.clone()) {
                out.push(f);
            }
        }
    }
    out
}

/// For a read model whose `combine` is a single flat
/// `case entity.<field> of <Ctor> -> Update|NoOp|Delete`, return the
/// scrutinee field and the set of pattern values (last `.`-segment) whose
/// branch is a definitive `NoOp`. We capture the NoOp set (not the Update
/// set) on purpose: a parse MISS then leaves an event CONNECTED (over-
/// approximation), never silently drops a real edge — the information-
/// completeness guardrail. Returns `(None, None)` when the combine has no
/// `case entity.<field> of`, and `(Some(field), None)` when the combine has
/// more than one `case` (nested / not flat) so the caller falls back rather
/// than trusting an incomplete NoOp set.
fn extract_combine_noop_values(combine: &str) -> (Option<String>, Option<BTreeSet<String>>) {
    static CASE_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let case_re =
        CASE_RE.get_or_init(|| Regex::new(r"case\s+entity\.([a-z]\w*)\s+of").unwrap());
    let mut cases = case_re.captures_iter(combine);
    let Some(first) = cases.next() else {
        return (None, None);
    };
    let field = first.get(1).unwrap().as_str().to_string();
    // More than one `case entity.<field> of` ⇒ nested / not a flat enum
    // dispatch ⇒ we cannot safely enumerate NoOp values. Surface the field
    // (so the caller knows the query cases on it) but no NoOp set ⇒ fallback.
    if cases.next().is_some() {
        return (Some(field), None);
    }
    // Branches: `  <Pattern> ->` then `Update|NoOp|Delete` on the same or the
    // next non-empty line. Collect the pattern value (last segment) of every
    // branch whose result is `NoOp`.
    static BRANCH_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let branch_re = BRANCH_RE.get_or_init(|| {
        Regex::new(r"(?m)^[ \t]+([A-Za-z_][\w.]*)[^\n]*->[ \t]*(?:\n[ \t]*)?(Update|NoOp|Delete)\b")
            .unwrap()
    });
    let mut noop = BTreeSet::new();
    for cap in branch_re.captures_iter(combine) {
        let pat = cap.get(1).map(|m| m.as_str()).unwrap_or("");
        let result = cap.get(2).map(|m| m.as_str()).unwrap_or("");
        if result == "NoOp" {
            if let Some(seg) = pat.rsplit('.').next() {
                noop.insert(seg.to_string());
            }
        }
    }
    (Some(field), Some(noop))
}

/// Per-event written-field sets parsed from an aggregate's
/// `update`/`evolve` fold, plus the bare-enum value each event sets a field
/// to (for value-level hub narrowing). `fold_found = false` signals the
/// caller to fall back to the all-local default (no positive evidence).
#[derive(Debug, Clone, Default)]
pub struct EntityWriteAnalysis {
    /// event constructor → entity fields it GENUINELY writes (rhs ≠ `entity.<field>`).
    pub writes: BTreeMap<String, BTreeSet<String>>,
    /// event constructor → (field → bare-enum value it is set to, last `.`-segment).
    pub enum_values: BTreeMap<String, BTreeMap<String, String>>,
    /// `true` once an `update`/`evolve` fold was located and split into arms.
    pub fold_found: bool,
}

/// Parse `<dir>/Entity.hs` (or `<dir>/Core.hs`) for the aggregate's
/// `update :: Event -> Entity -> Entity` (or `evolve`) fold, returning which
/// entity fields each event constructor writes. A field is GENUINELY written
/// only when its record-update RHS is not the identity copy `entity.<field>`
/// — full-record-copy aggregates (e.g. `ProposalEntity`) restate every field
/// every arm, so the copy-through filter is what makes the writes set precise.
pub fn event_write_sets_in_domain(dir: &Path, known_events: &[String]) -> EntityWriteAnalysis {
    let mut src = String::new();
    for fname in ["Entity.hs", "Core.hs"] {
        let path = dir.join(fname);
        let Ok(body) = std::fs::read_to_string(&path) else {
            continue;
        };
        // Prefer the file that actually defines the fold; fall back to the
        // first readable candidate (older projects inline it in Core.hs).
        if body.contains("update ") || body.contains("update::") || body.contains("evolve ") {
            src = body;
            break;
        }
        if src.is_empty() {
            src = body;
        }
    }
    let fold = extract_function_body(&src, "update")
        .or_else(|| extract_function_body(&src, "evolve"));
    let Some(fold) = fold else {
        return EntityWriteAnalysis::default();
    };
    // Isolate the `case … of` arms (the body after the first `of`). If the
    // fold has no case-of (single-event aggregate), scan the whole body.
    let case_body = match fold.find(" of\n") {
        Some(idx) => &fold[idx + 4..],
        None => fold.as_str(),
    };
    let candidate_set: BTreeSet<&str> = known_events.iter().map(String::as_str).collect();
    let mut writes: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut enum_values: BTreeMap<String, BTreeMap<String, String>> = BTreeMap::new();
    for (ctor, arm) in split_case_arms(case_body, &candidate_set) {
        let (fields, values) = extract_record_writes(&arm);
        if !fields.is_empty() {
            writes.entry(ctor.clone()).or_default().extend(fields);
        }
        if !values.is_empty() {
            enum_values.entry(ctor).or_default().extend(values);
        }
    }
    EntityWriteAnalysis {
        writes,
        enum_values,
        fold_found: true,
    }
}

/// From one `case` arm of an aggregate fold, the entity fields it writes and
/// the bare-enum value (last `.`-segment) of any field set to a single
/// constructor token. A `field = rhs` pair is a write iff `rhs` is not the
/// identity copy `entity.<field>`. Inner records (e.g. a `MetricScore {…}`
/// built in a `let`) over-capture harmlessly — over-approximation is the
/// safe direction; the wiring layer only ever DROPS an edge with positive
/// evidence on both sides.
fn extract_record_writes(arm: &str) -> (BTreeSet<String>, BTreeMap<String, String>) {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    // `field = rhs` in record syntax: the field ident is preceded by `{` or
    // `,` (possibly across newlines); rhs runs to the next newline/`,`/`}`.
    let re = RE.get_or_init(|| Regex::new(r"[{,]\s*([a-z]\w*)\s*=\s*([^\n,}]+)").unwrap());
    let mut fields = BTreeSet::new();
    let mut values = BTreeMap::new();
    for cap in re.captures_iter(arm) {
        let field = cap.get(1).unwrap().as_str();
        let rhs = cap.get(2).unwrap().as_str().trim();
        if rhs == format!("entity.{field}") {
            continue; // copy-through, not a genuine write
        }
        fields.insert(field.to_string());
        if let Some(value) = single_ctor_token(rhs) {
            values.insert(field.to_string(), value);
        }
    }
    (fields, values)
}

/// If `rhs` is a single constructor token (optionally module-qualified, e.g.
/// `Lifecycle.Approved`), return its last `.`-segment (`Approved`). Returns
/// `None` for applications (`Just e.x`), literals with whitespace, etc.
fn single_ctor_token(rhs: &str) -> Option<String> {
    let t = rhs.trim();
    if t.is_empty() || t.contains(char::is_whitespace) {
        return None;
    }
    let last = t.rsplit('.').next().unwrap_or(t);
    let mut chars = last.chars();
    let first = chars.next()?;
    if !first.is_ascii_uppercase() {
        return None;
    }
    if last.chars().all(|c| c.is_alphanumeric() || c == '_') {
        Some(last.to_string())
    } else {
        None
    }
}

fn parse_integration_file(
    path: &Path,
    known_events: &[String],
    known_commands: &[String],
) -> Option<IntegrationInfo> {
    let body = std::fs::read_to_string(path).ok()?;
    let name = path.file_stem()?.to_str()?.to_string();
    // STRICT per-file scan: look for a `handleEvent` function and walk
    // its `case event of` block, counting only arms whose RHS does any
    // `Integration.<verb>` other than `Integration.none` or any
    // `Command.Emit`. False positives from imports of event-types-for-
    // context are correctly excluded.
    //
    // For Pattern-A integrations (CIOS Proposal / ProposalMetricEvaluation)
    // whose handler function isn't named `handleEvent`, this scan returns
    // empty — that's fine; the dispatcher pass in `augment_from_dispatcher`
    // fills in the events afterwards.
    let handle_body = extract_function_body(&body, "handleEvent").unwrap_or_default();
    let handles_events = active_handles_in_case_body(&handle_body, known_events);
    // Emission can happen either DIRECTLY inside the handler body
    // (testbed-style: `Command.Emit { command = X { … } }`) or via a
    // sibling `ToAction` instance elsewhere in the same file
    // (CIOS-style: `Integration.emitCommand X { … }`). Scan the whole
    // file so the kind classifier finds both.
    let emits_commands = extract_emitted_commands(&body, known_commands);
    let kind = if emits_commands.is_empty() {
        IntegrationKind::Outbound
    } else {
        IntegrationKind::Reactive
    };
    Some(IntegrationInfo {
        name,
        file: path.to_path_buf(),
        kind,
        handles_events,
        emits_commands,
    })
}

/// Split a `case <scrutinee> of …` body into `(head_ctor, arm_text)` pairs,
/// one per arm whose head token is in `candidates`, in source order. Arms
/// are delimited the way Haskell layout does: an arm runs from its head
/// line to (exclusive) the next arm-start line. An arm-start is a line
/// whose first non-space token is a candidate constructor OR a `_`
/// wildcard (wildcards bound an arm but are not themselves emitted). This
/// is the shared primitive behind `active_handles_in_case_body`,
/// `extract_dispatcher_arms`, and the entity write-set parser.
fn split_case_arms(body: &str, candidates: &BTreeSet<&str>) -> Vec<(String, String)> {
    let is_arm_start = |line: &str| -> bool {
        let trimmed = line.trim_start();
        let first = match trimmed.chars().next() {
            Some(c) => c,
            None => return false,
        };
        if first == '_' {
            let next = trimmed.as_bytes().get(1).copied();
            return matches!(next, None | Some(b' ') | Some(b'\t'));
        }
        if !first.is_ascii_uppercase() {
            return false;
        }
        let end = trimmed
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(trimmed.len());
        candidates.contains(&trimmed[..end])
    };

    let lines: Vec<&str> = body.lines().collect();
    let mut out: Vec<(String, String)> = Vec::new();
    for i in 0..lines.len() {
        let trimmed = lines[i].trim_start();
        let first_word_end = trimmed
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(trimmed.len());
        let first_word = &trimmed[..first_word_end];
        if !candidates.contains(first_word) {
            continue;
        }
        let mut arm = String::from(lines[i]);
        for j in (i + 1)..lines.len() {
            if is_arm_start(lines[j]) {
                break;
            }
            arm.push('\n');
            arm.push_str(lines[j]);
        }
        out.push((first_word.to_string(), arm));
    }
    out
}

/// Walk a `case <evt> of …` body and return only the event constructors
/// whose arm body actually does something — i.e. whose RHS has any
/// `Integration.<word>` other than `Integration.none`, or any
/// `Command.Emit`. Wildcard arms (`_ -> …`) and arms whose only RHS is
/// `Integration.none` MUST NOT count as "handled".
fn active_handles_in_case_body(body: &str, candidates: &[String]) -> Vec<String> {
    let candidate_set: BTreeSet<&str> = candidates.iter().map(String::as_str).collect();
    let mut active = BTreeSet::new();
    for (ctor, arm) in split_case_arms(body, &candidate_set) {
        if arm_is_active(&arm) {
            active.insert(ctor);
        }
    }
    candidates
        .iter()
        .filter(|c| active.contains(c.as_str()))
        .cloned()
        .collect()
}

/// True iff the arm body has at least one non-`none` Integration verb or
/// any `Command.Emit`.
fn arm_is_active(arm: &str) -> bool {
    if arm.contains("Command.Emit") {
        return true;
    }
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r"\bIntegration\.([A-Za-z_]\w*)\b").unwrap());
    for cap in re.captures_iter(arm) {
        if cap.get(1).map(|m| m.as_str()) != Some("none") {
            return true;
        }
    }
    false
}

/// Extract constructor names from the event sum:
///
///     data CartEvent
///       = CartCreated { ... }
///       | ItemAdded { ... }
///       | ...
///
/// Limited to the FIRST `data X ... =` block that mentions `Event` in
/// the type name — that's the convention in NeoHaskell domains.
fn extract_event_constructors(src: &str) -> Vec<String> {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        // `(?m)` so `^` matches at line starts; `\z` (not multiline `$`)
        // so the non-greedy body keeps going past blank lines until
        // either the next top-level non-indented line or the end of input.
        Regex::new(r"(?m)^data\s+([A-Z]\w*Event)\b[^=]*=([\s\S]*?)(?:\n\S|\z)").unwrap()
    });
    let Some(cap) = re.captures(src) else {
        return Vec::new();
    };
    let block = cap.get(2).map(|m| m.as_str()).unwrap_or("");
    // Constructor names are tokens starting with uppercase letters,
    // appearing right after `=` or `|`.
    let mut out = Vec::new();
    let mut seen = BTreeSet::new();
    for raw in block.split('|') {
        let cleaned = raw.trim_start();
        let cleaned = cleaned.trim_start_matches(|c: char| c == '|' || c.is_whitespace());
        let ident: String = cleaned
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        if let Some(first) = ident.chars().next() {
            if first.is_ascii_uppercase() && seen.insert(ident.clone()) {
                out.push(ident);
            }
        }
    }
    out
}

/// Extract the payload-module names referenced by an event sum type's
/// arms. For
///
///     data ProposalEvent
///       = ProposalPdfUploaded    PdfUploaded.Event
///       | ProposalPdfTranscribed PdfTranscribed.Event
///       | EvaluationTriggered    EvaluationTriggered.Event
///
/// this returns `{PdfUploaded, PdfTranscribed, EvaluationTriggered}` — the
/// module qualifier in front of the `.Event` payload type on each arm. These
/// are the file stems under `Events/` that must NOT be minted as their own
/// events (they are payloads of the constructors on the left). Arms written
/// with an inline record (`CartCreated { entityId :: Uuid }`) contribute
/// nothing — there is no separate payload module to suppress.
fn extract_event_payload_modules(src: &str) -> BTreeSet<String> {
    static BLOCK_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    static PAYLOAD_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let block_re = BLOCK_RE.get_or_init(|| {
        Regex::new(r"(?m)^data\s+([A-Z]\w*Event)\b[^=]*=([\s\S]*?)(?:\n\S|\z)").unwrap()
    });
    // A payload reference: an upper-case module identifier immediately
    // followed by `.Event` (the NeoHaskell payload-type convention).
    let payload_re = PAYLOAD_RE.get_or_init(|| Regex::new(r"\b([A-Z]\w*)\.Event\b").unwrap());
    let mut out = BTreeSet::new();
    let Some(cap) = block_re.captures(src) else {
        return out;
    };
    let block = cap.get(2).map(|m| m.as_str()).unwrap_or("");
    for pc in payload_re.captures_iter(block) {
        if let Some(m) = pc.get(1) {
            out.insert(m.as_str().to_string());
        }
    }
    out
}

/// Slurp the body of a top-level Haskell function definition. Finds the
/// line that starts with `<name>` at column 0 (the function clause) and
/// returns everything from there until the next column-0 declaration.
fn extract_function_body(src: &str, name: &str) -> Option<String> {
    let mut found_at: Option<usize> = None;
    let mut end_at = src.len();
    let mut byte_cursor = 0usize;
    let mut iter = src.lines();

    while let Some(line) = iter.next() {
        let line_start = byte_cursor;
        byte_cursor += line.len() + 1; // +1 for the '\n' (works for typical \n; if \r\n, harmless)

        if found_at.is_none() {
            // Looking for the function clause: a line starting with `<name>` at column 0
            // followed by a space, `(`, or `=`.
            if line.starts_with(name) {
                let rest = &line[name.len()..];
                if rest
                    .chars()
                    .next()
                    .map(|c| c.is_whitespace() || c == '(' || c == ':' || c == '=')
                    .unwrap_or(false)
                {
                    found_at = Some(line_start);
                }
            }
        } else if !line.is_empty() && !line.starts_with(char::is_whitespace) {
            // Comments and pragmas don't end the body.
            if line.starts_with("--") || line.starts_with("{-") {
                continue;
            }
            // Continuation clauses of the SAME function — pattern-match
            // arms like `decide ... = ...` and `decide _ _ = ...` — also
            // don't end the body. Identify by the leading word.
            let first_word_end = line
                .find(|c: char| !c.is_alphanumeric() && c != '_')
                .unwrap_or(line.len());
            let first_word = &line[..first_word_end];
            if first_word == name {
                continue;
            }
            end_at = line_start;
            break;
        }
    }

    found_at.map(|start| src[start..end_at].to_string())
}

/// Slurp the body of a Haskell binding that may be indented — e.g. a
/// `combine` / `queryId` method inside an `instance QueryOf … where` block.
/// Finds the first line whose first non-space token is `<name>` (followed by
/// a space, `(`, `:`, or `=`) at indent `N`, and returns it plus every
/// following line until the next non-empty line at indent ≤ `N` that is not a
/// continuation clause of the same binding. Generalises `extract_function_body`
/// (which only sees column-0 definitions) to instance methods.
fn extract_method_body(src: &str, name: &str) -> Option<String> {
    let lines: Vec<&str> = src.lines().collect();
    let mut start = None;
    let mut header_indent = 0usize;
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed.strip_prefix(name) {
            if rest
                .chars()
                .next()
                .map(|c| c.is_whitespace() || c == '(' || c == ':' || c == '=')
                .unwrap_or(false)
            {
                start = Some(i);
                header_indent = line.len() - trimmed.len();
                break;
            }
        }
    }
    let start = start?;
    let mut end = lines.len();
    for (j, line) in lines.iter().enumerate().skip(start + 1) {
        if line.trim().is_empty() {
            continue;
        }
        let indent = line.len() - line.trim_start().len();
        if indent <= header_indent {
            // A continuation clause of the SAME binding (multi-clause method)
            // does not end the body; the next sibling binding / `where` exit does.
            let tw = line.trim_start();
            let fw_end = tw
                .find(|c: char| !c.is_alphanumeric() && c != '_')
                .unwrap_or(tw.len());
            if &tw[..fw_end] == name {
                continue;
            }
            end = j;
            break;
        }
    }
    Some(lines[start..end].join("\n"))
}

/// Return the elements of `candidates` (in their original order) that
/// appear as a whole-word token inside `haystack`.
fn filter_present(haystack: &str, candidates: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = BTreeSet::new();
    for c in candidates {
        if contains_word(haystack, c) && seen.insert(c.clone()) {
            out.push(c.clone());
        }
    }
    out
}

/// Whole-word match. We can't rely on regex word boundaries because
/// constructor names like `OrderPlaced_v2` should NOT match `OrderPlaced`
/// — the trailing char must not be alphanumeric/underscore.
fn contains_word(haystack: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return false;
    }
    let mut start = 0;
    while let Some(idx) = haystack[start..].find(needle) {
        let abs = start + idx;
        let before_ok = abs == 0
            || !haystack
                .as_bytes()
                .get(abs - 1)
                .map(|b| (*b as char).is_alphanumeric() || *b == b'_')
                .unwrap_or(false);
        let end = abs + needle.len();
        let after_ok = end >= haystack.len()
            || !haystack
                .as_bytes()
                .get(end)
                .map(|b| (*b as char).is_alphanumeric() || *b == b'_')
                .unwrap_or(false);
        if before_ok && after_ok {
            return true;
        }
        start = abs + 1;
    }
    false
}

/// Find every command this integration emits, in source order. Real
/// NeoHaskell integrations emit commands several ways; relying on the
/// `emitCommand`/`Command.Emit` keyword alone misses most of them (e.g. the
/// callback idiom `onSuccess :: … -> CompleteMetricEvaluation` inside
/// `Integration.batch`, or `Integration.emitCommand inner` where `inner` is a
/// pre-built command value). The robust, idiom-independent signal is that the
/// command VALUE is constructed as a record literal `CommandName { … }`
/// somewhere in the file — verified across every CIOS integration. So we
/// detect:
///   * explicit keyword idioms: `Command.Emit { command = X }`,
///     `Integration.emitCommand X` (constructor adjacent); plus
///   * any KNOWN command name constructed as a record `X { … }` (the `\s*`
///     spans the constructor-on-its-own-line layout). `known_commands` (the
///     GLOBAL command set) gates this so we don't mint config/response records
///     like `OpenRouter.Request { … }` as commands.
fn extract_emitted_commands(src: &str, known_commands: &[String]) -> Vec<String> {
    static KEYWORD_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    static RECORD_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let keyword_re = KEYWORD_RE.get_or_init(|| {
        Regex::new(
            r"(?:Command\.Emit\s*\{\s*command\s*=|Integration\.emitCommand)\s*([A-Z]\w*)",
        )
        .unwrap()
    });
    // `Name {` (constructor optionally on its own line before the brace).
    let record_re = RECORD_RE.get_or_init(|| Regex::new(r"\b([A-Z]\w*)\s*\{").unwrap());
    let known: BTreeSet<&str> = known_commands.iter().map(String::as_str).collect();

    let mut out = Vec::new();
    let mut seen = BTreeSet::new();
    // Pass A — explicit emit keywords (catches commands even if their record
    // is built in another module, e.g. `emitCommand SomeCmd { … }` inline).
    for cap in keyword_re.captures_iter(src) {
        if let Some(m) = cap.get(1) {
            let s = m.as_str().to_string();
            if seen.insert(s.clone()) {
                out.push(s);
            }
        }
    }
    // Pass B — known commands constructed as record literals anywhere in the
    // file (covers the callback-return-type and `emitCommand <var>` idioms).
    for cap in record_re.captures_iter(src) {
        let name = cap.get(1).map(|m| m.as_str()).unwrap_or("");
        if known.contains(name) && seen.insert(name.to_string()) {
            out.push(name.to_string());
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Record-field extraction (event/command payloads).
//
// Best-effort and intentionally "dumb" — robustness is future work. The
// guarantees we DO hold: never panic, and on any uncertainty return EMPTY
// fields rather than wrong ones (an incomplete read is safe; a wrong read is
// not). Source-declaration order is preserved so output is deterministic.
//
// Known limitations (documented, tested where it matters):
//   * Only INLINE records `{ f :: T, … }` are parsed. Payload-module arms
//     (`Foo Bar.Event`) yield empty fields (the payload module isn't read).
//   * Shared-type fields (`a, b :: T`) capture only `b` (the comma splits them).
//   * `deriving (…)` and `-- line comments` are excluded.
//   * Type strings are kept verbatim (`Maybe Text`, `(Int, Text)`), no resolution.
// ---------------------------------------------------------------------------

/// Map each event-sum constructor to the record fields of its inline payload.
/// Mirrors `extract_event_constructors` (same block regex + `|` split) so the
/// constructor keys line up exactly with the events we materialise.
fn extract_event_constructor_fields(src: &str) -> BTreeMap<String, Vec<RecordField>> {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        Regex::new(r"(?m)^data\s+([A-Z]\w*Event)\b[^=]*=([\s\S]*?)(?:\n\S|\z)").unwrap()
    });
    let mut out: BTreeMap<String, Vec<RecordField>> = BTreeMap::new();
    let Some(cap) = re.captures(src) else {
        return out;
    };
    let block = cap.get(2).map(|m| m.as_str()).unwrap_or("");
    for raw in block.split('|') {
        let cleaned = raw.trim_start();
        let cleaned = cleaned.trim_start_matches(|c: char| c == '|' || c.is_whitespace());
        let ident: String = cleaned
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        let Some(first) = ident.chars().next() else {
            continue;
        };
        if !first.is_ascii_uppercase() {
            continue;
        }
        // The arm text after the constructor name; its first `{…}` (if any) is
        // the inline record payload.
        let fields = brace_body(cleaned)
            .map(|b| parse_record_fields(&b))
            .unwrap_or_default();
        out.entry(ident).or_insert(fields);
    }
    out
}

/// Map each event-sum constructor to the payload MODULE referenced by its arm
/// (`CounterCreated CounterCreated.Event` → `CounterCreated → CounterCreated`,
/// `ProposalPdfUploaded PdfUploaded.Event` → `ProposalPdfUploaded → PdfUploaded`).
/// Inline-record arms contribute nothing. Lets the field extractor read
/// `Events/<Module>.hs` for the constructor's real fields.
fn extract_event_constructor_payload_modules(src: &str) -> BTreeMap<String, String> {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    static PAYLOAD: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        Regex::new(r"(?m)^data\s+([A-Z]\w*Event)\b[^=]*=([\s\S]*?)(?:\n\S|\z)").unwrap()
    });
    let payload_re = PAYLOAD.get_or_init(|| Regex::new(r"\b([A-Z]\w*)\.Event\b").unwrap());
    let mut out: BTreeMap<String, String> = BTreeMap::new();
    let Some(cap) = re.captures(src) else {
        return out;
    };
    let block = cap.get(2).map(|m| m.as_str()).unwrap_or("");
    for raw in block.split('|') {
        let cleaned = raw.trim_start();
        let cleaned = cleaned.trim_start_matches(|c: char| c == '|' || c.is_whitespace());
        let ident: String = cleaned
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        let Some(first) = ident.chars().next() else {
            continue;
        };
        if !first.is_ascii_uppercase() {
            continue;
        }
        // The arm text AFTER the constructor name — look for its `X.Event` payload.
        let rest = &cleaned[ident.len()..];
        if let Some(pc) = payload_re.captures(rest) {
            if let Some(m) = pc.get(1) {
                out.entry(ident).or_insert_with(|| m.as_str().to_string());
            }
        }
    }
    out
}

/// Record fields of the `data <type_name> = <type_name> { … }` declaration in
/// `src`. The `=` vs `::` distinction inside `parse_field_decl` means a record
/// *update* (`X { f = v }`) in a `decide`/`handleEvent` body contributes no
/// fields even if `brace_body` reaches it — so a fieldless command stays empty.
fn data_type_record_fields(src: &str, type_name: &str) -> Vec<RecordField> {
    let needle = format!("data {type_name}");
    let mut search = 0;
    while let Some(rel) = src[search..].find(&needle) {
        let abs = search + rel;
        let after = abs + needle.len();
        let boundary_ok = src[after..]
            .chars()
            .next()
            .map(|c| !(c.is_alphanumeric() || c == '_'))
            .unwrap_or(true);
        if boundary_ok {
            return brace_body(&src[abs..])
                .map(|b| parse_record_fields(&b))
                .unwrap_or_default();
        }
        search = after;
    }
    Vec::new()
}

/// Record fields of the FIRST `data <Type>` declaration in `src` (one-event-
/// per-file layout, where the type is usually `Event`). Anchored at a line
/// start so `Metadata`-style substrings don't false-match.
fn first_data_record_fields(src: &str) -> Vec<RecordField> {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r"(?m)^\s*data\s+[A-Z]\w*\b").unwrap());
    let Some(m) = re.find(src) else {
        return Vec::new();
    };
    brace_body(&src[m.start()..])
        .map(|b| parse_record_fields(&b))
        .unwrap_or_default()
}

/// Inner text of the FIRST brace-balanced `{ … }` in `src` (excluding the outer
/// braces). `None` if there's no `{`. Brace depth is tracked so nested records
/// don't terminate early. Operates on byte offsets at ASCII `{`/`}` boundaries,
/// which is safe regardless of multibyte content in between.
fn brace_body(src: &str) -> Option<String> {
    let start = src.find('{')?;
    let bytes = src.as_bytes();
    let mut depth = 0i32;
    let mut i = start;
    while i < bytes.len() {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(src[start + 1..i].to_string());
                }
            }
            _ => {}
        }
        i += 1;
    }
    None
}

/// Split a record body on TOP-LEVEL commas (depth 0), so commas inside tuple /
/// list / nested-record types (`(Int, Text)`, `[a]`) don't split a field.
fn split_top_level_commas(inner: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut depth = 0i32;
    let mut cur = String::new();
    for c in inner.chars() {
        match c {
            '(' | '[' | '{' => {
                depth += 1;
                cur.push(c);
            }
            ')' | ']' | '}' => {
                depth -= 1;
                cur.push(c);
            }
            ',' if depth == 0 => out.push(std::mem::take(&mut cur)),
            _ => cur.push(c),
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur);
    }
    out
}

/// Parse every `name :: Type` field from a record body, in source order.
fn parse_record_fields(inner: &str) -> Vec<RecordField> {
    split_top_level_commas(inner)
        .iter()
        .filter_map(|p| parse_field_decl(p))
        .collect()
}

/// Parse one `name :: Type` field declaration. Returns `None` for anything that
/// isn't a single lowercase-led field name bound by `::` (e.g. a record-update
/// `f = v`, a blank piece, or a multi-name `a, b` remnant).
fn parse_field_decl(piece: &str) -> Option<RecordField> {
    // Drop a trailing line comment.
    let piece = piece.split("--").next().unwrap_or(piece);
    let (name, ty) = piece.split_once("::")?;
    let name = name.trim();
    let type_name = collapse_ws(ty.trim());
    if name.is_empty() || type_name.is_empty() {
        return None;
    }
    let first = name.chars().next()?;
    if !(first.is_ascii_lowercase() || first == '_') {
        return None;
    }
    if !name.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '\'') {
        return None;
    }
    Some(RecordField {
        name: name.to_string(),
        type_name,
    })
}

/// Collapse internal whitespace runs (incl. newlines) to single spaces so a
/// type spread across lines reads as one token.
fn collapse_ws(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_event_constructors_from_data_block() {
        let src = r#"
module Foo where
data CartEvent
  = CartCreated { entityId :: Uuid }
  | ItemAdded { stockId :: Uuid }
  | ItemRemoved { stockId :: Uuid }
  deriving (Generic)
"#;
        assert_eq!(
            extract_event_constructors(src),
            vec!["CartCreated", "ItemAdded", "ItemRemoved"]
        );
    }

    #[test]
    fn extract_event_constructors_returns_empty_when_no_event_sum() {
        let src = "module Foo where\nbar = 1\n";
        assert!(extract_event_constructors(src).is_empty());
    }

    #[test]
    fn extracts_decide_body_until_next_top_level() {
        let src = r#"
module Foo where
decide :: Cmd -> Decision Event
decide cmd _ = case x of
  Just _ -> Decider.acceptExisting [Foo {}]
  _ -> Decider.reject "no"
type instance EntityOf Cmd = Bar
"#;
        let body = extract_function_body(src, "decide").expect("body");
        assert!(body.contains("Decider.acceptExisting"));
        assert!(body.contains("Foo {}"));
        assert!(!body.contains("type instance"));
    }

    #[test]
    fn extract_function_body_returns_none_when_function_missing() {
        let src = "module X where\nfoo = 1\n";
        assert!(extract_function_body(src, "decide").is_none());
    }

    #[test]
    fn contains_word_respects_token_boundaries() {
        assert!(contains_word("emit ItemAdded {}", "ItemAdded"));
        assert!(!contains_word("ItemAddedFoo", "ItemAdded"));
        assert!(!contains_word("FooItemAdded", "ItemAdded"));
        assert!(contains_word("[ItemAdded]", "ItemAdded"));
        assert!(contains_word("ItemAdded", "ItemAdded"));
    }

    #[test]
    fn filter_present_returns_only_real_matches_in_order() {
        let candidates = vec![
            "CartCreated".to_string(),
            "ItemAdded".to_string(),
            "ItemRemoved".to_string(),
        ];
        let body = "do Decider.acceptExisting [ItemAdded {entityId = i}]";
        assert_eq!(filter_present(body, &candidates), vec!["ItemAdded".to_string()]);
    }

    #[test]
    fn extract_emitted_commands_finds_command_dot_emit() {
        let src = r#"
do
  Integration.outbound
    Command.Emit
      { command = ReserveStock { quantity = q }
      }
  Integration.outbound
    Command.Emit { command = NotifyCustomer { id = c } }
"#;
        assert_eq!(
            extract_emitted_commands(src, &[]),
            vec!["ReserveStock".to_string(), "NotifyCustomer".to_string()]
        );
    }

    #[test]
    fn extract_emitted_commands_recognises_integration_emit_command_idiom() {
        // CIOS-style: a helper that wraps emission so the surface API is
        // `Integration.emitCommand <Ctor> { … }` rather than the
        // `Command.Emit { command = … }` style used in the testbed.
        let src = r#"
toAction req = Integration.action \_ctx ->
  Integration.emitCommand
    SendThankYouEmail
      { paymentId = req.paymentId
      , payerEmail = req.payerEmail
      }
"#;
        assert_eq!(
            extract_emitted_commands(src, &[]),
            vec!["SendThankYouEmail".to_string()]
        );
    }

    #[test]
    fn extract_emitted_commands_catches_callback_return_type_record() {
        // The CIOS callback idiom: the command is built as a record literal in
        // an `onSuccess`/`onError` handler, NOT via emitCommand/Command.Emit.
        // Detected by the known-command record-construction scan.
        let src = r#"
onSuccess :: Started.Event -> Response -> CompleteMetricEvaluation
onSuccess event response =
  CompleteMetricEvaluation
    { evaluationId = event.entityId
    , score = 3
    }
"#;
        let known = vec!["CompleteMetricEvaluation".to_string()];
        assert_eq!(
            extract_emitted_commands(src, &known),
            vec!["CompleteMetricEvaluation".to_string()],
        );
        // Without the known-command set the keyword scan alone misses it.
        assert!(extract_emitted_commands(src, &[]).is_empty());
    }

    #[test]
    fn extract_emitted_commands_record_scan_ignores_non_command_records() {
        // A config/response record that is NOT a known command must not be
        // minted as an emitted command.
        let src = "x = OpenRouter.Request { model = \"m\" }\n";
        assert!(extract_emitted_commands(src, &["RequestPayment".to_string()]).is_empty());
    }

    #[test]
    fn extract_emitted_commands_dedups() {
        let src = r#"
Command.Emit { command = Foo {} }
Command.Emit { command = Foo {} }
"#;
        assert_eq!(extract_emitted_commands(src, &[]), vec!["Foo".to_string()]);
    }

    #[test]
    fn extract_event_payload_modules_pulls_module_qualifiers() {
        let src = r#"
data ProposalEvent
  = ProposalPdfUploaded PdfUploaded.Event
  | ProposalPdfTranscribed PdfTranscribed.Event
  | EvaluationTriggered EvaluationTriggered.Event
  deriving (Generic, Show)
"#;
        let mods = extract_event_payload_modules(src);
        assert!(mods.contains("PdfUploaded"), "got {mods:?}");
        assert!(mods.contains("PdfTranscribed"), "got {mods:?}");
        assert!(mods.contains("EvaluationTriggered"), "got {mods:?}");
    }

    #[test]
    fn extract_event_payload_modules_empty_for_inline_record_arms() {
        let src = r#"
data CartEvent
  = CartCreated { entityId :: Uuid }
  | ItemAdded { stockId :: Uuid }
  deriving (Generic)
"#;
        assert!(extract_event_payload_modules(src).is_empty());
    }

    #[test]
    fn events_in_domain_skips_payload_modules_and_keeps_constructors() {
        // Regression: an event sum whose constructor is entity-prefixed
        // (`ProposalPdfTranscribed`) but whose payload module is bare
        // (`Events/PdfTranscribed.hs`) must yield ONE event named after the
        // constructor — not two (constructor + dangling payload stem).
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("Events")).unwrap();
        std::fs::write(
            root.join("Event.hs"),
            "module Datalake.Proposal.Event where\n\
             data ProposalEvent\n  \
               = ProposalPdfTranscribed PdfTranscribed.Event\n  \
               | EvaluationTriggered EvaluationTriggered.Event\n  \
               deriving (Generic, Show)\n",
        )
        .unwrap();
        std::fs::write(root.join("Events/PdfTranscribed.hs"), "module X where\ndata Event = Event {}\n").unwrap();
        std::fs::write(
            root.join("Events/EvaluationTriggered.hs"),
            "module X where\ndata Event = Event {}\n",
        )
        .unwrap();
        let events: Vec<String> = events_in_domain(root).into_iter().map(|e| e.name).collect();
        assert_eq!(
            events,
            vec!["ProposalPdfTranscribed".to_string(), "EvaluationTriggered".to_string()],
            "payload stem PdfTranscribed must be suppressed; EvaluationTriggered (ctor==module) kept once",
        );
    }

    #[test]
    fn write_set_partial_update_lists_only_named_fields() {
        let arm = "    PaymentApproved e ->\n      entity { lifecycle = Approved, approvedAt = Just e.at }";
        let (fields, _values) = extract_record_writes(arm);
        let got: Vec<&str> = fields.iter().map(String::as_str).collect();
        assert_eq!(got, vec!["approvedAt", "lifecycle"]);
    }

    #[test]
    fn write_set_full_record_excludes_copy_through() {
        // `proposalId = entity.proposalId` is a copy-through (unchanged) and
        // must NOT count as written; `summary = Just e.s` is a genuine write.
        let arm = "  ProposalSummarized e ->\n    ProposalEntity\n      { proposalId = entity.proposalId\n      , summary = Just e.s\n      }";
        let (fields, _values) = extract_record_writes(arm);
        assert!(fields.contains("summary"), "got {fields:?}");
        assert!(!fields.contains("proposalId"), "copy-through must drop: {fields:?}");
    }

    #[test]
    fn write_set_records_hub_value_for_enum_field() {
        let arm = "    PaymentApproved e -> entity { lifecycle = Approved }";
        let (_fields, values) = extract_record_writes(arm);
        assert_eq!(values.get("lifecycle").map(String::as_str), Some("Approved"));
    }

    #[test]
    fn write_set_qualified_enum_value_strips_module() {
        let arm = "    Requested e -> entity { lifecycle = Lifecycle.Requested }";
        let (_fields, values) = extract_record_writes(arm);
        assert_eq!(values.get("lifecycle").map(String::as_str), Some("Requested"));
    }

    #[test]
    fn single_ctor_token_rejects_applications() {
        assert_eq!(single_ctor_token("Approved").as_deref(), Some("Approved"));
        assert_eq!(single_ctor_token("Lifecycle.Approved").as_deref(), Some("Approved"));
        assert_eq!(single_ctor_token("Just e.x"), None);
        assert_eq!(single_ctor_token("e.field"), None);
    }

    #[test]
    fn event_write_sets_parses_update_fold() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::write(
            root.join("Entity.hs"),
            "module X.Entity where\nupdate :: E -> Ent -> Ent\nupdate event entity =\n  case event of\n    Created e -> Ent { name = e.name, status = Open }\n    Closed e -> entity { status = Closed }\n",
        )
        .unwrap();
        let a = event_write_sets_in_domain(root, &["Created".to_string(), "Closed".to_string()]);
        assert!(a.fold_found);
        assert!(a.writes["Created"].contains("name"));
        assert!(a.writes["Created"].contains("status"));
        assert_eq!(a.writes["Closed"].iter().map(String::as_str).collect::<Vec<_>>(), vec!["status"]);
        assert_eq!(a.enum_values["Closed"].get("status").map(String::as_str), Some("Closed"));
    }

    #[test]
    fn event_write_sets_handles_evolve_named_fold() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::write(
            root.join("Entity.hs"),
            "module X.Entity where\nevolve :: E -> Ent -> Ent\nevolve event entity =\n  case event of\n    Tick e -> entity { count = e.n }\n",
        )
        .unwrap();
        let a = event_write_sets_in_domain(root, &["Tick".to_string()]);
        assert!(a.fold_found);
        assert!(a.writes["Tick"].contains("count"));
    }

    #[test]
    fn event_write_sets_absent_fold_is_low_confidence() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("Core.hs"), "module X.Core where\nfoo = 1\n").unwrap();
        let a = event_write_sets_in_domain(dir.path(), &["Created".to_string()]);
        assert!(!a.fold_found);
        assert!(a.writes.is_empty());
    }

    #[test]
    fn extract_method_body_slurps_indented_combine() {
        let src = "instance QueryOf Ent View where\n  queryId e = e.id\n  combine entity _ =\n    case entity.status of\n      Open -> Update View {}\n      Closed -> NoOp\n\nderiveQuery ''View [''Ent]\n";
        let body = extract_method_body(src, "combine").expect("combine body");
        assert!(body.contains("case entity.status"));
        assert!(body.contains("NoOp"));
        assert!(!body.contains("queryId"), "must stop before sibling method: {body}");
        assert!(!body.contains("deriveQuery"));
    }

    #[test]
    fn extract_entity_field_reads_collects_accessors() {
        let combine = "combine entity _ = Update V { a = entity.lifecycle, b = entity.amount, c = entity.lifecycle }";
        assert_eq!(
            extract_entity_field_reads(combine),
            vec!["lifecycle".to_string(), "amount".to_string()]
        );
    }

    #[test]
    fn extract_combine_noop_values_flat_case() {
        let combine = "combine entity _ =\n  case entity.lifecycle of\n    Lifecycle.Open -> Update V {}\n    Lifecycle.Closed -> NoOp\n    Lifecycle.Archived -> NoOp\n";
        let (field, noop) = extract_combine_noop_values(combine);
        assert_eq!(field.as_deref(), Some("lifecycle"));
        let noop = noop.expect("flat case ⇒ Some");
        assert!(noop.contains("Closed") && noop.contains("Archived"));
        assert!(!noop.contains("Open"), "Open is an Update branch: {noop:?}");
    }

    #[test]
    fn extract_combine_noop_values_inline_branches() {
        // Inline `Pat -> Update (...)` / `Pat -> NoOp` on one line.
        let combine = "combine entity _ =\n  case entity.lifecycle of\n    Lifecycle.Approved -> Update (mkRow entity)\n    Lifecycle.Pending -> NoOp\n";
        let (_field, noop) = extract_combine_noop_values(combine);
        let noop = noop.unwrap();
        assert!(noop.contains("Pending"));
        assert!(!noop.contains("Approved"));
    }

    #[test]
    fn extract_combine_noop_values_nested_case_returns_none() {
        // More than one `case entity.<f> of` ⇒ not flat ⇒ no NoOp set ⇒ caller falls back.
        let combine = "combine entity _ =\n  case entity.fileRef of\n    Nothing -> NoOp\n    Just _ -> case entity.status of\n      Done -> Update V {}\n      _ -> NoOp\n";
        let (_field, noop) = extract_combine_noop_values(combine);
        assert!(noop.is_none(), "nested case must yield None NoOp set");
    }

    #[test]
    fn extract_combine_noop_values_no_case_returns_none() {
        let combine = "combine entity _ = Update V { x = entity.a }";
        let (field, noop) = extract_combine_noop_values(combine);
        assert!(field.is_none());
        assert!(noop.is_none());
    }

    #[test]
    fn events_in_domain_keeps_bare_file_events_when_no_sum_type() {
        // Older one-event-per-file projects have no `data XEvent` block, so
        // the Events/ stems ARE the events — preserve that.
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("Events")).unwrap();
        std::fs::write(root.join("Core.hs"), "module X.Core where\n").unwrap();
        std::fs::write(root.join("Events/ThingHappened.hs"), "module X where\ndata Event = Event {}\n").unwrap();
        let events: Vec<String> = events_in_domain(root).into_iter().map(|e| e.name).collect();
        assert_eq!(events, vec!["ThingHappened".to_string()]);
    }

    // --- record-field extraction -----------------------------------------

    fn rf(name: &str, ty: &str) -> RecordField {
        RecordField {
            name: name.to_string(),
            type_name: ty.to_string(),
        }
    }

    #[test]
    fn parse_record_fields_single() {
        assert_eq!(parse_record_fields("entityId :: Uuid"), vec![rf("entityId", "Uuid")]);
    }

    #[test]
    fn parse_record_fields_multiple_in_source_order() {
        assert_eq!(
            parse_record_fields("cartId :: Uuid, stockId :: Uuid, quantity :: Int"),
            vec![rf("cartId", "Uuid"), rf("stockId", "Uuid"), rf("quantity", "Int")],
        );
    }

    #[test]
    fn parse_record_fields_multiline_and_leading_comma() {
        let inner = "\n    ownerId :: Text\n  , itemCount :: Int\n  ";
        assert_eq!(
            parse_record_fields(inner),
            vec![rf("ownerId", "Text"), rf("itemCount", "Int")],
        );
    }

    #[test]
    fn parse_record_fields_qualified_and_applied_types_kept_verbatim() {
        assert_eq!(
            parse_record_fields("a :: Maybe Text, b :: Map Text Int, c :: Payload.Thing"),
            vec![rf("a", "Maybe Text"), rf("b", "Map Text Int"), rf("c", "Payload.Thing")],
        );
    }

    #[test]
    fn parse_record_fields_tuple_type_not_split_on_inner_comma() {
        assert_eq!(parse_record_fields("pair :: (Int, Text)"), vec![rf("pair", "(Int, Text)")]);
    }

    #[test]
    fn parse_record_fields_strips_trailing_line_comment() {
        assert_eq!(parse_record_fields("a :: Int -- the count"), vec![rf("a", "Int")]);
    }

    #[test]
    fn parse_record_fields_ignores_record_update_equals() {
        // A record UPDATE uses `=`, not `::`, so it contributes no fields —
        // this is what keeps a fieldless command from absorbing its `decide`
        // body's record updates.
        assert!(parse_record_fields("entityId = cart.cartId, quantity = cmd.quantity").is_empty());
    }

    #[test]
    fn parse_field_decl_rejects_non_fields() {
        assert!(parse_field_decl("not a field").is_none());
        assert!(parse_field_decl("Uppercase :: Int").is_none());
        assert!(parse_field_decl("   ").is_none());
        assert!(parse_field_decl("x ::").is_none());
    }

    #[test]
    fn brace_body_balances_nested_braces() {
        assert_eq!(
            brace_body("X { a :: Rec { y :: Int } }").as_deref(),
            Some(" a :: Rec { y :: Int } "),
        );
        assert!(brace_body("no braces here").is_none());
    }

    #[test]
    fn data_type_record_fields_extracts_command_payload_only() {
        // The `decide` body builds a record by UPDATE (`=`) — those must not
        // leak into the command's own declared fields.
        let src = "module M where\n\
                   data AddItem = AddItem { cartId :: Uuid, quantity :: Int }\n\
                   decide cmd _ _ = Decider.acceptExisting [ItemAdded { entityId = cmd.cartId }]\n";
        assert_eq!(
            data_type_record_fields(src, "AddItem"),
            vec![rf("cartId", "Uuid"), rf("quantity", "Int")],
        );
    }

    #[test]
    fn data_type_record_fields_empty_for_recordless_and_excludes_deriving() {
        assert!(data_type_record_fields("data Ping = Ping\n", "Ping").is_empty());
        assert_eq!(
            data_type_record_fields("data Foo = Foo { x :: Int } deriving (Show, Eq)\n", "Foo"),
            vec![rf("x", "Int")],
        );
    }

    #[test]
    fn extract_event_constructor_fields_per_constructor() {
        let src = "data CartEvent\n  \
                     = CartCreated { entityId :: Uuid, ownerId :: Text }\n  \
                     | ItemAdded { entityId :: Uuid, quantity :: Int }\n  \
                     deriving (Generic)\n";
        let map = extract_event_constructor_fields(src);
        assert_eq!(
            map.get("CartCreated").unwrap(),
            &vec![rf("entityId", "Uuid"), rf("ownerId", "Text")],
        );
        assert_eq!(
            map.get("ItemAdded").unwrap(),
            &vec![rf("entityId", "Uuid"), rf("quantity", "Int")],
        );
    }

    #[test]
    fn extract_event_constructor_fields_empty_for_payload_module_arm() {
        let src = "data ProposalEvent\n  \
                     = ProposalPdfUploaded PdfUploaded.Event\n  \
                     | EvaluationTriggered EvaluationTriggered.Event\n  \
                     deriving (Generic)\n";
        let map = extract_event_constructor_fields(src);
        assert!(map.get("ProposalPdfUploaded").unwrap().is_empty());
    }

    #[test]
    fn extract_event_constructor_fields_deterministic_under_reparse() {
        let src = "data E = A { x :: Int, y :: Text } | B { z :: Bool }\n  deriving (Generic)\n";
        assert_eq!(
            extract_event_constructor_fields(src),
            extract_event_constructor_fields(src),
        );
    }

    #[test]
    fn events_in_domain_attaches_inline_record_fields() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::write(
            root.join("Core.hs"),
            "module App.Cart.Core where\n\
             data CartEvent\n  \
               = CartCreated { entityId :: Uuid, ownerId :: Text }\n  \
               | ItemAdded { quantity :: Int }\n  \
               deriving (Generic)\n",
        )
        .unwrap();
        let events = events_in_domain(root);
        let created = events.iter().find(|e| e.name == "CartCreated").unwrap();
        assert_eq!(created.fields, vec![rf("entityId", "Uuid"), rf("ownerId", "Text")]);
        let added = events.iter().find(|e| e.name == "ItemAdded").unwrap();
        assert_eq!(added.fields, vec![rf("quantity", "Int")]);
    }

    #[test]
    fn extract_event_constructor_payload_modules_maps_ctor_to_module() {
        let src = "data CounterEvent\n  \
                     = CounterCreated CounterCreated.Event\n  \
                     | ProposalPdfUploaded PdfUploaded.Event\n  \
                     deriving (Generic)\n";
        let map = extract_event_constructor_payload_modules(src);
        assert_eq!(map.get("CounterCreated").map(String::as_str), Some("CounterCreated"));
        assert_eq!(map.get("ProposalPdfUploaded").map(String::as_str), Some("PdfUploaded"));
    }

    #[test]
    fn events_in_domain_reads_payload_module_fields() {
        // The real starter pattern: the sum arm is `Ctor Module.Event` and the
        // fields live in `Events/<Module>.hs` as `data Event = Event { … }`.
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("Events")).unwrap();
        std::fs::write(
            root.join("Event.hs"),
            "module Starter.Counter.Event (CounterEvent (..)) where\n\
             data CounterEvent\n  \
               = CounterCreated CounterCreated.Event\n  \
               | CounterIncremented CounterIncremented.Event\n  \
               deriving (Generic, Show)\n",
        )
        .unwrap();
        std::fs::write(
            root.join("Events/CounterCreated.hs"),
            "module Starter.Counter.Events.CounterCreated (Event (..)) where\n\
             data Event = Event\n  \
               { entityId :: Uuid\n  \
               , label :: Text\n  \
               }\n  \
               deriving (Generic, Show)\n",
        )
        .unwrap();
        std::fs::write(
            root.join("Events/CounterIncremented.hs"),
            "module Starter.Counter.Events.CounterIncremented (Event (..)) where\n\
             data Event = Event { entityId :: Uuid, amount :: Int } deriving (Generic)\n",
        )
        .unwrap();
        let events = events_in_domain(root);
        let created = events.iter().find(|e| e.name == "CounterCreated").expect("CounterCreated event");
        assert_eq!(created.fields, vec![rf("entityId", "Uuid"), rf("label", "Text")]);
        let incr = events.iter().find(|e| e.name == "CounterIncremented").expect("CounterIncremented event");
        assert_eq!(incr.fields, vec![rf("entityId", "Uuid"), rf("amount", "Int")]);
    }

    #[test]
    fn queries_in_domain_extracts_read_model_fields() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("Queries")).unwrap();
        std::fs::write(
            root.join("Queries/CounterView.hs"),
            "module Starter.Counter.Queries.CounterView (CounterView (..)) where\n\
             data CounterView = CounterView\n  \
               { counterId :: Uuid\n  \
               , label :: Text\n  \
               , value :: Int\n  \
               }\n  \
               deriving (Eq, Show, Generic)\n\n\
             instance QueryOf CounterEntity CounterView where\n  \
               combine entity _ = Update CounterView { counterId = entity.counterId }\n",
        )
        .unwrap();
        let queries = queries_in_domain(root, &[]);
        let q = queries.iter().find(|q| q.name == "CounterView").expect("CounterView query");
        assert_eq!(
            q.fields,
            vec![rf("counterId", "Uuid"), rf("label", "Text"), rf("value", "Int")],
            "read-model fields must come from the `data CounterView` record, not the combine update",
        );
    }
}
