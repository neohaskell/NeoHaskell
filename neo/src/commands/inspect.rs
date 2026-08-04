//! `neo inspect` — print a structured view of the project's NeoHaskell
//! domain layout (commands, events, queries, integrations + derived wiring).
//!
//! Resolves the project root from CWD (no `neo.json` lookup required —
//! we treat CWD as the workspace root) and dispatches to the per-section
//! view based on the optional subcommand. Output is always JSON on stdout
//! so it composes well with `jq` and so the heal flow can splice it
//! straight into the agent prompt.

use std::collections::BTreeMap;

use serde_json::json;

use crate::cli::InspectSubcommand;
use crate::errors::NeoError;
use crate::inspect::{
    inspect_project, IntegrationKind, ProjectInspection,
};

pub fn run(sub: Option<InspectSubcommand>) -> miette::Result<()> {
    let cwd = std::env::current_dir().map_err(|e| {
        NeoError::io_at(
            "resolving current working directory for `neo inspect`",
            std::path::PathBuf::from("."),
            e,
        )
    })?;
    // `sync` is an ACTION, not a JSON view — it mutates event-model.json and
    // reports status, so it short-circuits before the view dispatch below.
    if matches!(sub, Some(InspectSubcommand::Sync)) {
        return run_sync(&cwd);
    }

    let project = inspect_project(&cwd);

    let value = match sub {
        None => all_view(&project),
        Some(InspectSubcommand::Domains) => domains_view(&project),
        Some(InspectSubcommand::Commands) => commands_view(&project),
        Some(InspectSubcommand::Events) => events_view(&project),
        Some(InspectSubcommand::Queries) => queries_view(&project),
        Some(InspectSubcommand::Integrations) => integrations_view(&project),
        Some(InspectSubcommand::Wiring) => wiring_view(&project),
        // Handled above (short-circuit). Listed so the match stays exhaustive.
        Some(InspectSubcommand::Sync) => unreachable!("inspect sync handled before view dispatch"),
    };

    let s = serde_json::to_string_pretty(&value).map_err(|e| {
        NeoError::TemplateError {
            template: "neo inspect JSON".to_string(),
            reason: e.to_string(),
        }
    })?;
    println!("{s}");
    Ok(())
}

/// `neo inspect sync` — force a code→model sync of `event-model.json` and print
/// status. Delegates to the shared `crate::ide::sync` engine that the `neo ide`
/// background watcher also drives, so the CLI and IDE can never diverge.
fn run_sync(cwd: &std::path::Path) -> miette::Result<()> {
    println!("[info] syncing event-model.json from source");
    let outcome = crate::ide::sync::sync_event_model(cwd)?;
    if outcome.applied == 0 {
        println!("[ok] event-model.json already in sync");
    } else {
        let mode = if outcome.ran_full_heal {
            "structural + layout"
        } else {
            "fields only, no layout change"
        };
        println!(
            "[ok] synced event-model.json — {} change(s) applied, {} node field-set(s) updated ({mode})",
            outcome.applied, outcome.fields_updated,
        );
    }
    Ok(())
}

fn all_view(project: &ProjectInspection) -> serde_json::Value {
    json!({
        "root": project.root,
        "domains": project.domains,
        "wiring": wiring_view(project)["events"],
    })
}

fn domains_view(project: &ProjectInspection) -> serde_json::Value {
    json!({
        "root": project.root,
        "domains": project.domains.iter().map(|d| json!({
            "name": d.name,
            "path": d.path,
            "counts": {
                "events": d.events.len(),
                "commands": d.commands.len(),
                "queries": d.queries.len(),
                "integrations": d.integrations.len(),
            }
        })).collect::<Vec<_>>()
    })
}

fn commands_view(project: &ProjectInspection) -> serde_json::Value {
    json!({
        "domains": project.domains.iter().map(|d| json!({
            "domain": d.name,
            "commands": d.commands,
        })).collect::<Vec<_>>()
    })
}

fn events_view(project: &ProjectInspection) -> serde_json::Value {
    json!({
        "domains": project.domains.iter().map(|d| json!({
            "domain": d.name,
            "events": d.events,
        })).collect::<Vec<_>>()
    })
}

fn queries_view(project: &ProjectInspection) -> serde_json::Value {
    json!({
        "domains": project.domains.iter().map(|d| json!({
            "domain": d.name,
            "queries": d.queries,
        })).collect::<Vec<_>>()
    })
}

fn integrations_view(project: &ProjectInspection) -> serde_json::Value {
    json!({
        "domains": project.domains.iter().map(|d| json!({
            "domain": d.name,
            "integrations": d.integrations,
        })).collect::<Vec<_>>()
    })
}

/// Cross-referenced wiring table — flips the per-section views into a
/// graph keyed by event constructor, which is what the heal prompt
/// actually needs to fill the model.
fn wiring_view(project: &ProjectInspection) -> serde_json::Value {
    let mut by_event: BTreeMap<String, EventWiring> = BTreeMap::new();

    for domain in &project.domains {
        for ev in &domain.events {
            by_event
                .entry(qualified(&domain.name, &ev.name))
                .or_insert_with(|| EventWiring::new(&domain.name, &ev.name));
        }
        for cmd in &domain.commands {
            for evname in &cmd.produces {
                let wiring = by_event
                    .entry(qualified(&domain.name, evname))
                    .or_insert_with(|| EventWiring::new(&domain.name, evname));
                wiring.produced_by.push(qualified(&domain.name, &cmd.name));
            }
        }
        for q in &domain.queries {
            for evname in &q.subscribes_to {
                let wiring = by_event
                    .entry(qualified(&domain.name, evname))
                    .or_insert_with(|| EventWiring::new(&domain.name, evname));
                wiring.feeds_queries.push(qualified(&domain.name, &q.name));
            }
        }
        for intg in &domain.integrations {
            for evname in &intg.handles_events {
                let wiring = by_event
                    .entry(qualified(&domain.name, evname))
                    .or_insert_with(|| EventWiring::new(&domain.name, evname));
                let entry = QualifiedIntegration {
                    name: qualified(&domain.name, &intg.name),
                    kind: intg.kind,
                    emits: intg.emits_commands.clone(),
                };
                wiring.triggers_integrations.push(entry);
            }
        }
    }

    json!({ "events": by_event.into_values().collect::<Vec<_>>() })
}

#[derive(serde::Serialize)]
struct EventWiring {
    event: String,
    produced_by: Vec<String>,
    feeds_queries: Vec<String>,
    triggers_integrations: Vec<QualifiedIntegration>,
}

#[derive(serde::Serialize)]
struct QualifiedIntegration {
    name: String,
    kind: IntegrationKind,
    emits: Vec<String>,
}

impl EventWiring {
    fn new(domain: &str, event: &str) -> Self {
        Self {
            event: qualified(domain, event),
            produced_by: Vec::new(),
            feeds_queries: Vec::new(),
            triggers_integrations: Vec::new(),
        }
    }
}

fn qualified(domain: &str, name: &str) -> String {
    format!("{domain}::{name}")
}

/// Public entry point used by the heal flow to splice everything into
/// the prompt. Returns the project as a pretty-printed JSON string, or
/// `None` if the workspace has no NeoHaskell domains (e.g. an empty repo,
/// or a non-NeoHaskell project).
pub fn project_summary_for_prompt(root: &std::path::Path) -> Option<String> {
    let project = inspect_project(root);
    if project.domains.is_empty() {
        return None;
    }
    let value = json!({
        "domains": project.domains.iter().map(|d| json!({
            "name": d.name,
            "events":       d.events.iter().map(|e| &e.name).collect::<Vec<_>>(),
            "commands":     d.commands.iter().map(|c| json!({
                "name": c.name,
                "produces": c.produces,
                "viaWebTransport": c.via_web_transport,
            })).collect::<Vec<_>>(),
            "queries":      d.queries.iter().map(|q| json!({
                "name": q.name,
                "subscribesTo": q.subscribes_to,
            })).collect::<Vec<_>>(),
            "integrations": d.integrations.iter().map(|i| json!({
                "name": i.name,
                "kind": i.kind,
                "handlesEvents": i.handles_events,
                "emitsCommands": i.emits_commands,
            })).collect::<Vec<_>>(),
        })).collect::<Vec<_>>(),
        "wiring": wiring_view(&project)["events"],
    });
    Some(serde_json::to_string_pretty(&value).ok()?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inspect::inspect_project;

    fn write(root: &std::path::Path, rel: &str, body: &str) {
        let full = root.join(rel);
        std::fs::create_dir_all(full.parent().unwrap()).unwrap();
        std::fs::write(full, body).unwrap();
    }

    fn fixture_project(root: &std::path::Path) {
        write(
            root,
            "src/App/Cart/Core.hs",
            "module App.Cart.Core where\ndata CartEvent = ItemAdded {} | CartCreated {} deriving (Generic)\n",
        );
        // Use a raw string so the Haskell-style indentation survives. The
        // dumb function-body extractor STOPS at any column-0 line that
        // isn't a continuation clause, so case arms MUST be indented like
        // real Haskell or the parser thinks the body ended at line 2.
        write(
            root,
            "src/App/Cart/Commands/AddItem.hs",
            r#"module App.Cart.Commands.AddItem where
decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide cmd _ _ = Decider.acceptExisting [ItemAdded {}]
type instance TransportsOf AddItem = '[WebTransport]
"#,
        );
        write(
            root,
            "src/App/Cart/Integrations/Bridge.hs",
            r#"module App.Cart.Integrations.Bridge where
handleEvent _ event = case event of
  ItemAdded {} -> Integration.outbound Command.Emit { command = NotifyStock {} }
  _ -> Integration.none
"#,
        );
        write(
            root,
            "src/App/Cart/Queries/CartSummary.hs",
            "module App.Cart.Queries.CartSummary where\n-- summary tracks ItemAdded and CartCreated\n",
        );
    }

    #[test]
    fn wiring_view_indexes_events_with_producers_and_handlers() {
        let dir = tempfile::tempdir().unwrap();
        fixture_project(dir.path());
        let project = inspect_project(dir.path());
        let value = wiring_view(&project);
        let events = value["events"].as_array().unwrap();
        let item_added = events
            .iter()
            .find(|e| e["event"].as_str() == Some("Cart::ItemAdded"))
            .expect("ItemAdded entry");
        assert_eq!(
            item_added["produced_by"].as_array().unwrap(),
            &[serde_json::json!("Cart::AddItem")]
        );
        assert_eq!(
            item_added["feeds_queries"].as_array().unwrap(),
            &[serde_json::json!("Cart::CartSummary")]
        );
        let intgs = item_added["triggers_integrations"].as_array().unwrap();
        assert_eq!(intgs.len(), 1);
        assert_eq!(intgs[0]["name"], "Cart::Bridge");
        assert_eq!(intgs[0]["kind"], "reactive");
        assert_eq!(intgs[0]["emits"].as_array().unwrap(), &[serde_json::json!("NotifyStock")]);
    }

    #[test]
    fn project_summary_for_prompt_returns_none_when_no_domains() {
        let dir = tempfile::tempdir().unwrap();
        assert!(project_summary_for_prompt(dir.path()).is_none());
    }

    #[test]
    fn project_summary_for_prompt_emits_pretty_json_with_wiring() {
        let dir = tempfile::tempdir().unwrap();
        fixture_project(dir.path());
        let s = project_summary_for_prompt(dir.path()).expect("summary");
        assert!(s.contains("\"domains\""));
        assert!(s.contains("\"wiring\""));
        assert!(s.contains("ItemAdded"));
        assert!(s.contains("AddItem"));
        assert!(s.contains("Bridge"));
        // Trailing newlines + indentation prove this is pretty-printed.
        assert!(s.contains('\n'));
    }

    #[test]
    fn domains_view_lists_counts_per_domain() {
        let dir = tempfile::tempdir().unwrap();
        fixture_project(dir.path());
        let project = inspect_project(dir.path());
        let v = domains_view(&project);
        let d = &v["domains"][0];
        assert_eq!(d["name"], "Cart");
        assert_eq!(d["counts"]["events"], 2);
        assert_eq!(d["counts"]["commands"], 1);
        assert_eq!(d["counts"]["queries"], 1);
        assert_eq!(d["counts"]["integrations"], 1);
    }
}
