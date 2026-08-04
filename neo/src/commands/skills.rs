//! `neo skills setup` — fetch github.com/neohaskell/skills and install the
//! skills into the per-tool folders of the current project root.
//!
//! Core logic (discovery, planning, rendering, copying) lives in
//! [`crate::skills`]; the clone lives in [`crate::network::fetch_skills_repo`].
//! This module is the command wiring: the interactive tool picker / overwrite
//! confirmation (Tier B1 ratatui) and the orchestration that prints
//! `[info]`/`[ok]` lines in CI.

use miette::IntoDiagnostic;
use ratatui::{crossterm::event::{Event, KeyCode}, Frame};

use crate::app::{Action, App, State};
use crate::cli::SkillsSubcommand;
use crate::errors::NeoError;
use crate::output::OutputMode;
use crate::skills::{self, Action as PlanAction, PlanItem, SUPPORTED_TOOLS};
use crate::theme::Theme;
use crate::tui::confirm::Confirm;
use crate::tui::multiselect::MultiSelect;

pub async fn run(
    subcommand: Option<SkillsSubcommand>,
    output_mode: &mut OutputMode,
) -> miette::Result<()> {
    // Only `setup` exists today; bare `neo skills` behaves as setup with no flags.
    let (tool_args, all_tools, skill_args, force, dry_run, refresh, no_primer) = match subcommand {
        Some(SkillsSubcommand::Setup { tools, all_tools, skills, force, dry_run, refresh, no_primer }) => {
            (tools, all_tools, skills, force, dry_run, refresh, no_primer)
        }
        None => (Vec::new(), false, Vec::new(), false, false, false, false),
    };

    let ci = output_mode.is_ci();

    // 1. Project root = current working directory.
    let project_root = std::env::current_dir().map_err(|e| {
        NeoError::io_at(
            "resolving the current working directory for `neo skills setup`",
            std::path::PathBuf::from("."),
            e,
        )
    })?;

    // 2. Fetch / refresh the skills library (before any TUI, so network errors
    //    print normally instead of inside the alternate screen).
    if ci {
        println!("[info] fetching skills from github.com/neohaskell/skills");
    } else {
        println!("Fetching skills from github.com/neohaskell/skills…");
    }
    let checkout = crate::network::fetch_skills_repo(refresh).await?;

    // 2b. Read the optional primer (`neohaskell.md`). Absent upstream → the whole
    //     primer feature is a clean skip; `--no-primer` disables it explicitly.
    let primer_body: Option<String> = if no_primer {
        None
    } else {
        skills::read_primer(&checkout)?
    };

    // 3. Discover skills.
    let all_skills = skills::discover_skills(&checkout)?;
    if all_skills.is_empty() {
        let msg = "no skills found in neohaskell/skills yet — nothing to install";
        if ci {
            println!("[info] {msg}");
        } else {
            println!("{msg}");
        }
        return Ok(());
    }
    let skills_list = skills::filter_skills(all_skills, &skill_args)?;

    // 4. Select tools.
    let selected_ids: Vec<String> = if all_tools {
        all_tool_ids()
    } else if !tool_args.is_empty() {
        tool_args
    } else if ci {
        println!("[info] no --tool given; defaulting to --all-tools");
        all_tool_ids()
    } else {
        match run_tool_picker().await? {
            Some(ids) if !ids.is_empty() => ids,
            Some(_) => {
                println!("No tools selected — nothing installed.");
                return Ok(());
            }
            None => {
                println!("Cancelled — nothing installed.");
                return Ok(());
            }
        }
    };

    let tools = skills::resolve_tools(&selected_ids)?;

    // 5. Build the install plan (skills + optional primer).
    let plan = skills::build_plan(&project_root, &tools, &skills_list)?;
    let primer_plan = match &primer_body {
        Some(body) => Some(skills::build_primer_plan(&project_root, &tools, body)?),
        None => None,
    };

    // 6. Dry run: print the plan and stop.
    if dry_run {
        print_plan(ci, &plan);
        if let Some(pp) = &primer_plan {
            print_primer_plan(ci, pp);
        }
        if ci {
            println!("[info] dry run — no files written");
        } else {
            println!("Dry run — no files written.");
        }
        return Ok(());
    }

    // 7. Confirm overwrites. Skill folders and primer FILE copies are gated by
    //    `--force`; the primer's managed-block WIRINGS are not (they only ever
    //    rewrite their own delimited region, never user content).
    let mut overwrite_dests: Vec<std::path::PathBuf> = plan
        .iter()
        .filter(|p| p.action == PlanAction::Overwrite)
        .map(|p| p.dest.clone())
        .collect();
    if let Some(pp) = &primer_plan {
        overwrite_dests.extend(
            pp.files
                .iter()
                .filter(|f| f.action == PlanAction::Overwrite)
                .map(|f| f.dest.clone()),
        );
    }
    let mut skip_overwrites = false;
    if !overwrite_dests.is_empty() && !force {
        if ci {
            let list = overwrite_dests
                .iter()
                .map(|d| format!("  - {}", d.display()))
                .collect::<Vec<_>>()
                .join("\n");
            return Err(miette::miette!(
                help = format!(
                    "Re-run with `--force` to overwrite these, or `--dry-run` to preview without writing. Conflicting destinations:\n{list}"
                ),
                "`neo skills setup --ci`: {} destination(s) already exist and would be overwritten.",
                overwrite_dests.len(),
            ));
        }
        let prompt = format!("Overwrite {} existing destination(s)?", overwrite_dests.len());
        if !run_confirm(&prompt).await? {
            skip_overwrites = true;
        }
    }

    // 8. Write.
    let mut created = 0usize;
    let mut overwritten = 0usize;
    let mut skipped = 0usize;
    for item in &plan {
        match item.action {
            PlanAction::Skip => skipped += 1,
            PlanAction::Overwrite if skip_overwrites => skipped += 1,
            PlanAction::Create => {
                skills::apply_item(item)?;
                created += 1;
            }
            PlanAction::Overwrite => {
                skills::apply_item(item)?;
                overwritten += 1;
            }
        }
        if item.warn_bundled && item.action != PlanAction::Skip {
            let note = format!(
                "note: skill `{}` bundles extra files that `{}` cannot use — installed the markdown body only",
                item.skill_name, item.tool_id
            );
            if ci {
                println!("[info]   {note}");
            } else {
                println!("  {note}");
            }
        }
    }

    // 8b. Write the primer: file copies honor the overwrite decision; wirings
    //     always apply (self-delimited), and any marker warnings are surfaced.
    if let Some(pp) = &primer_plan {
        for f in &pp.files {
            match f.action {
                PlanAction::Skip => skipped += 1,
                PlanAction::Overwrite if skip_overwrites => skipped += 1,
                PlanAction::Create => {
                    skills::apply_primer_file(f)?;
                    created += 1;
                }
                PlanAction::Overwrite => {
                    skills::apply_primer_file(f)?;
                    overwritten += 1;
                }
            }
        }
        for w in &pp.wires {
            for warn in &w.warnings {
                if ci {
                    println!("[warn]   {warn}");
                } else {
                    println!("  warning: {warn}");
                }
            }
            match w.action {
                PlanAction::Skip => skipped += 1,
                PlanAction::Create => {
                    skills::apply_primer_wire(w)?;
                    created += 1;
                }
                PlanAction::Overwrite => {
                    skills::apply_primer_wire(w)?;
                    overwritten += 1;
                }
            }
        }
    }

    // 9. Report.
    let dests = tools.iter().map(|t| t.dest_hint()).collect::<Vec<_>>().join(" ");
    if ci {
        println!(
            "[ok] installed {} skill(s) for {} tool(s) into {dests}  (created {created}, overwritten {overwritten}, skipped {skipped})",
            skills_list.len(),
            tools.len(),
        );
    } else {
        println!();
        println!("  Installed {} skill(s) for {} tool(s).", skills_list.len(), tools.len());
        println!("  created {created} · overwritten {overwritten} · skipped {skipped}");
        println!("  destinations: {dests}");
        println!();
    }
    if let Some(pp) = primer_plan.as_ref().filter(|pp| !pp.is_empty()) {
        if ci {
            println!(
                "[ok] primer neohaskell.md → {} file(s), wired into {} instructions file(s)",
                pp.files.len(),
                pp.wires.len(),
            );
        } else {
            println!(
                "  primer: neohaskell.md → {} file(s), wired into {} instructions file(s)",
                pp.files.len(),
                pp.wires.len(),
            );
            println!();
        }
    }
    Ok(())
}

fn all_tool_ids() -> Vec<String> {
    SUPPORTED_TOOLS.iter().map(|t| t.id.to_string()).collect()
}

fn print_plan(ci: bool, plan: &[PlanItem]) {
    for item in plan {
        let verb = match item.action {
            PlanAction::Create => "create",
            PlanAction::Overwrite => "overwrite",
            PlanAction::Skip => "skip (unchanged)",
        };
        if ci {
            println!("[info]   {verb}: {} → {}", item.skill_name, item.dest.display());
        } else {
            println!("  {verb}: {} → {}", item.skill_name, item.dest.display());
        }
    }
}

/// Plan-line verb for a primer item (the managed-block Overwrite reads as an
/// in-place "update", never a destructive overwrite).
fn primer_verb(action: PlanAction) -> &'static str {
    match action {
        PlanAction::Create => "create",
        PlanAction::Overwrite => "update",
        PlanAction::Skip => "skip (unchanged)",
    }
}

fn print_primer_plan(ci: bool, pp: &skills::PrimerPlan) {
    for f in &pp.files {
        let verb = primer_verb(f.action);
        if ci {
            println!("[info]   {verb}: neohaskell.md → {}", f.dest.display());
        } else {
            println!("  {verb}: neohaskell.md → {}", f.dest.display());
        }
    }
    for w in &pp.wires {
        let verb = primer_verb(w.action);
        if ci {
            println!("[info]   {verb}: primer {} → {}", w.label, w.dest.display());
        } else {
            println!("  {verb}: primer {} → {}", w.label, w.dest.display());
        }
        for warn in &w.warnings {
            if ci {
                println!("[warn]   {warn}");
            } else {
                println!("  warning: {warn}");
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Interactive tool picker (Tier B1)
// ---------------------------------------------------------------------------

struct SkillsSetupState {
    theme: Theme,
    ids: Vec<&'static str>,
    labels: Vec<&'static str>,
    hints: Vec<&'static str>,
    checked: Vec<bool>,
    cursor: usize,
}

impl SkillsSetupState {
    fn new(theme: Theme) -> Self {
        let ids: Vec<&'static str> = SUPPORTED_TOOLS.iter().map(|t| t.id).collect();
        let labels: Vec<&'static str> = SUPPORTED_TOOLS.iter().map(|t| t.display).collect();
        let hints: Vec<&'static str> = SUPPORTED_TOOLS.iter().map(|t| t.dest_hint()).collect();
        let checked = vec![false; ids.len()];
        Self { theme, ids, labels, hints, checked, cursor: 0 }
    }

    fn selected_ids(&self) -> Vec<String> {
        self.ids
            .iter()
            .zip(&self.checked)
            .filter(|(_, c)| **c)
            .map(|(id, _)| id.to_string())
            .collect()
    }
}

impl State for SkillsSetupState {
    /// `Some(ids)` = confirmed selection; `None` = cancelled.
    type Output = Option<Vec<String>>;

    fn view(&self, frame: &mut Frame) {
        let widget = MultiSelect::new(
            &self.theme,
            "Which AI coding tools do you use?",
            &self.labels,
            &self.hints,
            &self.checked,
            self.cursor,
        );
        frame.render_widget(widget, frame.area());
    }

    fn update(&mut self, event: Event) -> miette::Result<Action<Self::Output>> {
        if let Event::Key(key) = event {
            match key.code {
                KeyCode::Up => {
                    if self.cursor > 0 {
                        self.cursor -= 1;
                    }
                }
                KeyCode::Down => {
                    if self.cursor + 1 < self.ids.len() {
                        self.cursor += 1;
                    }
                }
                KeyCode::Char(' ') => {
                    let v = self.checked[self.cursor];
                    self.checked[self.cursor] = !v;
                }
                KeyCode::Enter => return Ok(Action::Quit(Some(self.selected_ids()))),
                KeyCode::Esc => return Ok(Action::Quit(None)),
                _ => {}
            }
        }
        Ok(Action::Continue)
    }

    fn tick(&mut self) {}
}

async fn run_tool_picker() -> miette::Result<Option<Vec<String>>> {
    let state = SkillsSetupState::new(Theme::neo());
    with_terminal(state).await
}

// ---------------------------------------------------------------------------
// Interactive overwrite confirmation (Tier B1)
// ---------------------------------------------------------------------------

struct ConfirmState {
    theme: Theme,
    prompt: String,
    value: bool,
}

impl State for ConfirmState {
    type Output = bool;

    fn view(&self, frame: &mut Frame) {
        let widget = Confirm::new(&self.theme, &self.prompt, self.value);
        frame.render_widget(widget, frame.area());
    }

    fn update(&mut self, event: Event) -> miette::Result<Action<bool>> {
        if let Event::Key(key) = event {
            match key.code {
                KeyCode::Left | KeyCode::Right | KeyCode::Char('h') | KeyCode::Char('l') => {
                    self.value = !self.value;
                }
                KeyCode::Char('y') | KeyCode::Char('Y') => return Ok(Action::Quit(true)),
                KeyCode::Char('n') | KeyCode::Char('N') => return Ok(Action::Quit(false)),
                KeyCode::Enter => return Ok(Action::Quit(self.value)),
                KeyCode::Esc => return Ok(Action::Quit(false)),
                _ => {}
            }
        }
        Ok(Action::Continue)
    }

    fn tick(&mut self) {}
}

async fn run_confirm(prompt: &str) -> miette::Result<bool> {
    let state = ConfirmState { theme: Theme::neo(), prompt: prompt.to_string(), value: true };
    with_terminal(state).await
}

/// Enter the alternate screen, run one `State` to completion, and restore the
/// terminal — mirrors the setup/teardown in `commands::new::run`.
async fn with_terminal<S: State>(state: S) -> miette::Result<S::Output> {
    crossterm::terminal::enable_raw_mode().into_diagnostic()?;
    crossterm::execute!(std::io::stdout(), crossterm::terminal::EnterAlternateScreen)
        .into_diagnostic()?;

    let backend = ratatui::backend::CrosstermBackend::new(std::io::stdout());
    let mut terminal = ratatui::Terminal::new(backend).into_diagnostic()?;
    let mut app = App::new(state, &mut terminal);
    let result = app.run().await;

    crossterm::execute!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen)
        .into_diagnostic()?;
    crossterm::terminal::disable_raw_mode().into_diagnostic()?;

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::crossterm::event::{KeyEvent, KeyModifiers};

    fn key(code: KeyCode) -> Event {
        Event::Key(KeyEvent::new(code, KeyModifiers::NONE))
    }

    #[test]
    fn test_skills_setup_state_toggle_confirm() {
        let mut state = SkillsSetupState::new(Theme::neo());
        // Move to index 1, check it; back to index 0, check it.
        state.update(key(KeyCode::Down)).unwrap();
        state.update(key(KeyCode::Char(' '))).unwrap();
        state.update(key(KeyCode::Up)).unwrap();
        state.update(key(KeyCode::Char(' '))).unwrap();
        match state.update(key(KeyCode::Enter)).unwrap() {
            Action::Quit(Some(ids)) => {
                // Returned in table order regardless of toggle order.
                assert_eq!(
                    ids,
                    vec![SUPPORTED_TOOLS[0].id.to_string(), SUPPORTED_TOOLS[1].id.to_string()]
                );
            }
            _ => panic!("expected Quit(Some(..))"),
        }
    }

    #[test]
    fn test_skills_setup_state_esc_cancels() {
        let mut state = SkillsSetupState::new(Theme::neo());
        match state.update(key(KeyCode::Esc)).unwrap() {
            Action::Quit(None) => {}
            _ => panic!("expected cancel"),
        }
    }

    #[test]
    fn test_confirm_state_toggle_and_enter() {
        let mut state = ConfirmState { theme: Theme::neo(), prompt: "ok?".into(), value: true };
        // Toggle to `n`, then Enter commits the toggled value.
        state.update(key(KeyCode::Left)).unwrap();
        match state.update(key(KeyCode::Enter)).unwrap() {
            Action::Quit(v) => assert!(!v),
            _ => panic!("expected Quit"),
        }
    }
}
