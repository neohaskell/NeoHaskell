//! Core logic for `neo skills setup`.
//!
//! Discovers skills in a cloned `neohaskell/skills` checkout and plans/installs
//! them into the per-tool folders of a project root. This module is
//! deliberately free of terminal and network concerns so it is fully
//! unit-testable without a TTY or a real clone:
//!
//!   - the clone lives in [`crate::network::fetch_skills_repo`];
//!   - the interactive picker + orchestration live in [`crate::commands::skills`].
//!
//! A source skill is a directory `skills/<name>/SKILL.md` in the
//! [agentskills.io](https://agentskills.io) format: a `---`-fenced frontmatter
//! header with at least `name:` and `description:`, followed by the skill body.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::errors::NeoError;

// ---------------------------------------------------------------------------
// Supported tools + install strategies
// ---------------------------------------------------------------------------

/// How a discovered skill is materialized for a given tool.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Strategy {
    /// Copy the whole `<name>/` skill folder verbatim into `<dir>/<name>/`.
    /// Used by tools that natively read the agentskills.io `SKILL.md` format
    /// (bundled scripts/references are preserved).
    FolderCopy { dir: &'static str },
    /// Render each skill to a single `<dir>/<name>.mdc` file with Cursor
    /// frontmatter. Bundled scripts cannot travel into a single file (warned).
    CursorRule { dir: &'static str },
}

/// A supported AI coding agent and where its skills live in the project root.
#[derive(Debug, Clone, Copy)]
pub struct Tool {
    pub id: &'static str,
    pub display: &'static str,
    pub strategy: Strategy,
}

impl Tool {
    /// The project-root-relative folder or file this tool installs into, for
    /// display in the picker and the summary line.
    pub fn dest_hint(&self) -> &'static str {
        match self.strategy {
            Strategy::FolderCopy { dir } => dir,
            Strategy::CursorRule { dir } => dir,
        }
    }
}

/// The v1 tool set: the three tools that natively read the `SKILL.md` format
/// (folder-copied verbatim), plus Cursor.
///
/// The exact destination strings carry sharp collision traps — see the tests:
///   - Codex installs to `.agents/skills` (plural), NOT `.codex/skills`.
///   - `.agents/skills` (Codex) is one char from `.agent/rules` (Antigravity,
///     not yet supported) — do not "fix" it.
///
/// There is deliberately no universal `AGENTS.md` "tool". Each tool gets its
/// skills as discrete files, and setup leaves project instruction files alone.
pub const SUPPORTED_TOOLS: &[Tool] = &[
    Tool { id: "claude", display: "Claude Code", strategy: Strategy::FolderCopy { dir: ".claude/skills" } },
    Tool { id: "codex", display: "OpenAI Codex CLI", strategy: Strategy::FolderCopy { dir: ".agents/skills" } },
    Tool { id: "kiro", display: "Kiro (AWS)", strategy: Strategy::FolderCopy { dir: ".kiro/skills" } },
    Tool { id: "cursor", display: "Cursor", strategy: Strategy::CursorRule { dir: ".cursor/rules" } },
];

/// Comma-separated list of valid `--tool` ids, for error help text.
pub fn valid_tool_ids() -> String {
    SUPPORTED_TOOLS.iter().map(|t| t.id).collect::<Vec<_>>().join(", ")
}

fn tool_by_id(id: &str) -> Option<&'static Tool> {
    SUPPORTED_TOOLS.iter().find(|t| t.id == id)
}

/// Resolve a list of `--tool` id strings to [`Tool`] refs, de-duplicating and
/// erroring actionably on any unknown id.
pub fn resolve_tools(ids: &[String]) -> miette::Result<Vec<&'static Tool>> {
    let mut out: Vec<&'static Tool> = Vec::new();
    for id in ids {
        match tool_by_id(id) {
            Some(t) => {
                if !out.iter().any(|x| x.id == t.id) {
                    out.push(t);
                }
            }
            None => {
                return Err(miette::miette!(
                    help = format!(
                        "Valid --tool ids are: {}. Re-run e.g. `neo skills setup --tool claude --tool cursor`, or `--all-tools` to install for every supported tool.",
                        valid_tool_ids()
                    ),
                    "selecting tools for `neo skills setup`: `{}` is not a supported tool id.",
                    id,
                ));
            }
        }
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// Frontmatter parsing
// ---------------------------------------------------------------------------

/// Parse a leading `---`-fenced frontmatter block into a flat map of
/// single-line `key: value` pairs, returning `(frontmatter, body)`.
///
/// Only flat single-line scalar values are supported (sufficient for the
/// agentskills.io `name` + `description` fields). Surrounding single or double
/// quotes are stripped; keys and values are trimmed. A leading UTF-8 BOM and
/// `\r\n` line endings are tolerated. A document without a leading `---` fence,
/// or without a matching closing `---`, yields an empty map and the whole input
/// as the body.
pub fn parse_front_matter(input: &str) -> (BTreeMap<String, String>, String) {
    let stripped = input.strip_prefix('\u{feff}').unwrap_or(input);
    let mut lines = stripped.lines();
    match lines.next() {
        Some(l) if l.trim_end() == "---" => {}
        _ => return (BTreeMap::new(), input.to_string()),
    }

    let mut fm = BTreeMap::new();
    let mut closed = false;
    let mut body_lines: Vec<&str> = Vec::new();
    for line in lines.by_ref() {
        if !closed {
            if line.trim_end() == "---" {
                closed = true;
                continue;
            }
            if let Some((k, v)) = line.split_once(':') {
                let key = k.trim();
                if !key.is_empty() {
                    fm.insert(key.to_string(), strip_quotes(v.trim()).to_string());
                }
            }
        } else {
            body_lines.push(line);
        }
    }

    if !closed {
        return (BTreeMap::new(), input.to_string());
    }

    let mut body = body_lines.join("\n");
    if body.starts_with('\n') {
        body.remove(0);
    }
    (fm, body)
}

fn strip_quotes(v: &str) -> &str {
    let bytes = v.as_bytes();
    if bytes.len() >= 2 {
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
            return &v[1..v.len() - 1];
        }
    }
    v
}

// ---------------------------------------------------------------------------
// Skill discovery
// ---------------------------------------------------------------------------

/// A skill discovered in a `neohaskell/skills` checkout.
#[derive(Debug, Clone)]
pub struct Skill {
    pub name: String,
    pub description: String,
    /// Absolute path to the skill's source directory (contains `SKILL.md`).
    pub dir: PathBuf,
    /// The `SKILL.md` body (everything after the frontmatter fence).
    pub body: String,
    /// True when the skill folder bundles files other than `SKILL.md`
    /// (scripts/references) — preserved by FolderCopy, dropped by CursorRule.
    pub has_bundled_files: bool,
}

/// Discover all skills under `<checkout>/skills/`.
///
/// Returns an empty vec (not an error) when the `skills/` directory is absent —
/// that is the correct "nothing to install yet" signal against an empty
/// upstream repo. Each skill folder must contain a `SKILL.md` whose frontmatter
/// declares `name` (matching the folder) and `description`; a violation is an
/// actionable error naming the offending path.
pub fn discover_skills(checkout: &Path) -> miette::Result<Vec<Skill>> {
    let skills_root = checkout.join("skills");
    if !skills_root.is_dir() {
        return Ok(Vec::new());
    }

    let mut skills = Vec::new();
    let entries = std::fs::read_dir(&skills_root)
        .map_err(|e| NeoError::io_at("listing the skills directory at", &skills_root, e))?;
    for entry in entries {
        let entry = entry
            .map_err(|e| NeoError::io_at("reading an entry of the skills directory at", &skills_root, e))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|e| NeoError::io_at("reading the file type of", &path, e))?;
        if !file_type.is_dir() {
            // Ignore stray top-level files (README.md, LICENSE, …).
            continue;
        }
        let dir_name = entry.file_name().to_string_lossy().to_string();
        let skill_md = path.join("SKILL.md");
        if !skill_md.is_file() {
            return Err(miette::miette!(
                help = format!(
                    "Every skill in `neohaskell/skills` is a folder containing a `SKILL.md`. Add `skills/{dir}/SKILL.md` (agentskills.io format: a `---`-fenced `name:`/`description:` header, then the skill body), or remove the empty `skills/{dir}/` folder from the source repo.",
                    dir = dir_name
                ),
                "discovering skills: the skill folder `skills/{dir}/` has no `SKILL.md` (expected `skills/{dir}/SKILL.md`).",
                dir = dir_name,
            ));
        }

        let content = std::fs::read_to_string(&skill_md)
            .map_err(|e| NeoError::io_at("reading the skill manifest at", &skill_md, e))?;
        let (fm, body) = parse_front_matter(&content);
        let name = fm.get("name").map(String::as_str).unwrap_or("");
        let description = fm.get("description").map(String::as_str).unwrap_or("");

        if name.is_empty() {
            return Err(miette::miette!(
                help = format!(
                    "Add a `name:` line to the frontmatter of `{path}`. The file must start with a `---` fence, e.g.\n  ---\n  name: {dir}\n  description: Use when ...\n  ---\n  <skill body>",
                    path = skill_md.display(),
                    dir = dir_name
                ),
                "discovering skills: `{path}` is missing the required `name` frontmatter field (got name=\"\").",
                path = skill_md.display(),
            ));
        }
        if description.is_empty() {
            return Err(miette::miette!(
                help = format!(
                    "Add a `description:` line to the frontmatter of `{path}` — one line summarizing when the skill applies, e.g. `description: Use when adding a new event slice`.",
                    path = skill_md.display()
                ),
                "discovering skills: `{path}` is missing the required `description` frontmatter field (got description=\"\").",
                path = skill_md.display(),
            ));
        }
        if name != dir_name {
            return Err(miette::miette!(
                help = format!(
                    "Rename the folder to `skills/{name}/`, or change the frontmatter `name` to `{dir}`, so they match (agentskills.io requires the skill name to equal its folder name).",
                    name = name,
                    dir = dir_name
                ),
                "discovering skills: skill folder `skills/{dir}/` declares `name: {name}` — the folder name and frontmatter name must match.",
                dir = dir_name,
                name = name,
            ));
        }

        let has_bundled_files = dir_has_extra_files(&path, "SKILL.md")?;
        skills.push(Skill {
            name: name.to_string(),
            description: description.to_string(),
            dir: path,
            body,
            has_bundled_files,
        });
    }

    skills.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(skills)
}

fn dir_has_extra_files(dir: &Path, ignore: &str) -> miette::Result<bool> {
    for entry in std::fs::read_dir(dir).map_err(|e| NeoError::io_at("listing the skill folder at", dir, e))? {
        let entry = entry.map_err(|e| NeoError::io_at("reading an entry of the skill folder at", dir, e))?;
        if entry.file_name() != ignore {
            return Ok(true);
        }
    }
    Ok(false)
}

/// Filter the discovered skills down to the `--skill`-requested subset.
/// An empty `wanted` keeps all. An unknown requested name is an actionable
/// error listing the available skills.
pub fn filter_skills(all: Vec<Skill>, wanted: &[String]) -> miette::Result<Vec<Skill>> {
    if wanted.is_empty() {
        return Ok(all);
    }
    let available: Vec<&str> = all.iter().map(|s| s.name.as_str()).collect();
    for w in wanted {
        if !available.iter().any(|n| n == w) {
            return Err(miette::miette!(
                help = format!(
                    "Available skills: {}. Omit `--skill` to install all of them.",
                    if available.is_empty() { "(none)".to_string() } else { available.join(", ") }
                ),
                "selecting skills for `neo skills setup`: no skill named `{}` exists in `neohaskell/skills`.",
                w,
            ));
        }
    }
    Ok(all.into_iter().filter(|s| wanted.iter().any(|w| w == &s.name)).collect())
}

// ---------------------------------------------------------------------------
// Renderers (CursorRule)
// ---------------------------------------------------------------------------

/// Render a skill as a Cursor `.mdc` rule file: Cursor frontmatter
/// (`description`/`globs`/`alwaysApply`) followed by the skill body.
pub fn render_cursor_mdc(skill: &Skill) -> String {
    format!(
        "---\ndescription: {desc}\nglobs:\nalwaysApply: false\n---\n\n{body}\n",
        desc = skill.description,
        body = skill.body.trim_end(),
    )
}

// ---------------------------------------------------------------------------
// Install plan
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Create,
    Overwrite,
    Skip,
}

/// One unit of installation work: a (tool, skill) destination and what would
/// happen to it.
#[derive(Debug, Clone)]
pub struct PlanItem {
    pub tool_id: &'static str,
    pub skill_name: String,
    pub dest: PathBuf,
    pub action: Action,
    /// Pre-rendered content for the single-file CursorRule strategy; `None` for
    /// FolderCopy (the source dir is copied at write time).
    pub rendered: Option<String>,
    /// Source skill dir for FolderCopy; `None` otherwise.
    pub src_dir: Option<PathBuf>,
    /// True when bundled scripts will be dropped (CursorRule of a bundled skill).
    pub warn_bundled: bool,
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SkillReceipts {
    schema: u32,
    entries: BTreeMap<String, String>,
}

const SKILL_RECEIPTS_PATH: &str = ".neo/skills-setup-receipts.json";
const LEGACY_PRIMER_MARKERS: [&str; 2] = [
    "<!-- BEGIN neohaskell-skills -->",
    "<!-- END neohaskell-skills -->",
];

pub fn legacy_primer_warnings(project_root: &Path) -> Vec<String> {
    let mut legacy_paths = Vec::new();
    for relative in [
        ".claude/neohaskell.md",
        ".agents/neohaskell.md",
        ".kiro/neohaskell.md",
        ".cursor/rules/neohaskell.mdc",
    ] {
        if project_root.join(relative).symlink_metadata().is_ok() {
            legacy_paths.push(relative.to_string());
        }
    }

    for relative in ["AGENTS.md", "CLAUDE.md"] {
        let path = project_root.join(relative);
        let Ok(metadata) = path.symlink_metadata() else {
            continue;
        };
        let has_legacy_content = if !metadata.file_type().is_file() || metadata.file_type().is_symlink() {
            true
        } else {
            std::fs::read_to_string(&path)
                .map(|contents| {
                    LEGACY_PRIMER_MARKERS.iter().any(|marker| contents.contains(marker))
                        || [
                            "@.claude/neohaskell.md",
                            "@.agents/neohaskell.md",
                            "@.kiro/neohaskell.md",
                        ]
                        .iter()
                        .any(|import| contents.contains(import))
                })
                .unwrap_or(true)
        };
        if has_legacy_content {
            legacy_paths.push(relative.to_string());
        }
    }

    if legacy_paths.is_empty() {
        Vec::new()
    } else {
        vec![format!(
            "legacy mutable NeoHaskell guidance remains in {}; setup preserved those files. The source-matched guide is at `.neo/agent-guidance/active.md`. Review and remove stale primer blocks, imports, or files if they conflict; `neo skills setup` does not edit instruction files",
            legacy_paths.iter().map(|path| format!("`{path}`")).collect::<Vec<_>>().join(", ")
        )]
    }
}

pub fn load_skill_receipts(project_root: &Path) -> miette::Result<(SkillReceipts, Option<String>)> {
    let path = project_root.join(SKILL_RECEIPTS_PATH);
    let metadata = match std::fs::symlink_metadata(&path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Ok((SkillReceipts { schema: 1, entries: BTreeMap::new() }, None));
        }
        Err(error) => return Err(NeoError::io_at("checking skill setup ownership receipts at", &path, error).into()),
    };
    if !metadata.file_type().is_file() || metadata.file_type().is_symlink() || metadata.len() > 256 * 1024 {
        return Ok((
            SkillReceipts { schema: 1, entries: BTreeMap::new() },
            Some(format!("ignoring invalid skill setup ownership receipt at `{}`; existing skills will be preserved unless `--force` is used", path.display())),
        ));
    }
    let bytes = std::fs::read(&path).map_err(|error| NeoError::io_at("reading skill setup ownership receipts at", &path, error))?;
    match serde_json::from_slice::<SkillReceipts>(&bytes) {
        Ok(receipts) if receipts.schema == 1 => Ok((receipts, None)),
        Ok(_) | Err(_) => Ok((
            SkillReceipts { schema: 1, entries: BTreeMap::new() },
            Some(format!("ignoring unsupported skill setup ownership receipts at `{}`; existing skills will be preserved unless `--force` is used", path.display())),
        )),
    }
}

pub fn preserve_developer_edits(
    project_root: &Path,
    plan: &mut [PlanItem],
    receipts: &SkillReceipts,
    force: bool,
) -> miette::Result<Vec<String>> {
    if force {
        return Ok(Vec::new());
    }
    let mut warnings = Vec::new();
    for item in plan.iter_mut().filter(|item| item.action == Action::Overwrite) {
        let key = receipt_key(project_root, &item.dest)?;
        let current = destination_digest(&item.dest)?;
        if receipts.entries.get(&key) != Some(&current) {
            item.action = Action::Skip;
            warnings.push(format!(
                "preserved developer-edited skill `{}` for `{}`; use `--force` to replace it",
                item.skill_name, item.tool_id
            ));
        }
    }
    Ok(warnings)
}

pub fn record_skill_install(
    project_root: &Path,
    receipts: &mut SkillReceipts,
    item: &PlanItem,
) -> miette::Result<()> {
    let key = receipt_key(project_root, &item.dest)?;
    receipts.entries.insert(key, destination_digest(&item.dest)?);
    Ok(())
}

pub fn save_skill_receipts(project_root: &Path, receipts: &SkillReceipts) -> miette::Result<()> {
    let path = project_root.join(SKILL_RECEIPTS_PATH);
    let parent = path.parent().expect("skill receipt path has parent");
    std::fs::create_dir_all(parent).map_err(|error| NeoError::io_at("creating skill receipt directory at", parent, error))?;
    let bytes = serde_json::to_vec_pretty(receipts)
        .map_err(|error| miette::miette!("serializing skill setup ownership receipts: {error}"))?;
    let mut temp = tempfile::NamedTempFile::new_in(parent)
        .map_err(|error| NeoError::io_at("creating temporary skill receipt at", parent, error))?;
    std::io::Write::write_all(&mut temp, &bytes)
        .map_err(|error| NeoError::io_at("writing temporary skill receipt at", temp.path(), error))?;
    temp.as_file().sync_all().map_err(|error| NeoError::io_at("syncing temporary skill receipt at", temp.path(), error))?;
    temp.persist(&path).map_err(|error| NeoError::io_at("promoting skill receipt to", &path, error.error))?;
    Ok(())
}

fn receipt_key(project_root: &Path, destination: &Path) -> miette::Result<String> {
    let relative = destination.strip_prefix(project_root).map_err(|_| miette::miette!(
        "skill destination `{}` is outside project root `{}`",
        destination.display(),
        project_root.display()
    ))?;
    Ok(relative.to_string_lossy().replace('\\', "/"))
}

fn destination_digest(destination: &Path) -> miette::Result<String> {
    let metadata = std::fs::symlink_metadata(destination)
        .map_err(|error| NeoError::io_at("checking installed skill destination at", destination, error))?;
    let mut digest = Sha256::new();
    if metadata.file_type().is_symlink() {
        return Err(miette::miette!("skill destination `{}` is a symbolic link", destination.display()));
    }
    if metadata.file_type().is_file() {
        let bytes = std::fs::read(destination)
            .map_err(|error| NeoError::io_at("reading installed skill file at", destination, error))?;
        digest.update(b"file\0");
        digest.update((bytes.len() as u64).to_be_bytes());
        digest.update(bytes);
    } else if metadata.file_type().is_dir() {
        for entry in walkdir::WalkDir::new(destination)
            .min_depth(1)
            .sort_by_file_name()
            .follow_links(false)
        {
            let entry = entry.map_err(|error| walkdir_io(error, destination))?;
            let relative = entry.path().strip_prefix(destination).expect("skill traversal remains under destination");
            if entry.file_type().is_symlink() {
                return Err(miette::miette!("skill destination `{}` contains a symbolic link", entry.path().display()));
            }
            if entry.file_type().is_file() {
                let path = relative.to_string_lossy();
                let bytes = std::fs::read(entry.path())
                    .map_err(|error| NeoError::io_at("reading installed skill file at", entry.path(), error))?;
                digest.update(b"file\0");
                digest.update((path.len() as u64).to_be_bytes());
                digest.update(path.as_bytes());
                digest.update((bytes.len() as u64).to_be_bytes());
                digest.update(bytes);
            } else if entry.file_type().is_dir() {
                let path = relative.to_string_lossy();
                digest.update(b"directory\0");
                digest.update((path.len() as u64).to_be_bytes());
                digest.update(path.as_bytes());
            } else {
                return Err(miette::miette!("skill destination `{}` contains a non-regular entry", entry.path().display()));
            }
        }
    } else {
        return Err(miette::miette!("skill destination `{}` is not a regular file or directory", destination.display()));
    }
    Ok(format!("{:x}", digest.finalize()))
}

/// Compute the install plan for the chosen tools and skills against
/// `project_root`. Pure: reads existing destinations to classify each item as
/// Create/Overwrite/Skip but writes nothing.
pub fn build_plan(
    project_root: &Path,
    tools: &[&Tool],
    skills: &[Skill],
) -> miette::Result<Vec<PlanItem>> {
    let mut plan = Vec::new();
    for tool in tools {
        match tool.strategy {
            Strategy::FolderCopy { dir } => {
                for skill in skills {
                    let dest = project_root.join(dir).join(&skill.name);
                    let action = folder_action(&skill.dir, &dest)?;
                    plan.push(PlanItem {
                        tool_id: tool.id,
                        skill_name: skill.name.clone(),
                        dest,
                        action,
                        rendered: None,
                        src_dir: Some(skill.dir.clone()),
                        warn_bundled: false,
                    });
                }
            }
            Strategy::CursorRule { dir } => {
                for skill in skills {
                    let dest = project_root.join(dir).join(format!("{}.mdc", skill.name));
                    let rendered = render_cursor_mdc(skill);
                    let action = file_action(&dest, &rendered)?;
                    plan.push(PlanItem {
                        tool_id: tool.id,
                        skill_name: skill.name.clone(),
                        dest,
                        action,
                        rendered: Some(rendered),
                        src_dir: None,
                        warn_bundled: skill.has_bundled_files,
                    });
                }
            }
        }
    }
    Ok(plan)
}

fn folder_action(src: &Path, dest: &Path) -> miette::Result<Action> {
    if !dest.exists() {
        return Ok(Action::Create);
    }
    if dirs_identical(src, dest)? {
        Ok(Action::Skip)
    } else {
        Ok(Action::Overwrite)
    }
}

fn file_action(dest: &Path, rendered: &str) -> miette::Result<Action> {
    match read_to_string_opt(dest)? {
        None => Ok(Action::Create),
        Some(existing) if existing == rendered => Ok(Action::Skip),
        Some(_) => Ok(Action::Overwrite),
    }
}

fn read_to_string_opt(path: &Path) -> miette::Result<Option<String>> {
    match std::fs::read_to_string(path) {
        Ok(s) => Ok(Some(s)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(NeoError::io_at("reading the existing file at", path, e).into()),
    }
}

fn relative_file_set(root: &Path) -> miette::Result<BTreeSet<PathBuf>> {
    let mut set = BTreeSet::new();
    for entry in walkdir::WalkDir::new(root).min_depth(1) {
        let entry = entry.map_err(|e| walkdir_io(e, root))?;
        if entry.file_type().is_file() {
            let rel = entry.path().strip_prefix(root).unwrap().to_path_buf();
            set.insert(rel);
        }
    }
    Ok(set)
}

fn dirs_identical(a: &Path, b: &Path) -> miette::Result<bool> {
    let a_files = relative_file_set(a)?;
    let b_files = relative_file_set(b)?;
    if a_files != b_files {
        return Ok(false);
    }
    for rel in &a_files {
        let ca = std::fs::read(a.join(rel))
            .map_err(|e| NeoError::io_at("reading a source skill file at", &a.join(rel), e))?;
        let cb = std::fs::read(b.join(rel))
            .map_err(|e| NeoError::io_at("reading an installed skill file at", &b.join(rel), e))?;
        if ca != cb {
            return Ok(false);
        }
    }
    Ok(true)
}

// ---------------------------------------------------------------------------
// Writers
// ---------------------------------------------------------------------------

/// Recursively copy `src` into `dst`, creating `dst` and parents. Does NOT
/// remove `dst` first — callers wanting a clean replace must do so themselves.
pub fn copy_dir_recursive(src: &Path, dst: &Path) -> miette::Result<()> {
    std::fs::create_dir_all(dst)
        .map_err(|e| NeoError::io_at("creating the destination skill directory at", dst, e))?;
    for entry in walkdir::WalkDir::new(src).min_depth(1) {
        let entry = entry.map_err(|e| walkdir_io(e, src))?;
        let rel = entry.path().strip_prefix(src).expect("walkdir entry is under src");
        let target = dst.join(rel);
        if entry.file_type().is_dir() {
            std::fs::create_dir_all(&target)
                .map_err(|e| NeoError::io_at("creating a skill subdirectory at", &target, e))?;
        } else {
            if let Some(parent) = target.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| NeoError::io_at("creating a skill parent directory at", parent, e))?;
            }
            std::fs::copy(entry.path(), &target)
                .map_err(|e| NeoError::io_at("copying a skill file to", &target, e))?;
        }
    }
    Ok(())
}

/// Apply one plan item to disk. Writes for Create/Overwrite, no-op for Skip.
/// FolderCopy overwrites are a clean replace (remove then copy).
pub fn apply_item(item: &PlanItem) -> miette::Result<()> {
    if item.action == Action::Skip {
        return Ok(());
    }
    match (&item.src_dir, &item.rendered) {
        (Some(src), None) => {
            if item.dest.exists() {
                std::fs::remove_dir_all(&item.dest)
                    .map_err(|e| NeoError::io_at("removing the existing skill directory at", &item.dest, e))?;
            }
            copy_dir_recursive(src, &item.dest)?;
        }
        (None, Some(content)) => {
            if let Some(parent) = item.dest.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| NeoError::io_at("creating the destination directory at", parent, e))?;
            }
            std::fs::write(&item.dest, content)
                .map_err(|e| NeoError::io_at("writing the rendered file to", &item.dest, e))?;
        }
        _ => unreachable!("a plan item is either a folder copy or a rendered file"),
    }
    Ok(())
}

fn walkdir_io(e: walkdir::Error, root: &Path) -> NeoError {
    let io = e
        .into_io_error()
        .unwrap_or_else(|| std::io::Error::other("walkdir traversal error"));
    NeoError::io_at("walking a skill directory at", root, io)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn write(root: &Path, rel: &str, body: &str) {
        let p = root.join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, body).unwrap();
    }

    /// Build a checkout with one skill `<name>` (optionally with a bundled
    /// script) and return the checkout dir.
    fn checkout_with(skill: &str, desc: &str, bundled: bool) -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path(),
            &format!("skills/{skill}/SKILL.md"),
            &format!("---\nname: {skill}\ndescription: {desc}\n---\n\n# {skill}\n\nBody text.\n"),
        );
        if bundled {
            write(dir.path(), &format!("skills/{skill}/scripts/run.sh"), "#!/bin/sh\necho hi\n");
        }
        dir
    }

    // ---- frontmatter ----

    #[test]
    fn parse_front_matter_valid() {
        let (fm, body) = parse_front_matter("---\nname: foo\ndescription: a bar\n---\n\n# Title\nbody\n");
        assert_eq!(fm.get("name").unwrap(), "foo");
        assert_eq!(fm.get("description").unwrap(), "a bar");
        assert_eq!(body, "# Title\nbody");
    }

    #[test]
    fn parse_front_matter_no_fence_is_all_body() {
        let (fm, body) = parse_front_matter("# Just markdown\nno frontmatter\n");
        assert!(fm.is_empty());
        assert_eq!(body, "# Just markdown\nno frontmatter\n");
    }

    #[test]
    fn parse_front_matter_unterminated_fence_is_all_body() {
        let input = "---\nname: foo\n(no closing fence)\n";
        let (fm, body) = parse_front_matter(input);
        assert!(fm.is_empty());
        assert_eq!(body, input);
    }

    #[test]
    fn parse_front_matter_handles_crlf_and_bom_and_quotes() {
        let (fm, body) =
            parse_front_matter("\u{feff}---\r\nname: \"foo\"\r\ndescription: 'a bar'\r\n---\r\nbody\r\n");
        assert_eq!(fm.get("name").unwrap(), "foo");
        assert_eq!(fm.get("description").unwrap(), "a bar");
        assert_eq!(body, "body");
    }

    #[test]
    fn parse_front_matter_empty_value_is_empty_string() {
        let (fm, _) = parse_front_matter("---\nname: foo\ndescription:\n---\n");
        assert_eq!(fm.get("name").unwrap(), "foo");
        assert_eq!(fm.get("description").unwrap(), "");
    }

    // ---- tool table ----

    #[test]
    fn tool_table_dest_paths_exact() {
        let by = |id: &str| SUPPORTED_TOOLS.iter().find(|t| t.id == id).unwrap();
        assert_eq!(by("claude").dest_hint(), ".claude/skills");
        // Codex is `.agents/skills` (plural) — NOT `.codex/skills`.
        assert_eq!(by("codex").dest_hint(), ".agents/skills");
        assert_ne!(by("codex").dest_hint(), ".codex/skills");
        // …and NOT `.agent/rules` (Antigravity, singular).
        assert_ne!(by("codex").dest_hint(), ".agent/rules");
        assert_eq!(by("kiro").dest_hint(), ".kiro/skills");
        assert_eq!(by("cursor").dest_hint(), ".cursor/rules");
        // There is no universal `AGENTS.md` "tool" — it was removed because
        // inlining every skill body into one file produced an unusable blob.
        assert!(SUPPORTED_TOOLS.iter().all(|t| t.id != "agents"));
        assert_eq!(SUPPORTED_TOOLS.len(), 4);
    }

    #[test]
    fn resolve_tools_unknown_id_errors_and_lists_valid() {
        let err = resolve_tools(&["bogus".to_string()]).unwrap_err();
        let rendered = format!("{err:?}");
        assert!(rendered.contains("bogus"), "should quote the bad id: {rendered}");
        assert!(rendered.contains("claude"), "should list valid ids: {rendered}");
    }

    #[test]
    fn resolve_tools_dedupes() {
        let tools = resolve_tools(&["claude".to_string(), "claude".to_string()]).unwrap();
        assert_eq!(tools.len(), 1);
    }

    // ---- discovery ----

    #[test]
    fn discover_skills_missing_dir_is_empty() {
        let dir = tempfile::tempdir().unwrap();
        assert!(discover_skills(dir.path()).unwrap().is_empty());
    }

    #[test]
    fn discover_skills_happy_sorted() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "skills/bbb/SKILL.md", "---\nname: bbb\ndescription: B\n---\nbody b\n");
        write(dir.path(), "skills/aaa/SKILL.md", "---\nname: aaa\ndescription: A\n---\nbody a\n");
        write(dir.path(), "skills/README.md", "ignored stray file");
        let skills = discover_skills(dir.path()).unwrap();
        let names: Vec<&str> = skills.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(names, vec!["aaa", "bbb"]);
    }

    #[test]
    fn discover_skills_missing_skill_md_errors() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("skills/broken")).unwrap();
        let err = discover_skills(dir.path()).unwrap_err();
        let rendered = format!("{err:?}");
        assert!(rendered.contains("skills/broken/SKILL.md"), "names expected path: {rendered}");
    }

    #[test]
    fn discover_skills_name_dir_mismatch_errors() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "skills/foo/SKILL.md", "---\nname: bar\ndescription: x\n---\n");
        let err = discover_skills(dir.path()).unwrap_err();
        let rendered = format!("{err:?}");
        assert!(rendered.contains("foo"), "{rendered}");
        assert!(rendered.contains("bar"), "{rendered}");
    }

    #[test]
    fn discover_skills_missing_description_errors() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "skills/foo/SKILL.md", "---\nname: foo\n---\n");
        let err = discover_skills(dir.path()).unwrap_err();
        assert!(format!("{err:?}").contains("description"));
    }

    #[test]
    fn discover_skills_flags_bundled_files() {
        let c = checkout_with("withscripts", "has a script", true);
        let skills = discover_skills(c.path()).unwrap();
        assert!(skills[0].has_bundled_files);
    }

    // ---- filter ----

    #[test]
    fn filter_skills_unknown_name_errors() {
        let c = checkout_with("foo", "d", false);
        let all = discover_skills(c.path()).unwrap();
        let err = filter_skills(all, &["nope".to_string()]).unwrap_err();
        assert!(format!("{err:?}").contains("nope"));
    }

    #[test]
    fn filter_skills_subset() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "skills/foo/SKILL.md", "---\nname: foo\ndescription: d\n---\n");
        write(dir.path(), "skills/bar/SKILL.md", "---\nname: bar\ndescription: d\n---\n");
        let all = discover_skills(dir.path()).unwrap();
        let got = filter_skills(all, &["foo".to_string()]).unwrap();
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].name, "foo");
    }

    // ---- renderers ----

    #[test]
    fn render_cursor_mdc_shape() {
        let skill = Skill {
            name: "foo".into(),
            description: "Use when X".into(),
            dir: PathBuf::from("/x"),
            body: "# Foo\n\ndo the thing".into(),
            has_bundled_files: false,
        };
        let out = render_cursor_mdc(&skill);
        assert!(out.starts_with("---\ndescription: Use when X\nglobs:\nalwaysApply: false\n---\n"));
        assert!(out.contains("do the thing"));
    }

    // ---- plan + write ----

    #[test]
    fn build_plan_create_then_skip_then_overwrite() {
        let root = tempfile::tempdir().unwrap();
        let c = checkout_with("foo", "d", false);
        let skills = discover_skills(c.path()).unwrap();
        let tools = resolve_tools(&["claude".into(), "cursor".into()]).unwrap();

        // First run: everything is Create.
        let plan = build_plan(root.path(), &tools, &skills).unwrap();
        assert!(plan.iter().all(|p| p.action == Action::Create));
        for item in &plan {
            apply_item(item).unwrap();
        }
        assert!(root.path().join(".claude/skills/foo/SKILL.md").exists());
        assert!(root.path().join(".cursor/rules/foo.mdc").exists());

        // Second run with identical inputs: everything is Skip (idempotent).
        let plan2 = build_plan(root.path(), &tools, &skills).unwrap();
        assert!(plan2.iter().all(|p| p.action == Action::Skip), "expected all Skip: {plan2:?}");

        // Mutate the installed claude SKILL.md → that item becomes Overwrite.
        std::fs::write(root.path().join(".claude/skills/foo/SKILL.md"), "tampered").unwrap();
        let plan3 = build_plan(root.path(), &tools, &skills).unwrap();
        let claude_item = plan3.iter().find(|p| p.tool_id == "claude").unwrap();
        assert_eq!(claude_item.action, Action::Overwrite);
    }

    #[test]
    fn folder_copy_overwrite_is_clean_replace() {
        let root = tempfile::tempdir().unwrap();
        let c = checkout_with("foo", "d", false);
        let skills = discover_skills(c.path()).unwrap();
        let tools = resolve_tools(&["claude".into()]).unwrap();
        for item in build_plan(root.path(), &tools, &skills).unwrap() {
            apply_item(&item).unwrap();
        }
        // Add a stale extra file under the installed dir, then re-install.
        let stale = root.path().join(".claude/skills/foo/STALE.txt");
        std::fs::write(&stale, "old").unwrap();
        std::fs::write(root.path().join(".claude/skills/foo/SKILL.md"), "tampered").unwrap();
        for item in build_plan(root.path(), &tools, &skills).unwrap() {
            apply_item(&item).unwrap();
        }
        // Clean replace: stale file gone, SKILL.md restored from source.
        assert!(!stale.exists(), "overwrite must remove stale files");
        let restored = std::fs::read_to_string(root.path().join(".claude/skills/foo/SKILL.md")).unwrap();
        assert!(restored.contains("name: foo"));
    }

    #[test]
    fn cursor_rule_flags_bundled_and_copy_preserves_them() {
        let root = tempfile::tempdir().unwrap();
        let c = checkout_with("foo", "d", true);
        let skills = discover_skills(c.path()).unwrap();
        let tools = resolve_tools(&["claude".into(), "cursor".into()]).unwrap();
        let plan = build_plan(root.path(), &tools, &skills).unwrap();
        let cursor_item = plan.iter().find(|p| p.tool_id == "cursor").unwrap();
        assert!(cursor_item.warn_bundled, "cursor item must flag dropped bundled scripts");
        for item in &plan {
            apply_item(item).unwrap();
        }
        // FolderCopy preserves the bundled script; CursorRule cannot.
        assert!(root.path().join(".claude/skills/foo/scripts/run.sh").exists());
        assert!(root.path().join(".cursor/rules/foo.mdc").exists());
    }

    #[test]
    fn receipt_preserves_edited_skill_until_force_is_requested() {
        let root = tempfile::tempdir().unwrap();
        let checkout = checkout_with("foo", "d", false);
        let discovered = discover_skills(checkout.path()).unwrap();
        let tools = resolve_tools(&["claude".into()]).unwrap();
        let first_plan = build_plan(root.path(), &tools, &discovered).unwrap();
        let mut receipts = SkillReceipts { schema: 1, entries: BTreeMap::new() };
        for item in &first_plan {
            apply_item(item).unwrap();
            record_skill_install(root.path(), &mut receipts, item).unwrap();
        }
        save_skill_receipts(root.path(), &receipts).unwrap();

        let installed = root.path().join(".claude/skills/foo/SKILL.md");
        std::fs::write(&installed, "developer edit\n").unwrap();
        let mut normal_plan = build_plan(root.path(), &tools, &discovered).unwrap();
        let warnings = preserve_developer_edits(root.path(), &mut normal_plan, &receipts, false).unwrap();
        assert_eq!(normal_plan[0].action, Action::Skip);
        assert!(warnings[0].contains("preserved developer-edited skill"));

        let mut forced_plan = build_plan(root.path(), &tools, &discovered).unwrap();
        assert!(preserve_developer_edits(root.path(), &mut forced_plan, &receipts, true).unwrap().is_empty());
        assert_eq!(forced_plan[0].action, Action::Overwrite);
        apply_item(&forced_plan[0]).unwrap();
        assert!(std::fs::read_to_string(installed).unwrap().contains("name: foo"));
    }


}
