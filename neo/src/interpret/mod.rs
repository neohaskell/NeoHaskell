//! Subprocess-error interpretation: regex-matched `Interpreter` entries that
//! convert raw child output into a `cause + fix` recipe.
//!
//! The registry is a `&'static [&'static Interpreter]` declared in
//! `patterns::ALL`. Each entry is one file under `patterns/`. Adding a new
//! failure mode means writing one new pattern file and adding two lines to
//! `patterns/mod.rs` — no logic edits here. See the project-local skill
//! `.claude/skills/adding-error-interpreter-pattern/SKILL.md` for the
//! end-to-end workflow.
//!
//! No external config exists. There is no `~/.neo/interpreters.jsonl`, no
//! env-var override, no asset file. Bugs in the in-tree registry are caught
//! by compile-time consistency tests (`every_interpreter_regex_compiles`,
//! `every_template_var_has_a_named_capture`, etc.) — not at runtime.

pub mod patterns;

use regex::{Captures, Regex};
use std::sync::OnceLock;

/// Result of interpreting a captured subprocess output stream.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interpreted {
    pub cause: String,
    pub fix: String,
}

/// Which subprocess produced the failing output. Callers that know they're
/// only interested in one tool's failures (e.g. `network.rs` is always
/// invoking `git`) pass the specific `Kind` to avoid spurious cross-tool
/// matches.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Kind {
    Cabal,
    Nix,
    Git,
    Hurl,
}

/// One declarative interpreter entry. All fields are `&'static str` so the
/// whole table can live in a `const` slice — adding a new entry is a
/// one-struct-literal change in a dedicated file under `patterns/`.
pub struct Interpreter {
    pub id: &'static str,
    pub kind: Kind,
    /// Rust-regex syntax; use `(?<name>…)` named captures for any variable
    /// content you want to surface in the cause/fix templates.
    pub pattern: &'static str,
    /// Template; `{name}` is substituted with the named capture of the same
    /// name. Unknown `{name}` placeholders render verbatim (no panic).
    /// `{{` and `}}` escape to literal `{` and `}`.
    pub cause: &'static str,
    pub fix: &'static str,
}

/// Internal: precompiled form of an `Interpreter`. We hold this in a
/// `OnceLock` so each regex is built once per process. The `id` field is
/// retained for diagnostic surfaces (panic messages, future tracing) even
/// when the match path doesn't read it directly.
#[allow(dead_code)]
struct CompiledInterpreter {
    id: &'static str,
    kind: Kind,
    regex: Regex,
    cause: &'static str,
    fix: &'static str,
}

fn compiled() -> &'static [CompiledInterpreter] {
    static CELL: OnceLock<Vec<CompiledInterpreter>> = OnceLock::new();
    CELL.get_or_init(|| {
        patterns::ALL
            .iter()
            .map(|e| CompiledInterpreter {
                id: e.id,
                kind: e.kind,
                regex: Regex::new(e.pattern).unwrap_or_else(|err| {
                    panic!(
                        "interpreter `{}` has an invalid regex `{}`: {} \
                         — this is a bug in the in-tree registry; the \
                         `every_interpreter_regex_compiles` test should \
                         have caught it pre-merge",
                        e.id, e.pattern, err
                    )
                }),
                cause: e.cause,
                fix: e.fix,
            })
            .collect()
    })
}

/// Try every interpreter in document order; first match wins.
pub fn match_any(captured: &str) -> Option<Interpreted> {
    match_inner(None, captured)
}

/// Try only interpreters of the given kind; first match wins.
pub fn match_kind(kind: Kind, captured: &str) -> Option<Interpreted> {
    match_inner(Some(kind), captured)
}

fn match_inner(filter: Option<Kind>, captured: &str) -> Option<Interpreted> {
    for entry in compiled() {
        if let Some(k) = filter {
            if entry.kind != k {
                continue;
            }
        }
        if let Some(caps) = entry.regex.captures(captured) {
            return Some(Interpreted {
                cause: render(entry.cause, &caps),
                fix: render(entry.fix, &caps),
            });
        }
    }
    None
}

/// Substitute `{name}` placeholders in `template` with the corresponding
/// named capture from `caps`. Unknown placeholders render verbatim. `{{`
/// and `}}` escape to literal `{` and `}`. Single `{` not followed by a
/// valid `{name}` is left as a literal.
fn render(template: &str, caps: &Captures) -> String {
    let mut out = String::with_capacity(template.len());
    let bytes = template.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'{' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'{' {
                out.push('{');
                i += 2;
                continue;
            }
            if let Some(close) = template[i + 1..].find('}') {
                let name = &template[i + 1..i + 1 + close];
                // Reject anything that isn't a plausible capture-group name
                // (lower-ascii + digits + `_`) — keeps `{` in prose safe.
                if !name.is_empty()
                    && name.bytes().all(|b| {
                        b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'_'
                    })
                {
                    if let Some(m) = caps.name(name) {
                        out.push_str(m.as_str());
                    } else {
                        // Unknown capture name — render the placeholder
                        // verbatim, including the braces.
                        out.push('{');
                        out.push_str(name);
                        out.push('}');
                    }
                    i = i + 1 + close + 1;
                    continue;
                }
            }
            // Bare `{` with no matching `}` or invalid name → literal.
            out.push('{');
            i += 1;
        } else if c == b'}' && i + 1 < bytes.len() && bytes[i + 1] == b'}' {
            out.push('}');
            i += 2;
        } else {
            out.push(c as char);
            i += 1;
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---------------- public surface ----------------

    #[test]
    fn match_any_returns_none_on_empty_input() {
        assert_eq!(match_any(""), None);
    }

    #[test]
    fn match_any_returns_none_on_whitespace_input() {
        assert_eq!(match_any("   \n\n"), None);
    }

    #[test]
    fn match_any_returns_none_on_unknown_pattern() {
        assert_eq!(match_any("some unrelated noise"), None);
    }

    #[test]
    fn match_kind_filters_to_requested_kind() {
        // A cabal-only input must not match when asking for Git only.
        let cabal_input = "Resolving dependencies...\nError: unknown package: foo\n";
        assert_eq!(match_kind(Kind::Git, cabal_input), None);
    }

    #[test]
    fn match_any_first_entry_wins_on_overlap() {
        // Cabal entries come before nix entries in document order, so an
        // input that matches both must surface the cabal interpretation.
        let captured = "unknown package: foo\nattribute 'bar' missing\n";
        let i = match_any(captured).expect("should match");
        assert!(
            i.cause.contains("package `foo`"),
            "cabal should win, got cause: {}",
            i.cause
        );
    }

    #[test]
    fn compiled_registry_length_matches_const() {
        assert_eq!(compiled().len(), patterns::ALL.len());
    }

    // ---------------- render() ----------------

    fn caps_from(pattern: &str, haystack: &str) -> regex::Captures<'static> {
        let re = Box::leak(Box::new(Regex::new(pattern).unwrap()));
        let h: &'static str = Box::leak(haystack.to_string().into_boxed_str());
        re.captures(h).expect("no match")
    }

    #[test]
    fn render_substitutes_named_captures() {
        let caps = caps_from(r"(?<x>foo)", "foo");
        assert_eq!(render("got {x}", &caps), "got foo");
    }

    #[test]
    fn render_unknown_var_renders_literally() {
        let caps = caps_from(r"(?<x>foo)", "foo");
        assert_eq!(render("{nope} and {x}", &caps), "{nope} and foo");
    }

    #[test]
    fn render_escapes_double_braces() {
        let caps = caps_from(r"(?<x>foo)", "foo");
        assert_eq!(render("{{x}} = {x}", &caps), "{x} = foo");
    }

    #[test]
    fn render_preserves_unicode_in_capture() {
        let caps = caps_from(r"(?<ref>\S+)", "refs/heads/日本語");
        assert_eq!(render("got {ref}", &caps), "got refs/heads/日本語");
    }

    #[test]
    fn render_handles_template_without_placeholders() {
        let caps = caps_from(r"(?<x>foo)", "foo");
        assert_eq!(render("no vars here", &caps), "no vars here");
    }

    #[test]
    fn render_handles_lone_brace_in_prose() {
        // A `{` that doesn't introduce a valid `{name}` placeholder must be
        // left as-is so prose like "use { for blocks" doesn't crash or get
        // mangled.
        let caps = caps_from(r"(?<x>foo)", "foo");
        assert_eq!(render("use { for blocks", &caps), "use { for blocks");
    }

    // ---------------- consistency lints (guard future PRs) ----------------

    #[test]
    fn every_interpreter_regex_compiles() {
        for e in patterns::ALL {
            Regex::new(e.pattern).unwrap_or_else(|err| {
                panic!("interpreter `{}` has invalid regex `{}`: {}", e.id, e.pattern, err)
            });
        }
    }

    #[test]
    fn every_template_var_has_a_named_capture() {
        // For every `{name}` in cause/fix, the regex must declare a
        // matching `(?<name>…)` group. Catches typos like `{packge}`.
        let placeholder = Regex::new(r"\{([a-z][a-z0-9_]*)\}").unwrap();
        for e in patterns::ALL {
            let re = Regex::new(e.pattern).unwrap();
            let group_names: std::collections::HashSet<&str> = re
                .capture_names()
                .flatten()
                .collect();
            for template_field in [("cause", e.cause), ("fix", e.fix)] {
                let (field, template) = template_field;
                // Strip escaped `{{` first so we only inspect real placeholders.
                let stripped = template.replace("{{", "").replace("}}", "");
                for cap in placeholder.captures_iter(&stripped) {
                    let name = cap.get(1).unwrap().as_str();
                    assert!(
                        group_names.contains(name),
                        "interpreter `{}` references `{{{}}}` in its `{}` template, \
                         but its regex has no `(?<{}>…)` named-capture group",
                        e.id, name, field, name
                    );
                }
            }
        }
    }

    #[test]
    fn every_entry_id_is_unique_kebab() {
        let id_shape = Regex::new(r"^[a-z][a-z0-9-]*$").unwrap();
        let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
        for e in patterns::ALL {
            assert!(
                id_shape.is_match(e.id),
                "interpreter id `{}` must be kebab-case ASCII: ^[a-z][a-z0-9-]*$",
                e.id
            );
            assert!(
                seen.insert(e.id),
                "interpreter id `{}` is duplicated in `patterns::ALL`",
                e.id
            );
        }
    }

    #[test]
    fn entry_count_matches_documented() {
        // Day-one count is 11. Bump this when adding/removing entries —
        // the test failure forces an intentional update.
        assert_eq!(
            patterns::ALL.len(),
            11,
            "patterns::ALL has {} entries; update entry_count_matches_documented \
             after adding or removing an interpreter",
            patterns::ALL.len()
        );
    }
}
