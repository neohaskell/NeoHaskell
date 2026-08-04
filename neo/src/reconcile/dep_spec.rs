//! Parser and translator for `neo.json` dependency values.
//!
//! User-facing grammar (string → DependencyDecl):
//!
//! - `"name": "<npm-range>"`            → `Bare { req }` (NeoPackages registry)
//! - `"hackage:name": "<npm-range>"`    → `Hackage { req }`
//! - `"name": "git:<url>[#ref]"`        → `Git`
//! - `"name": "github:<owner>/<repo>[#ref]"` → `GitHub`
//! - `"name": "file:<path>"`            → `File`
//!
//! Values for `Bare` and `Hackage` are parsed as npm semver ranges
//! (caret, tilde, comparators, x-ranges, hyphen ranges, `||`, AND).
//! `to_cabal_constraint` renders a parsed range as cabal `build-depends` syntax.

use std::fmt::{self, Write};

use crate::errors::NeoError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencyDecl {
    Bare { req: NpmRange },
    Hackage { req: NpmRange },
    Git { url: String, git_ref: Option<String> },
    GitHub { owner_repo: String, git_ref: Option<String> },
    File { path: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NpmRange(pub Vec<NpmClause>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NpmClause(pub Vec<NpmConstraint>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NpmConstraint {
    pub op: NpmOp,
    pub version: NpmVersion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NpmOp { Ge, Gt, Le, Lt, Eq }

impl NpmOp {
    fn cabal_str(self) -> &'static str {
        match self {
            NpmOp::Ge => ">=",
            NpmOp::Gt => ">",
            NpmOp::Le => "<=",
            NpmOp::Lt => "<",
            NpmOp::Eq => "==",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NpmVersion {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub pre: Option<String>,
}

impl fmt::Display for NpmVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if let Some(pre) = &self.pre {
            write!(f, "-{}", pre)?;
        }
        Ok(())
    }
}

// ---------- top-level parse(key, value) ----------

pub fn parse(name_key: &str, value: &str) -> miette::Result<(String, DependencyDecl)> {
    let trimmed_value = value.trim();

    let (name, key_is_hackage) = if let Some(rest) = name_key.strip_prefix("hackage:") {
        if rest.is_empty() {
            return Err(invalid(name_key, value, "empty package name after `hackage:` key prefix"));
        }
        (rest.to_string(), true)
    } else {
        (name_key.to_string(), false)
    };

    let value_decl = if let Some(rest) = trimmed_value.strip_prefix("git:") {
        if rest.is_empty() {
            return Err(invalid(name_key, value, "empty URL after `git:` protocol prefix"));
        }
        let (url, git_ref) = split_ref(rest);
        Some(DependencyDecl::Git {
            url: url.to_string(),
            git_ref: git_ref.map(str::to_string),
        })
    } else if let Some(rest) = trimmed_value.strip_prefix("github:") {
        let (path, git_ref) = split_ref(rest);
        validate_owner_repo(name_key, value, path)?;
        Some(DependencyDecl::GitHub {
            owner_repo: path.to_string(),
            git_ref: git_ref.map(str::to_string),
        })
    } else if let Some(rest) = trimmed_value.strip_prefix("file:") {
        if rest.is_empty() {
            return Err(invalid(name_key, value, "empty path after `file:` protocol prefix"));
        }
        Some(DependencyDecl::File { path: rest.to_string() })
    } else if has_protocol_marker(trimmed_value) {
        return Err(invalid(name_key, value, "unknown protocol prefix"));
    } else {
        None
    };

    if key_is_hackage && value_decl.is_some() {
        return Err(invalid(name_key, value, "protocol on both key and value — use one or the other"));
    }

    let decl = if key_is_hackage {
        let req = parse_npm_range(trimmed_value)
            .map_err(|e| invalid(name_key, value, &e))?;
        DependencyDecl::Hackage { req }
    } else if let Some(d) = value_decl {
        d
    } else {
        let req = parse_npm_range(trimmed_value)
            .map_err(|e| invalid(name_key, value, &e))?;
        DependencyDecl::Bare { req }
    };

    Ok((name, decl))
}

fn invalid(key: &str, value: &str, reason: &str) -> miette::Report {
    // The pure parser has no NeoConfig in scope — spans are attached at the
    // caller (`resolve::resolve_decl`) where the file content is available.
    NeoError::InvalidDependency {
        key: key.to_string(),
        value: value.to_string(),
        reason: reason.to_string(),
        src: None,
        span: None,
    }
    .into()
}

fn split_ref(rest: &str) -> (&str, Option<&str>) {
    match rest.split_once('#') {
        Some((url, r)) if !r.is_empty() => (url, Some(r)),
        Some((url, _)) => (url, None),
        None => (rest, None),
    }
}

fn validate_owner_repo(key: &str, value: &str, path: &str) -> miette::Result<()> {
    let parts: Vec<&str> = path.split('/').collect();
    if parts.len() != 2 || parts[0].is_empty() || parts[1].is_empty() {
        return Err(invalid(key, value, "github: protocol expects `owner/repo` (exactly one `/`)"));
    }
    Ok(())
}

fn has_protocol_marker(value: &str) -> bool {
    // npm semver ranges never contain `:`. If we got here without matching a known
    // protocol prefix and the value has `:`, it's an unknown protocol.
    value.contains(':')
}

// ---------- npm range parser ----------

pub fn parse_npm_range(input: &str) -> Result<NpmRange, String> {
    let input = input.trim();
    if input.is_empty() {
        return Ok(NpmRange(vec![NpmClause(vec![])]));
    }
    let clause_strs: Vec<&str> = input.split("||").collect();
    let mut clauses = Vec::with_capacity(clause_strs.len());
    for c in clause_strs {
        clauses.push(parse_clause(c.trim())?);
    }
    Ok(NpmRange(clauses))
}

fn parse_clause(input: &str) -> Result<NpmClause, String> {
    if input.is_empty()
        || input == "*"
        || input == "x"
        || input == "X"
        || input == "latest"
    {
        return Ok(NpmClause(vec![]));
    }
    let tokens: Vec<&str> = input.split_whitespace().collect();

    if tokens.len() == 3 && tokens[1] == "-" {
        return parse_hyphen_range(tokens[0], tokens[2]);
    }
    if tokens.contains(&"-") {
        return Err(format!("invalid hyphen range: `{}`", input));
    }

    let mut constraints = Vec::new();
    for tok in tokens {
        constraints.extend(parse_atom(tok)?);
    }
    Ok(NpmClause(constraints))
}

fn parse_hyphen_range(lo: &str, hi: &str) -> Result<NpmClause, String> {
    let lo_p = parse_partial(lo)?;
    let hi_p = parse_partial(hi)?;
    let mut cs = Vec::new();
    cs.push(ge_partial(&lo_p));
    if hi_p.is_fully_specified() {
        cs.push(NpmConstraint {
            op: NpmOp::Le,
            version: hi_p.to_zero_filled_version(),
        });
    } else {
        cs.push(NpmConstraint {
            op: NpmOp::Lt,
            version: upper_bound_partial(&hi_p),
        });
    }
    Ok(NpmClause(cs))
}

fn parse_atom(tok: &str) -> Result<Vec<NpmConstraint>, String> {
    if tok == "*" || tok == "x" || tok == "X" || tok == "latest" {
        return Ok(vec![]);
    }
    if let Some(rest) = tok.strip_prefix('^') {
        return Ok(caret_expand(&parse_partial(rest)?));
    }
    if let Some(rest) = tok.strip_prefix('~') {
        return Ok(tilde_expand(&parse_partial(rest)?));
    }
    let (op, rest) = if let Some(r) = tok.strip_prefix(">=") {
        (Some(NpmOp::Ge), r)
    } else if let Some(r) = tok.strip_prefix("<=") {
        (Some(NpmOp::Le), r)
    } else if let Some(r) = tok.strip_prefix('>') {
        (Some(NpmOp::Gt), r)
    } else if let Some(r) = tok.strip_prefix('<') {
        (Some(NpmOp::Lt), r)
    } else if let Some(r) = tok.strip_prefix('=') {
        (Some(NpmOp::Eq), r)
    } else {
        (None, tok)
    };
    let p = parse_partial(rest)?;
    match op {
        Some(NpmOp::Ge) => Ok(vec![ge_partial(&p)]),
        Some(NpmOp::Le) => Ok(vec![le_partial(&p)]),
        Some(NpmOp::Gt) => Ok(vec![gt_partial(&p)]),
        Some(NpmOp::Lt) => Ok(vec![lt_partial(&p)]),
        Some(NpmOp::Eq) | None => Ok(eq_partial(&p)),
    }
}

// ---------- partial version + per-operator expansion ----------

#[derive(Debug, Clone)]
struct Partial {
    major: Option<u64>,
    minor: Option<u64>,
    patch: Option<u64>,
    pre: Option<String>,
}

impl Partial {
    fn is_fully_specified(&self) -> bool {
        self.major.is_some() && self.minor.is_some() && self.patch.is_some()
    }
    fn to_zero_filled_version(&self) -> NpmVersion {
        NpmVersion {
            major: self.major.unwrap_or(0),
            minor: self.minor.unwrap_or(0),
            patch: self.patch.unwrap_or(0),
            pre: self.pre.clone(),
        }
    }
    fn is_any(&self) -> bool {
        self.major.is_none()
    }
}

fn parse_partial(s: &str) -> Result<Partial, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(Partial { major: None, minor: None, patch: None, pre: None });
    }
    let s = s.strip_prefix('v').unwrap_or(s);

    // Build metadata after `+` is ignored per semver spec.
    let s = match s.split_once('+') {
        Some((v, _)) => v,
        None => s,
    };
    // Pre-release after `-`.
    let (core, pre) = match s.split_once('-') {
        Some((v, p)) => (v, Some(p.to_string())),
        None => (s, None),
    };

    let parts: Vec<&str> = core.split('.').collect();
    if parts.is_empty() || parts.len() > 3 {
        return Err(format!("invalid version `{}`: expected up to three dot-separated parts", core));
    }

    let parse_part = |p: &str| -> Result<Option<u64>, String> {
        if p == "*" || p == "x" || p == "X" || p.is_empty() {
            Ok(None)
        } else {
            p.parse::<u64>()
                .map(Some)
                .map_err(|_| format!("invalid version part `{}`", p))
        }
    };

    let major = parse_part(parts[0])?;
    let minor = if parts.len() >= 2 { parse_part(parts[1])? } else { None };
    let patch = if parts.len() >= 3 { parse_part(parts[2])? } else { None };

    Ok(Partial { major, minor, patch, pre })
}

fn upper_bound_partial(p: &Partial) -> NpmVersion {
    // Upper-exclusive bound for a partial version.
    //   `M`     → `(M+1).0.0`
    //   `M.m`   → `M.(m+1).0`
    //   `M.m.p` → `M.m.p` (caller should use Le instead, but emit value for completeness)
    match (p.major, p.minor, p.patch) {
        (Some(big_m), None, _) => NpmVersion { major: big_m + 1, minor: 0, patch: 0, pre: None },
        (Some(big_m), Some(m), None) => NpmVersion { major: big_m, minor: m + 1, patch: 0, pre: None },
        (Some(big_m), Some(m), Some(p_)) => NpmVersion { major: big_m, minor: m, patch: p_, pre: None },
        _ => NpmVersion { major: 0, minor: 0, patch: 0, pre: None },
    }
}

fn ge_partial(p: &Partial) -> NpmConstraint {
    NpmConstraint { op: NpmOp::Ge, version: p.to_zero_filled_version() }
}

fn le_partial(p: &Partial) -> NpmConstraint {
    // <=1.2 is treated as <1.3.0 (upper-exclusive bound of `1.2`)
    if p.is_fully_specified() {
        NpmConstraint { op: NpmOp::Le, version: p.to_zero_filled_version() }
    } else {
        NpmConstraint { op: NpmOp::Lt, version: upper_bound_partial(p) }
    }
}

fn gt_partial(p: &Partial) -> NpmConstraint {
    // >1.2 → >=1.3.0; >1 → >=2.0.0; >1.2.3 → >1.2.3
    if p.is_fully_specified() {
        NpmConstraint { op: NpmOp::Gt, version: p.to_zero_filled_version() }
    } else {
        NpmConstraint { op: NpmOp::Ge, version: upper_bound_partial(p) }
    }
}

fn lt_partial(p: &Partial) -> NpmConstraint {
    // <1 → <1.0.0; <1.2 → <1.2.0; <1.2.3 → <1.2.3
    NpmConstraint { op: NpmOp::Lt, version: p.to_zero_filled_version() }
}

fn eq_partial(p: &Partial) -> Vec<NpmConstraint> {
    if p.is_any() {
        return vec![];
    }
    if p.is_fully_specified() {
        return vec![NpmConstraint { op: NpmOp::Eq, version: p.to_zero_filled_version() }];
    }
    // Partial expands into a range [>=lower, <upper)
    vec![
        ge_partial(p),
        NpmConstraint { op: NpmOp::Lt, version: upper_bound_partial(p) },
    ]
}

fn caret_expand(p: &Partial) -> Vec<NpmConstraint> {
    if p.is_any() {
        return vec![];
    }
    let lower = p.to_zero_filled_version();

    // Pivot = leftmost concrete non-zero element; if none, rightmost concrete element.
    let parts: [Option<u64>; 3] = [p.major, p.minor, p.patch];
    let pivot = match parts.iter().position(|x| matches!(x, Some(n) if *n != 0)) {
        Some(i) => i,
        None => {
            if p.patch.is_some() { 2 }
            else if p.minor.is_some() { 1 }
            else { 0 }
        }
    };
    let upper = match pivot {
        0 => NpmVersion { major: lower.major + 1, minor: 0, patch: 0, pre: None },
        1 => NpmVersion { major: lower.major, minor: lower.minor + 1, patch: 0, pre: None },
        _ => NpmVersion { major: lower.major, minor: lower.minor, patch: lower.patch + 1, pre: None },
    };
    vec![
        NpmConstraint { op: NpmOp::Ge, version: lower },
        NpmConstraint { op: NpmOp::Lt, version: upper },
    ]
}

fn tilde_expand(p: &Partial) -> Vec<NpmConstraint> {
    if p.is_any() {
        return vec![];
    }
    let lower = p.to_zero_filled_version();
    let upper = if p.minor.is_some() {
        NpmVersion { major: lower.major, minor: lower.minor + 1, patch: 0, pre: None }
    } else {
        NpmVersion { major: lower.major + 1, minor: 0, patch: 0, pre: None }
    };
    vec![
        NpmConstraint { op: NpmOp::Ge, version: lower },
        NpmConstraint { op: NpmOp::Lt, version: upper },
    ]
}

// ---------- npm range → cabal constraint string ----------

pub fn to_cabal_constraint(req: &NpmRange) -> String {
    let parts: Vec<String> = req.0.iter().map(clause_to_cabal).collect();
    let non_empty: Vec<&String> = parts.iter().filter(|s| !s.is_empty()).collect();
    if non_empty.is_empty() {
        return String::new();
    }
    if non_empty.len() == 1 {
        return non_empty[0].clone();
    }
    non_empty
        .iter()
        .map(|s| format!("({})", s))
        .collect::<Vec<_>>()
        .join(" || ")
}

fn clause_to_cabal(c: &NpmClause) -> String {
    let mut buf = String::new();
    let mut first = true;
    for cs in &c.0 {
        if !first {
            buf.push_str(" && ");
        }
        first = false;
        write!(buf, "{}{}", cs.op.cabal_str(), cs.version).unwrap();
    }
    buf
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(major: u64, minor: u64, patch: u64) -> NpmVersion {
        NpmVersion { major, minor, patch, pre: None }
    }
    fn vp(major: u64, minor: u64, patch: u64, pre: &str) -> NpmVersion {
        NpmVersion { major, minor, patch, pre: Some(pre.to_string()) }
    }
    fn ge(version: NpmVersion) -> NpmConstraint { NpmConstraint { op: NpmOp::Ge, version } }
    fn lt(version: NpmVersion) -> NpmConstraint { NpmConstraint { op: NpmOp::Lt, version } }
    fn le(version: NpmVersion) -> NpmConstraint { NpmConstraint { op: NpmOp::Le, version } }
    fn eq(version: NpmVersion) -> NpmConstraint { NpmConstraint { op: NpmOp::Eq, version } }

    fn parse_ok(value: &str) -> NpmRange {
        parse_npm_range(value).unwrap_or_else(|e| panic!("parse failed for `{}`: {}", value, e))
    }

    fn cabal_of(value: &str) -> String {
        to_cabal_constraint(&parse_ok(value))
    }

    // ===== §6.1 Parser tests — protocol routing =====

    #[test]
    fn parse_bare_caret() {
        let (name, decl) = parse("aeson", "^2.1.0").unwrap();
        assert_eq!(name, "aeson");
        assert!(matches!(decl, DependencyDecl::Bare { .. }));
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(2, 1, 0)), lt(v(3, 0, 0))]);
        }
    }

    #[test]
    fn parse_bare_tilde() {
        let (_, decl) = parse("text", "~1.2.0").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(1, 2, 0)), lt(v(1, 3, 0))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_exact() {
        let (_, decl) = parse("uuid", "1.3.15").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![eq(v(1, 3, 15))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_wildcard() {
        let (_, decl) = parse("x", "*").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert!(req.0[0].0.is_empty(), "wildcard should produce empty constraint list");
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_latest() {
        let (_, decl) = parse("x", "latest").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert!(req.0[0].0.is_empty());
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_x_range() {
        let (_, decl) = parse("x", "1.x").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(1, 0, 0)), lt(v(2, 0, 0))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_hyphen_range() {
        let (_, decl) = parse("x", "1.0.0 - 2.0.0").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(1, 0, 0)), le(v(2, 0, 0))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_or_range() {
        let (_, decl) = parse("x", "<2 || >=3").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0.len(), 2);
            assert_eq!(req.0[0].0, vec![lt(v(2, 0, 0))]);
            assert_eq!(req.0[1].0, vec![ge(v(3, 0, 0))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_bare_and_range() {
        let (_, decl) = parse("x", ">=1.0.0 <2.0.0").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(1, 0, 0)), lt(v(2, 0, 0))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_hackage_caret() {
        let (name, decl) = parse("hackage:relude", "^1.0.0").unwrap();
        assert_eq!(name, "relude");
        if let DependencyDecl::Hackage { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(1, 0, 0)), lt(v(2, 0, 0))]);
        } else { panic!("expected Hackage"); }
    }

    #[test]
    fn parse_hackage_empty() {
        let (name, decl) = parse("hackage:base", "").unwrap();
        assert_eq!(name, "base");
        if let DependencyDecl::Hackage { req } = decl {
            assert!(req.0[0].0.is_empty());
        } else { panic!("expected Hackage"); }
    }

    #[test]
    fn parse_git_with_ref() {
        let (_, decl) = parse("lib", "git:github.com/me/lib.git#v1.2.3").unwrap();
        assert_eq!(
            decl,
            DependencyDecl::Git {
                url: "github.com/me/lib.git".to_string(),
                git_ref: Some("v1.2.3".to_string()),
            }
        );
    }

    #[test]
    fn parse_git_without_ref() {
        let (_, decl) = parse("lib", "git:gitlab.com/x/y.git").unwrap();
        assert_eq!(
            decl,
            DependencyDecl::Git { url: "gitlab.com/x/y.git".to_string(), git_ref: None }
        );
    }

    #[test]
    fn parse_git_https_url() {
        let (_, decl) = parse("lib", "git:https://example.com/repo.git#main").unwrap();
        assert_eq!(
            decl,
            DependencyDecl::Git {
                url: "https://example.com/repo.git".to_string(),
                git_ref: Some("main".to_string()),
            }
        );
    }

    #[test]
    fn parse_git_ssh_url() {
        let (_, decl) = parse("lib", "git:git@github.com:user/repo.git").unwrap();
        // SSH URLs contain `:` so the bare-protocol marker check would trigger…
        // but we strip `git:` first, so the remaining `git@github.com:user/repo.git` is the URL.
        assert_eq!(
            decl,
            DependencyDecl::Git {
                url: "git@github.com:user/repo.git".to_string(),
                git_ref: None,
            }
        );
    }

    #[test]
    fn parse_github_full() {
        let (_, decl) = parse("lib", "github:owner/repo#v1").unwrap();
        assert_eq!(
            decl,
            DependencyDecl::GitHub {
                owner_repo: "owner/repo".to_string(),
                git_ref: Some("v1".to_string()),
            }
        );
    }

    #[test]
    fn parse_github_no_ref() {
        let (_, decl) = parse("lib", "github:owner/repo").unwrap();
        assert_eq!(
            decl,
            DependencyDecl::GitHub { owner_repo: "owner/repo".to_string(), git_ref: None }
        );
    }

    #[test]
    fn parse_file_relative() {
        let (_, decl) = parse("lib", "file:../sibling").unwrap();
        assert_eq!(decl, DependencyDecl::File { path: "../sibling".to_string() });
    }

    #[test]
    fn parse_file_absolute() {
        let (_, decl) = parse("lib", "file:/abs/path").unwrap();
        assert_eq!(decl, DependencyDecl::File { path: "/abs/path".to_string() });
    }

    // ===== §6.1 Parser tests — edge cases =====

    #[test]
    fn parse_value_whitespace_trimmed() {
        let (_, decl) = parse("aeson", "  ^2.1.0  ").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![ge(v(2, 1, 0)), lt(v(3, 0, 0))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_value_with_newlines() {
        parse("aeson", "\n^2.1.0\n").unwrap();
    }

    #[test]
    fn parse_hackage_prefix_case_sensitive() {
        // `HACKAGE:` (uppercase) is not stripped — the whole key is treated as a bare
        // package name. We document case-sensitivity: only lowercase `hackage:` routes
        // to the Hackage protocol.
        let (name, decl) = parse("HACKAGE:foo", "^1").unwrap();
        assert_eq!(name, "HACKAGE:foo");
        assert!(matches!(decl, DependencyDecl::Bare { .. }));
    }

    #[test]
    fn parse_unknown_protocol() {
        let err = parse("lib", "npm:foo").unwrap_err();
        assert!(err.to_string().contains("unknown protocol"));
    }

    #[test]
    fn parse_hackage_empty_pkg_name() {
        let err = parse("hackage:", "^1").unwrap_err();
        assert!(err.to_string().contains("empty package name"));
    }

    #[test]
    fn parse_github_missing_repo() {
        let err = parse("lib", "github:owner").unwrap_err();
        assert!(err.to_string().contains("owner/repo"));
    }

    #[test]
    fn parse_github_too_many_slashes() {
        let err = parse("lib", "github:owner/repo/sub").unwrap_err();
        assert!(err.to_string().contains("owner/repo"));
    }

    #[test]
    fn parse_git_no_url() {
        let err = parse("lib", "git:").unwrap_err();
        assert!(err.to_string().contains("empty URL"));
    }

    #[test]
    fn parse_git_ref_with_slash() {
        let (_, decl) = parse("lib", "git:host/r.git#refs/tags/v1").unwrap();
        if let DependencyDecl::Git { url, git_ref } = decl {
            assert_eq!(url, "host/r.git");
            assert_eq!(git_ref.as_deref(), Some("refs/tags/v1"));
        } else { panic!("expected Git"); }
    }

    #[test]
    fn parse_file_empty_path() {
        let err = parse("lib", "file:").unwrap_err();
        assert!(err.to_string().contains("empty path"));
    }

    #[test]
    fn parse_conflicting_key_value() {
        let err = parse("hackage:foo", "git:repo.git").unwrap_err();
        assert!(err.to_string().contains("both key and value"));
    }

    #[test]
    fn parse_neo_excluded() {
        // Parser does NOT skip; the resolver is responsible for filtering "neo".
        let (name, _) = parse("neo", "*").unwrap();
        assert_eq!(name, "neo");
    }

    #[test]
    fn parse_invalid_semver_bare() {
        let err = parse("aeson", "not-a-version").unwrap_err();
        let msg = format!("{:?}", err);
        assert!(msg.contains("invalid"), "got: {}", msg);
    }

    #[test]
    fn parse_invalid_semver_hackage() {
        let err = parse("hackage:foo", "1.2.3.4").unwrap_err();
        let msg = format!("{:?}", err);
        assert!(msg.contains("expected up to three"), "got: {}", msg);
    }

    #[test]
    fn parse_unicode_pkg_name() {
        let (name, _) = parse("náme-é", "*").unwrap();
        assert_eq!(name, "náme-é");
    }

    #[test]
    fn parse_pre_release_version() {
        let (_, decl) = parse("foo", "1.0.0-beta.1").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![eq(vp(1, 0, 0, "beta.1"))]);
        } else { panic!("expected Bare"); }
    }

    #[test]
    fn parse_build_metadata_stripped() {
        let (_, decl) = parse("foo", "1.0.0+sha.abc").unwrap();
        if let DependencyDecl::Bare { req } = decl {
            assert_eq!(req.0[0].0, vec![eq(v(1, 0, 0))]);
        } else { panic!("expected Bare"); }
    }

    // ===== §6.2 npm → cabal translation tests =====

    #[test]
    fn translate_caret_normal() {
        assert_eq!(cabal_of("^1.2.3"), ">=1.2.3 && <2.0.0");
    }

    #[test]
    fn translate_caret_zero_minor() {
        assert_eq!(cabal_of("^0.2.3"), ">=0.2.3 && <0.3.0");
    }

    #[test]
    fn translate_caret_zero_zero_patch() {
        // ^0.0.5 → >=0.0.5 <0.0.6 (npm rule, NOT exact)
        assert_eq!(cabal_of("^0.0.5"), ">=0.0.5 && <0.0.6");
    }

    #[test]
    fn translate_caret_zero_zero_zero() {
        assert_eq!(cabal_of("^0.0.0"), ">=0.0.0 && <0.0.1");
    }

    #[test]
    fn translate_caret_partial_minor() {
        assert_eq!(cabal_of("^1.2"), ">=1.2.0 && <2.0.0");
    }

    #[test]
    fn translate_caret_partial_major() {
        assert_eq!(cabal_of("^1"), ">=1.0.0 && <2.0.0");
    }

    #[test]
    fn translate_caret_zero_partial() {
        assert_eq!(cabal_of("^0"), ">=0.0.0 && <1.0.0");
        assert_eq!(cabal_of("^0.0"), ">=0.0.0 && <0.1.0");
    }

    #[test]
    fn translate_tilde_full() {
        assert_eq!(cabal_of("~1.2.3"), ">=1.2.3 && <1.3.0");
    }

    #[test]
    fn translate_tilde_partial_major() {
        assert_eq!(cabal_of("~1"), ">=1.0.0 && <2.0.0");
    }

    #[test]
    fn translate_tilde_partial_minor() {
        assert_eq!(cabal_of("~1.2"), ">=1.2.0 && <1.3.0");
    }

    #[test]
    fn translate_exact() {
        assert_eq!(cabal_of("1.2.3"), "==1.2.3");
    }

    #[test]
    fn translate_wildcard_empty() {
        assert_eq!(cabal_of("*"), "");
        assert_eq!(cabal_of("x"), "");
        assert_eq!(cabal_of("latest"), "");
        assert_eq!(cabal_of(""), "");
    }

    #[test]
    fn translate_x_range_major() {
        assert_eq!(cabal_of("1.x"), ">=1.0.0 && <2.0.0");
        assert_eq!(cabal_of("1.X"), ">=1.0.0 && <2.0.0");
    }

    #[test]
    fn translate_x_range_minor() {
        assert_eq!(cabal_of("1.2.x"), ">=1.2.0 && <1.3.0");
    }

    #[test]
    fn translate_hyphen_range_full() {
        assert_eq!(cabal_of("1.0.0 - 2.0.0"), ">=1.0.0 && <=2.0.0");
    }

    #[test]
    fn translate_hyphen_range_partial_upper() {
        // `1.2.3 - 2.3` → >=1.2.3 <2.4.0
        assert_eq!(cabal_of("1.2.3 - 2.3"), ">=1.2.3 && <2.4.0");
    }

    #[test]
    fn translate_hyphen_range_partial_lower() {
        // `1.2 - 2.3.4` → >=1.2.0 <=2.3.4
        assert_eq!(cabal_of("1.2 - 2.3.4"), ">=1.2.0 && <=2.3.4");
    }

    #[test]
    fn translate_or_range_parenthesized() {
        // >=1 <2 || >=3 → (>=1.0.0 && <2.0.0) || >=3.0.0
        assert_eq!(cabal_of(">=1 <2 || >=3"), "(>=1.0.0 && <2.0.0) || (>=3.0.0)");
    }

    #[test]
    fn translate_or_no_parens_when_single_clause() {
        assert_eq!(cabal_of(">=1.0.0"), ">=1.0.0");
    }

    #[test]
    fn translate_or_three_clauses() {
        let out = cabal_of("<1 || 2 || >=3");
        assert!(out.contains("(<1.0.0)"), "got: {}", out);
        assert!(out.contains("(==2.0.0 && <3.0.0)") || out.contains("(>=2.0.0 && <3.0.0)"), "got: {}", out);
        assert!(out.contains("(>=3.0.0)"), "got: {}", out);
    }

    #[test]
    fn translate_and_range() {
        assert_eq!(cabal_of(">=1.0.0 <2.0.0"), ">=1.0.0 && <2.0.0");
    }

    #[test]
    fn translate_caret_x_y_z_inclusive() {
        // Boundary check: ^1.2.3 includes 1.2.3 (>=) but excludes 2.0.0 (<).
        // Reflected in the rendered string.
        assert_eq!(cabal_of("^1.2.3"), ">=1.2.3 && <2.0.0");
    }

    #[test]
    fn translate_pre_release_passthrough() {
        assert_eq!(cabal_of("1.0.0-beta.1"), "==1.0.0-beta.1");
    }

    #[test]
    fn translate_partial_two_part() {
        assert_eq!(cabal_of("1.2"), ">=1.2.0 && <1.3.0");
    }

    #[test]
    fn translate_partial_one_part() {
        assert_eq!(cabal_of("1"), ">=1.0.0 && <2.0.0");
    }

    #[test]
    fn translate_complex_mixed() {
        // ~1.2 || ^3.0.0 → (>=1.2.0 && <1.3.0) || (>=3.0.0 && <4.0.0)
        let out = cabal_of("~1.2 || ^3.0.0");
        assert_eq!(out, "(>=1.2.0 && <1.3.0) || (>=3.0.0 && <4.0.0)");
    }

    #[test]
    fn translate_greater_than_partial() {
        // `>1` → `>=2.0.0`
        assert_eq!(cabal_of(">1"), ">=2.0.0");
        // `>1.2` → `>=1.3.0`
        assert_eq!(cabal_of(">1.2"), ">=1.3.0");
    }

    #[test]
    fn translate_less_than_or_equal_partial() {
        // `<=1.2` → `<1.3.0`
        assert_eq!(cabal_of("<=1.2"), "<1.3.0");
        // `<=1` → `<2.0.0`
        assert_eq!(cabal_of("<=1"), "<2.0.0");
        // `<=1.2.3` → `<=1.2.3`
        assert_eq!(cabal_of("<=1.2.3"), "<=1.2.3");
    }
}
