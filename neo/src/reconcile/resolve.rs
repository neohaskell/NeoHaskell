use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::config::{NeoConfig, ProjectKind};
use crate::errors::NeoError;
use crate::network::{NeoPackages, PackageTag};
use crate::reconcile::dep_spec::{self, DependencyDecl, NpmRange};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DependencySource {
    Hackage(String),
    Git { url: String, rev: String },
    File(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedDependency {
    pub name: String,
    pub source: DependencySource,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedConfig {
    pub name: String,
    pub version: String,
    pub neo_version: String,
    pub neo_sha: String,
    pub description: Option<String>,
    pub author: Option<String>,
    pub license: String,
    pub kind: ProjectKind,
    pub dependencies: Vec<ResolvedDependency>,
}

pub async fn resolve(config: &NeoConfig) -> miette::Result<ResolvedConfig> {
    let neo_sha = resolve_neo_sha(&config.neo_version).await?;

    // Parse every dep first so we can skip the registry fetch when no bare deps are present.
    let mut parsed: Vec<(String, DependencyDecl)> = Vec::new();
    let mut needs_registry = false;
    for (key, value) in &config.dependencies {
        let (name, decl) = dep_spec::parse(key, value)?;
        if name == "neo" {
            continue;
        }
        if matches!(decl, DependencyDecl::Bare { .. }) {
            needs_registry = true;
        }
        parsed.push((name, decl));
    }

    let registry = if needs_registry {
        Some(crate::network::fetch_package_registry().await?)
    } else {
        None
    };

    // Versions in NeoPackages live in upstream git tags, not in the registry
    // JSON. For every bare dep that the registry knows, fetch the tag list now
    // so the synchronous resolve pass has it on hand.
    let mut tags_by_name: HashMap<String, Vec<PackageTag>> = HashMap::new();
    if let Some(reg) = registry.as_ref() {
        for (name, decl) in &parsed {
            if !matches!(decl, DependencyDecl::Bare { .. }) {
                continue;
            }
            if let Some(meta) = reg.packages.get(name) {
                let tags = crate::network::fetch_package_tags(&meta.repository).await?;
                tags_by_name.insert(name.clone(), tags);
            }
        }
    }

    let mut deps = Vec::with_capacity(parsed.len());
    for (name, decl) in parsed {
        let source = resolve_decl(&name, &decl, registry.as_ref(), &tags_by_name, Some(config))?;
        deps.push(ResolvedDependency { name, source });
    }
    // Sort for deterministic generated artifacts.
    deps.sort_by(|a, b| a.name.cmp(&b.name));

    Ok(ResolvedConfig {
        name: config.name.clone(),
        version: config.version.clone(),
        neo_version: config.neo_version.clone(),
        neo_sha,
        description: config.description.clone(),
        author: config.author.clone(),
        license: config.license.clone(),
        kind: config.kind,
        dependencies: deps,
    })
}

async fn resolve_neo_sha(version: &str) -> miette::Result<String> {
    if std::env::var("NEO_SKIP_NETWORK").is_ok() {
        return Ok("deadbeef".to_string());
    }
    crate::network::fetch_neo_sha(version).await
}

pub(crate) fn resolve_decl(
    name: &str,
    decl: &DependencyDecl,
    registry: Option<&NeoPackages>,
    tags_by_name: &HashMap<String, Vec<PackageTag>>,
    config: Option<&NeoConfig>,
) -> miette::Result<DependencySource> {
    match decl {
        DependencyDecl::Hackage { req } => {
            Ok(DependencySource::Hackage(dep_spec::to_cabal_constraint(req)))
        }
        DependencyDecl::Git { url, git_ref } => Ok(DependencySource::Git {
            url: ensure_scheme(url),
            rev: git_ref.clone().unwrap_or_else(|| "main".to_string()),
        }),
        DependencyDecl::GitHub { owner_repo, git_ref } => Ok(DependencySource::Git {
            url: format!("https://github.com/{}.git", owner_repo),
            rev: git_ref.clone().unwrap_or_else(|| "main".to_string()),
        }),
        DependencyDecl::File { path } => Ok(DependencySource::File(path.clone())),
        DependencyDecl::Bare { req } => resolve_bare(name, req, registry, tags_by_name, config),
    }
}

/// Build the (src, span) pair to attach to an `InvalidDependency` error, if the
/// caller has a loaded `NeoConfig` with source info. Returns `(None, None)` for
/// programmatic / test fixtures with no source.
fn dep_source(
    config: Option<&NeoConfig>,
    name: &str,
) -> (Option<miette::NamedSource<String>>, Option<miette::SourceSpan>) {
    let Some(config) = config else { return (None, None) };
    let Some(content) = config.source_content.as_ref() else { return (None, None) };
    let path = config.source_path.as_deref().unwrap_or("neo.json");
    (
        Some(miette::NamedSource::new(path, content.clone())),
        crate::errors::key_span(content, name),
    )
}

fn resolve_bare(
    name: &str,
    req: &NpmRange,
    registry: Option<&NeoPackages>,
    tags_by_name: &HashMap<String, Vec<PackageTag>>,
    config: Option<&NeoConfig>,
) -> miette::Result<DependencySource> {
    let registry = match registry {
        Some(r) => r,
        None => return Err(unknown_package(name, &[], config)),
    };

    // NEO_SKIP_NETWORK returns an empty registry; emit a stub so existing offline
    // tests (and unit tests for the rest of the reconcile pipeline) keep working
    // without needing a real registry round-trip.
    if std::env::var("NEO_SKIP_NETWORK").is_ok() && registry.packages.is_empty() {
        return Ok(DependencySource::Git {
            url: format!("https://example.invalid/{}.git", name),
            rev: "stub".to_string(),
        });
    }

    let meta = registry.packages.get(name).ok_or_else(|| {
        unknown_package(name, &registry.packages.keys().cloned().collect::<Vec<_>>(), config)
    })?;

    let empty: Vec<PackageTag> = Vec::new();
    let tags = tags_by_name.get(name).unwrap_or(&empty);
    let picked = pick_best_version(name, req, &meta.repository, tags, config)?;

    Ok(DependencySource::Git { url: meta.repository.clone(), rev: picked.sha })
}

fn pick_best_version(
    name: &str,
    req: &NpmRange,
    repository: &str,
    tags: &[PackageTag],
    config: Option<&NeoConfig>,
) -> miette::Result<PackageTag> {
    use nodejs_semver::{Range, Version};

    let range_str = npm_range_string(req);
    let range: Range = range_str.parse().map_err(|e: nodejs_semver::SemverError| {
        let (src, span) = dep_source(config, name);
        NeoError::InvalidDependency {
            key: name.to_string(),
            value: range_str.clone(),
            reason: format!("internal: re-parsing npm range failed: {}", e),
            src,
            span,
        }
    })?;

    // Each tag is parsed as semver after stripping an optional leading `v` (the
    // most common upstream convention). Tags that aren't semver are skipped —
    // they cannot satisfy a semver constraint.
    let mut versions: Vec<(PackageTag, Version)> = tags
        .iter()
        .filter_map(|t| {
            let stripped = t.tag.strip_prefix('v').unwrap_or(&t.tag);
            stripped.parse::<Version>().ok().map(|v| (t.clone(), v))
        })
        .collect();
    versions.sort_by(|a, b| b.1.cmp(&a.1));

    for (tag, v) in &versions {
        if v.satisfies(&range) {
            return Ok(tag.clone());
        }
    }

    let available: Vec<String> = versions.into_iter().map(|(t, _)| t.tag).collect();
    let (src, span) = dep_source(config, name);
    let reason = if available.is_empty() {
        format!(
            "no semver tags found at `{}` (ran `git ls-remote --tags`). NeoPackages discovers versions from upstream git tags — push a tag like `0.1.0` (or `v0.1.0`) to that repo, or replace this `neo.json` entry with `\"git:{}#<ref>\"` to pin a branch/SHA directly",
            repository, repository,
        )
    } else {
        format!(
            "no upstream git tag of `{}` satisfies the constraint. Available tags at `{}`: [{}]. Either widen the constraint in `neo.json` (e.g. `\"^{}\"`), or pin a specific tag with `\"git:{}#<tag>\"`",
            name,
            repository,
            available.join(", "),
            available[0],
            repository,
        )
    };
    Err(NeoError::InvalidDependency {
        key: name.to_string(),
        value: range_str,
        reason,
        src,
        span,
    }
    .into())
}

fn npm_range_string(req: &NpmRange) -> String {
    let clauses: Vec<String> = req
        .0
        .iter()
        .map(|c| {
            let parts: Vec<String> = c
                .0
                .iter()
                .map(|cs| {
                    let op = match cs.op {
                        dep_spec::NpmOp::Ge => ">=",
                        dep_spec::NpmOp::Gt => ">",
                        dep_spec::NpmOp::Le => "<=",
                        dep_spec::NpmOp::Lt => "<",
                        dep_spec::NpmOp::Eq => "=",
                    };
                    format!("{}{}", op, cs.version)
                })
                .collect();
            if parts.is_empty() {
                "*".to_string()
            } else {
                parts.join(" ")
            }
        })
        .collect();
    if clauses.is_empty() {
        "*".to_string()
    } else {
        clauses.join(" || ")
    }
}

fn unknown_package(name: &str, available: &[String], config: Option<&NeoConfig>) -> miette::Report {
    let hint = if available.is_empty() {
        "use `hackage:<name>` for a Hackage package, or `git:<url>` for a git source".to_string()
    } else {
        format!(
            "available packages: [{}]. Use `hackage:<name>` for a Hackage package, or `git:<url>` for a git source",
            available.join(", "),
        )
    };
    let (src, span) = dep_source(config, name);
    NeoError::InvalidDependency {
        key: name.to_string(),
        value: String::new(),
        reason: format!(
            "package `{}` not found in the NeoPackages registry. Hint: {}",
            name, hint
        ),
        src,
        span,
    }
    .into()
}

fn ensure_scheme(url: &str) -> String {
    // Already a full URL, or an SSH-style ref?
    if url.contains("://") || url.starts_with("git@") {
        url.to_string()
    } else {
        format!("https://{}", url)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::network::{NeoPackageMetadata, NeoPackages, PackageTag};

    /// Build a (registry-entry, tag-list) fixture. Each tuple is `(tag, sha)`.
    fn make_pkg(repo: &str, tags: &[(&str, &str)]) -> (NeoPackageMetadata, Vec<PackageTag>) {
        let meta = NeoPackageMetadata {
            description: "test".to_string(),
            repository: repo.to_string(),
        };
        let tag_vec = tags
            .iter()
            .map(|(t, s)| PackageTag { tag: t.to_string(), sha: s.to_string() })
            .collect();
        (meta, tag_vec)
    }

    fn registry_with(packages: Vec<(&str, (NeoPackageMetadata, Vec<PackageTag>))>) -> (NeoPackages, HashMap<String, Vec<PackageTag>>) {
        let mut m = HashMap::new();
        let mut tags = HashMap::new();
        for (k, (meta, tag_vec)) in packages {
            m.insert(k.to_string(), meta);
            tags.insert(k.to_string(), tag_vec);
        }
        (NeoPackages { packages: m }, tags)
    }

    fn resolve_one(name: &str, value: &str, registry: Option<&NeoPackages>, tags: &HashMap<String, Vec<PackageTag>>) -> miette::Result<DependencySource> {
        let (n, decl) = dep_spec::parse(name, value)?;
        resolve_decl(&n, &decl, registry, tags, None)
    }

    fn no_tags() -> HashMap<String, Vec<PackageTag>> { HashMap::new() }

    // ===== happy paths =====

    #[test]
    fn resolve_bare_picks_highest_match() {
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "https://github.com/x/foo.git",
            &[("1.0.0", "sha1"), ("1.5.0", "sha15"), ("2.0.0", "sha20")],
        ))]);
        let src = resolve_one("foo", "^1.0.0", Some(&reg), &tags).unwrap();
        match src {
            DependencySource::Git { url, rev } => {
                assert_eq!(url, "https://github.com/x/foo.git");
                assert_eq!(rev, "sha15");
            }
            _ => panic!("expected Git source"),
        }
    }

    #[test]
    fn resolve_bare_accepts_v_prefixed_tags() {
        // Most upstream projects publish `v1.2.3`-style tags. The leading `v`
        // is stripped before semver parsing; the original tag is preserved on
        // the returned `PackageTag` (which feeds the cabal rev).
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "u", &[("v1.0.0", "sha1"), ("v1.5.0", "sha15"), ("v2.0.0", "sha20")],
        ))]);
        let src = resolve_one("foo", "^1.0.0", Some(&reg), &tags).unwrap();
        match src {
            DependencySource::Git { rev, .. } => assert_eq!(rev, "sha15"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_bare_exact_version() {
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "u", &[("1.5.0", "sha15")],
        ))]);
        let src = resolve_one("foo", "1.5.0", Some(&reg), &tags).unwrap();
        match src {
            DependencySource::Git { rev, .. } => assert_eq!(rev, "sha15"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_bare_latest() {
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "u", &[("1.0.0", "s1"), ("2.0.0", "s2")],
        ))]);
        let src = resolve_one("foo", "latest", Some(&reg), &tags).unwrap();
        match src {
            DependencySource::Git { rev, .. } => assert_eq!(rev, "s2"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_hackage_translates_caret() {
        let src = resolve_one("hackage:relude", "^1.0.0", None, &no_tags()).unwrap();
        match src {
            DependencySource::Hackage(s) => assert_eq!(s, ">=1.0.0 && <2.0.0"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_hackage_empty_constraint() {
        let src = resolve_one("hackage:base", "", None, &no_tags()).unwrap();
        match src {
            DependencySource::Hackage(s) => assert_eq!(s, ""),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_git_default_main() {
        let src = resolve_one("lib", "git:host/r.git", None, &no_tags()).unwrap();
        match src {
            DependencySource::Git { url, rev } => {
                assert_eq!(url, "https://host/r.git");
                assert_eq!(rev, "main");
            }
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_git_with_ref() {
        let src = resolve_one("lib", "git:host/r.git#v1", None, &no_tags()).unwrap();
        match src {
            DependencySource::Git { rev, .. } => assert_eq!(rev, "v1"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_git_preserves_scheme() {
        let src = resolve_one("lib", "git:https://example.com/r.git", None, &no_tags()).unwrap();
        match src {
            DependencySource::Git { url, .. } => assert_eq!(url, "https://example.com/r.git"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_git_preserves_ssh_url() {
        let src = resolve_one("lib", "git:git@github.com:o/r.git", None, &no_tags()).unwrap();
        match src {
            DependencySource::Git { url, .. } => assert_eq!(url, "git@github.com:o/r.git"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_github_shorthand() {
        let src = resolve_one("lib", "github:o/r#main", None, &no_tags()).unwrap();
        match src {
            DependencySource::Git { url, rev } => {
                assert_eq!(url, "https://github.com/o/r.git");
                assert_eq!(rev, "main");
            }
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_file_passthrough() {
        let src = resolve_one("lib", "file:../l", None, &no_tags()).unwrap();
        match src {
            DependencySource::File(p) => assert_eq!(p, "../l"),
            _ => panic!(),
        }
    }

    #[tokio::test]
    async fn resolve_excludes_neo() {
        let mut deps = HashMap::new();
        deps.insert("base".to_string(), "*".to_string());
        deps.insert("neo".to_string(), "latest".to_string());
        let config = NeoConfig {
            name: "p".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            description: None,
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: deps,
            source_path: None,
            source_content: None,
        };
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        let resolved = resolve(&config).await.unwrap();
        assert_eq!(resolved.dependencies.len(), 1);
        assert_eq!(resolved.dependencies[0].name, "base");
    }

    // ===== edge cases =====

    #[test]
    fn resolve_bare_no_registry_match() {
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "u", &[("2.0.0", "s2")],
        ))]);
        let err = resolve_one("foo", "^1.0.0", Some(&reg), &tags).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("no upstream git tag"), "got: {}", msg);
        assert!(msg.contains("2.0.0"), "got: {}", msg);
    }

    #[test]
    fn resolve_bare_no_semver_tags_at_all() {
        // Package is in the registry but its repo has no semver tags. The error
        // should tell the user exactly how to fix it: push a tag, or pin via git:.
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "https://github.com/x/foo.git", &[("not-a-version", "sx"), ("nightly", "sn")],
        ))]);
        let err = resolve_one("foo", "*", Some(&reg), &tags).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("no semver tags"), "got: {}", msg);
        assert!(msg.contains("git ls-remote --tags"), "got: {}", msg);
        assert!(msg.contains("https://github.com/x/foo.git"), "got: {}", msg);
    }

    #[test]
    fn resolve_bare_package_missing_from_registry() {
        let (reg, tags) = registry_with(vec![("bar", make_pkg("u", &[("1.0.0", "s")]))]);
        let err = resolve_one("foo", "^1.0.0", Some(&reg), &tags).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("not found"), "got: {}", msg);
        assert!(msg.contains("hackage:") || msg.contains("git:"), "got: {}", msg);
    }

    #[test]
    fn resolve_bare_empty_registry_under_skip_network() {
        let (reg, tags) = registry_with(vec![]);
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        let src = resolve_one("foo", "^1.0.0", Some(&reg), &tags).unwrap();
        match src {
            DependencySource::Git { rev, .. } => assert_eq!(rev, "stub"),
            _ => panic!("expected stub git source"),
        }
    }

    #[test]
    fn resolve_bare_registry_version_not_semver() {
        let (reg, tags) = registry_with(vec![("foo", make_pkg(
            "u",
            &[("weird", "sx"), ("1.0.0", "s1")],
        ))]);
        let src = resolve_one("foo", "*", Some(&reg), &tags).unwrap();
        match src {
            DependencySource::Git { rev, .. } => assert_eq!(rev, "s1"),
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_invalid_semver_input() {
        let err = resolve_one("foo", "not-a-version", None, &no_tags()).unwrap_err();
        assert!(err.to_string().contains("Invalid dependency"));
    }

    #[test]
    fn resolve_unknown_protocol() {
        let err = resolve_one("foo", "svn:abc", None, &no_tags()).unwrap_err();
        assert!(err.to_string().contains("unknown protocol"));
    }

    #[tokio::test]
    async fn resolve_neo_with_protocol_prefix_skipped() {
        let mut deps = HashMap::new();
        deps.insert("hackage:neo".to_string(), "*".to_string());
        let config = NeoConfig {
            name: "p".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            description: None, author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: deps,
            source_path: None,
            source_content: None,
        };
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        let resolved = resolve(&config).await.unwrap();
        // `name == "neo"` skip is applied after prefix strip, so `hackage:neo` is filtered too.
        assert_eq!(resolved.dependencies.len(), 0);
    }

    #[tokio::test]
    async fn resolve_determinism() {
        let mut deps = HashMap::new();
        deps.insert("hackage:c".to_string(), "*".to_string());
        deps.insert("hackage:a".to_string(), "*".to_string());
        deps.insert("hackage:b".to_string(), "*".to_string());
        let config = NeoConfig {
            name: "p".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            description: None, author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: deps,
            source_path: None,
            source_content: None,
        };
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        let r1 = resolve(&config).await.unwrap();
        let r2 = resolve(&config).await.unwrap();
        let names1: Vec<_> = r1.dependencies.iter().map(|d| &d.name).collect();
        let names2: Vec<_> = r2.dependencies.iter().map(|d| &d.name).collect();
        assert_eq!(names1, names2);
    }

    #[tokio::test]
    async fn resolve_output_sorted() {
        let mut deps = HashMap::new();
        deps.insert("hackage:zeta".to_string(), "*".to_string());
        deps.insert("hackage:alpha".to_string(), "*".to_string());
        deps.insert("hackage:mu".to_string(), "*".to_string());
        let config = NeoConfig {
            name: "p".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            description: None, author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: deps,
            source_path: None,
            source_content: None,
        };
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        let resolved = resolve(&config).await.unwrap();
        let names: Vec<_> = resolved.dependencies.iter().map(|d| d.name.clone()).collect();
        assert_eq!(names, vec!["alpha", "mu", "zeta"]);
    }
}
