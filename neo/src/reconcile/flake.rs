use minijinja::{context, Environment};
use std::fs;
use crate::reconcile::resolve::ResolvedConfig;
use crate::errors::NeoError;

use std::path::Path;

pub fn generate<P: AsRef<Path>>(
    project_dir: P,
    env: &Environment,
    config: &ResolvedConfig,
) -> miette::Result<()> {
    // Consumer-contract override: when NEO_NEOHASKELL_SOURCE points at a local
    // NeoHaskell checkout, the generated flake fetches its `neohaskell` source
    // from that path instead of the upstream git revision. This is the
    // deterministic redirect the generated-project consumer contract uses to
    // build against the exact monorepo checkout under test, and it survives
    // re-reconciliation (`neo build`/`test`/`run` regenerate flake.nix on every
    // invocation, so the override must be reproduced here, not patched in after).
    // Unset in normal use, so `neo new`/`neo build` behavior is unchanged.
    // Mirrors the existing NEO_SKIP_NETWORK test hook in resolve.rs.
    let neohaskell_source = local_source_override()?;

    let rendered = render(env, config, neohaskell_source.as_ref())?;

    let out_path = project_dir.as_ref().join("flake.nix");
    fs::write(&out_path, rendered).map_err(|e| NeoError::io_at("writing generated `flake.nix` at", &out_path, e))?;

    Ok(())
}

pub(super) struct LocalSourceOverride {
    pub(super) nix_url: String,
    pub(super) git_url: String,
    pub(super) rev: String,
}

/// Resolve the opt-in local checkout as both a Nix git input and a Cabal git
/// source. Both consumers use the same immutable HEAD revision. `git+file:` makes
/// Nix export a clean source tree instead of carrying a worktree's `.git` pointer.
pub(super) fn local_source_override() -> miette::Result<Option<LocalSourceOverride>> {
    let Some(raw) = std::env::var_os("NEO_NEOHASKELL_SOURCE").filter(|s| !s.is_empty()) else {
        return Ok(None);
    };
    local_source_override_for_path(Path::new(&raw)).map(Some)
}

fn local_source_override_for_path(path: &Path) -> miette::Result<LocalSourceOverride> {
    if !path.is_absolute() {
        return Err(miette::miette!(
            "NEO_NEOHASKELL_SOURCE must be an absolute path, got `{}`",
            path.display()
        ));
    }
    let canonical = path.canonicalize().map_err(|e| NeoError::io_at(
        "resolving NEO_NEOHASKELL_SOURCE at",
        &path.to_path_buf(),
        e,
    ))?;
    let text = canonical.to_str().ok_or_else(|| miette::miette!(
        "NEO_NEOHASKELL_SOURCE is not valid UTF-8: `{}`",
        canonical.display()
    ))?;
    let mut encoded = String::with_capacity(text.len());
    for byte in text.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' | b'/' => {
                encoded.push(byte as char);
            }
            _ => encoded.push_str(&format!("%{byte:02X}")),
        }
    }
    let output = std::process::Command::new("git")
        .args(["-C", text, "rev-parse", "HEAD"])
        .output()
        .map_err(|e| NeoError::io_at("running git in NEO_NEOHASKELL_SOURCE at", &canonical, e))?;
    if !output.status.success() {
        return Err(miette::miette!(
            "NEO_NEOHASKELL_SOURCE is not a readable Git checkout: `{}`",
            canonical.display()
        ));
    }
    let rev = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if rev.len() != 40 || !rev.bytes().all(|b| b.is_ascii_hexdigit()) {
        return Err(miette::miette!("Git returned an invalid HEAD revision for `{}`", canonical.display()));
    }
    let git_url = format!("file://{encoded}");
    let nix_url = format!("git+{git_url}?rev={rev}");
    Ok(LocalSourceOverride { nix_url, git_url, rev })
}

/// Render the flake template. Split from `generate` so the (env-driven) override
/// branch is unit-testable without mutating process-global environment.
fn render(
    env: &Environment,
    config: &ResolvedConfig,
    neohaskell_source: Option<&LocalSourceOverride>,
) -> miette::Result<String> {
    let template = env.get_template("flake.nix")
        .map_err(|e| NeoError::TemplateError { template: "flake.nix".to_string(), reason: e.to_string() })?;

    template.render(context! {
        name => config.name,
        description => config.description,
        neo_sha => config.neo_sha,
        neohaskell_source => neohaskell_source.map(|s| s.nix_url.as_str()),
        neohaskell_git_url => neohaskell_source.map(|s| s.git_url.as_str()).unwrap_or("https://github.com/neohaskell/neohaskell.git"),
        neohaskell_commit => neohaskell_source.map(|s| s.rev.as_str()).unwrap_or(config.neo_sha.as_str()),
    }).map_err(|e| NeoError::TemplateError { template: "flake.nix".to_string(), reason: e.to_string() }.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_generate_flake() {
        let dir = tempdir().unwrap();

        let mut env = Environment::new();
        env.add_template("flake.nix", "description: {{description}}, sha: {{neo_sha}}").unwrap();

        let config = ResolvedConfig {
            name: "test-project".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            neo_sha: "abc1234".to_string(),
            description: Some("A test description".to_string()),
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: vec![],
        };

        generate(dir.path(), &env, &config).unwrap();

        let content = fs::read_to_string(dir.path().join("flake.nix")).unwrap();
        assert!(content.contains("description: A test description"));
        assert!(content.contains("sha: abc1234"));
    }

    fn real_env() -> Environment<'static> {
        let mut env = Environment::new();
        env.add_template("flake.nix", include_str!("../../assets/templates/flake.nix.j2"))
            .unwrap();
        env
    }

    fn real_config() -> ResolvedConfig {
        ResolvedConfig {
            name: "acme".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            neo_sha: "cafef00d".to_string(),
            description: Some("Acme".to_string()),
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: vec![],
        }
    }

    #[test]
    fn render_without_override_uses_upstream_git_rev() {
        let out = render(&real_env(), &real_config(), None).unwrap();
        assert!(
            out.contains(r#"inputs.neohaskell.url = "git+https://github.com/neohaskell/neohaskell.git?rev=cafef00d";"#),
            "default generation must fetch the pinned upstream rev:\n{out}"
        );
        assert!(!out.contains("path:"), "no local path override when the env is unset:\n{out}");
        assert!(!out.contains("{%"), "no unrendered template syntax should remain:\n{out}");
        // The inputMap key + neohaskellCommit still key on the sha regardless.
        assert!(out.contains(r#"neohaskellCommit = "cafef00d";"#));
    }

    #[test]
    fn render_with_override_redirects_source_to_local_git() {
        let source = LocalSourceOverride {
            nix_url: "git+file:///abs/checkout?rev=0123456789012345678901234567890123456789".to_string(),
            git_url: "file:///abs/checkout".to_string(),
            rev: "0123456789012345678901234567890123456789".to_string(),
        };
        let out = render(&real_env(), &real_config(), Some(&source)).unwrap();
        assert!(
            out.contains(r#"inputs.neohaskell.url = "git+file:///abs/checkout?rev=0123456789012345678901234567890123456789";"#),
            "the override must redirect the neohaskell source to the local checkout:\n{out}"
        );
        assert!(
            !out.contains("git+https://github.com/neohaskell/neohaskell.git?rev="),
            "the override must replace (not duplicate) the upstream git url:\n{out}"
        );
        assert!(out.contains("inputs.neohaskell.flake = false;"), "flake=false must be preserved:\n{out}");
        assert!(out.contains(r#"neohaskellCommit = "0123456789012345678901234567890123456789";"#));
        assert!(out.contains(r#"file:///abs/checkout/${neohaskellCommit}"#));
        assert!(!out.contains("{%"), "no unrendered template syntax should remain:\n{out}");
    }

    #[test]
    fn render_ignores_empty_override() {
        // An empty NEO_NEOHASKELL_SOURCE is treated as unset (see `generate`).
        let out = render(&real_env(), &real_config(), None).unwrap();
        assert!(out.contains("git+https://github.com/neohaskell/neohaskell.git?rev=cafef00d"));
    }

    #[test]
    fn local_source_override_requires_git_checkout_and_encodes_unsafe_bytes() {
        assert!(local_source_override_for_path(Path::new("relative")).is_err());
        assert!(local_source_override_for_path(Path::new("/definitely/missing/neo-checkout")).is_err());

        let dir = tempfile::tempdir().unwrap();
        let checkout = dir.path().join("checkout with # marker");
        fs::create_dir(&checkout).unwrap();
        let git = |args: &[&str]| {
            let status = std::process::Command::new("git")
                .arg("-C").arg(&checkout).args(args).status().unwrap();
            assert!(status.success());
        };
        git(&["init", "-q"]);
        git(&["config", "user.email", "contract@example.invalid"]);
        git(&["config", "user.name", "Consumer Contract"]);
        git(&["commit", "--allow-empty", "-qm", "fixture"]);

        let source = local_source_override_for_path(&checkout).unwrap();
        assert!(source.nix_url.starts_with("git+file:///"));
        assert!(source.nix_url.contains("checkout%20with%20%23%20marker"));
        assert!(source.nix_url.ends_with(&format!("?rev={}", source.rev)));
        assert!(source.git_url.starts_with("file:///"));
        assert!(!source.nix_url.contains(' '));
        assert!(!source.nix_url.contains('#'));
    }
}
