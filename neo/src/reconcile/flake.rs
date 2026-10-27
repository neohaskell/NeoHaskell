use minijinja::{context, Environment};
use crate::reconcile::resolve::ResolvedConfig;
#[cfg(test)]
use crate::reconcile::source::{FrameworkOrigin, FrameworkSource, FullSha};
use crate::errors::NeoError;

use std::path::Path;

pub fn generate<P: AsRef<Path>>(
    project_dir: P,
    env: &Environment,
    config: &ResolvedConfig,
) -> miette::Result<()> {
    let rendered = render(env, config)?;

    let out_path = project_dir.as_ref().join("flake.nix");
    super::atomic_write(&out_path, rendered.as_bytes())
        .map_err(|e| NeoError::io_at("writing generated `flake.nix` at", &out_path, e))?;

    Ok(())
}

fn render(env: &Environment, config: &ResolvedConfig) -> miette::Result<String> {
    let template = env.get_template("flake.nix")
        .map_err(|e| NeoError::TemplateError { template: "flake.nix".to_string(), reason: e.to_string() })?;

    let source_url = config.framework_source.nix_source_url();
    let rendered = template.render(context! {
        name => config.name,
        description => config.description,
        neo_sha => config.framework_source.pin(),
        neohaskell_source => source_url.as_deref(),
        neohaskell_git_url => config.framework_source.git_url(),
        neohaskell_commit => config.framework_source.pin(),
    })
    .map_err(|error| NeoError::TemplateError {
        template: "flake.nix".to_string(),
        reason: error.to_string(),
    })?;
    Ok(rendered)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_generate_flake() {
        let dir = tempdir().unwrap();

        let mut env = Environment::new();
        env.add_template("flake.nix", "description: {{description}}, sha: {{neo_sha}}").unwrap();

        let config = ResolvedConfig {
            name: "test-project".to_string(),
            version: "0.1.0".to_string(),
            framework_source: FrameworkSource::OfflinePlaceholder,
            description: Some("A test description".to_string()),
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: vec![],
        };

        generate(dir.path(), &env, &config).unwrap();

        let content = fs::read_to_string(dir.path().join("flake.nix")).unwrap();
        assert!(content.contains("description: A test description"));
        assert!(content.contains("sha: deadbeef"));
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
            framework_source: FrameworkSource::Commit {
                sha: FullSha::parse("cafef00dcafef00dcafef00dcafef00dcafef00d").unwrap(),
                origin: FrameworkOrigin::Upstream,
            },
            description: Some("Acme".to_string()),
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: vec![],
        }
    }

    #[test]
    fn render_without_override_uses_upstream_git_rev() {
        let out = render(&real_env(), &real_config()).unwrap();
        assert!(
            out.contains(r#"inputs.neohaskell.url = "git+https://github.com/neohaskell/neohaskell.git?rev=cafef00dcafef00dcafef00dcafef00dcafef00d";"#),
            "default generation must fetch the pinned upstream rev:\n{out}"
        );
        assert!(!out.contains("path:"), "no local path override when the env is unset:\n{out}");
        assert!(!out.contains("{%"), "no unrendered template syntax should remain:\n{out}");
        // The inputMap key + neohaskellCommit still key on the sha regardless.
        assert!(out.contains(r#"neohaskellCommit = "cafef00dcafef00dcafef00dcafef00dcafef00d";"#));
    }

    #[test]
    fn render_with_override_redirects_source_to_local_git() {
        let mut config = real_config();
        config.framework_source = FrameworkSource::Commit {
            sha: FullSha::parse("0123456789012345678901234567890123456789").unwrap(),
            origin: FrameworkOrigin::LocalGit { root: Path::new("/abs/checkout").to_path_buf() },
        };
        let out = render(&real_env(), &config).unwrap();
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
}
