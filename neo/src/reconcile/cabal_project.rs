use minijinja::{context, Environment};
use std::fs;
use crate::reconcile::resolve::{ResolvedConfig, DependencySource};
use crate::errors::NeoError;

use std::path::Path;

pub fn generate<P: AsRef<Path>>(
    project_dir: P,
    env: &Environment,
    config: &ResolvedConfig,
) -> miette::Result<()> {
    let template = env.get_template("cabal.project")
        .map_err(|e| NeoError::TemplateError { template: "cabal.project".to_string(), reason: e.to_string() })?;

    let mut git_dependencies = Vec::new();
    let mut file_dependencies = Vec::new();

    for dep in &config.dependencies {
        match &dep.source {
            DependencySource::Git { url, rev } => {
                git_dependencies.push(context! { url => url, rev => rev });
            }
            DependencySource::File(path) => {
                file_dependencies.push(context! { path => path });
            }
            _ => {}
        }
    }

    let rendered = template.render(context! {
        git_dependencies => git_dependencies,
        file_dependencies => file_dependencies,
        neo_sha => config.neo_sha,
        name => config.name,
    }).map_err(|e| NeoError::TemplateError { template: "cabal.project".to_string(), reason: e.to_string() })?;

    let out_path = project_dir.as_ref().join("cabal.project");
    fs::write(&out_path, rendered).map_err(|e| NeoError::io_at("writing generated `cabal.project` at", &out_path, e))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    use crate::reconcile::resolve::{ResolvedConfig, ResolvedDependency, DependencySource};

    fn rc(deps: Vec<ResolvedDependency>) -> ResolvedConfig {
        ResolvedConfig {
            name: "p".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            neo_sha: "neo-sha".to_string(),
            description: None,
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: deps,
        }
    }

    fn stanza_env() -> Environment<'static> {
        // Mimics the real template structure enough to assert stanzas + packages lines.
        let mut env = Environment::new();
        env.add_template(
            "cabal.project",
            "packages:\n  *.cabal\nsha: {{ neo_sha }}\n{% for dep in git_dependencies %}source-repository-package\n    type: git\n    location: {{ dep.url }}\n    tag: {{ dep.rev }}\n{% endfor %}{% for dep in file_dependencies %}packages: {{ dep.path }}\n{% endfor %}package {{ name }}\n  tests: True\n",
        )
        .unwrap();
        env
    }

    #[test]
    fn cabal_project_emits_git_source_repo_package() {
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let config = rc(vec![ResolvedDependency {
            name: "g".to_string(),
            source: DependencySource::Git {
                url: "https://github.com/user/repo".to_string(),
                rev: "v1".to_string(),
            },
        }]);
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert!(content.contains("source-repository-package"));
        assert!(content.contains("location: https://github.com/user/repo"));
        assert!(content.contains("tag: v1"));
    }

    #[test]
    fn cabal_project_emits_file_packages() {
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let config = rc(vec![ResolvedDependency {
            name: "f".to_string(),
            source: DependencySource::File("../sibling".to_string()),
        }]);
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert!(content.contains("packages: ../sibling"), "got: {}", content);
    }

    #[test]
    fn cabal_project_emits_neopackages_as_git() {
        // NeoPackages-resolved deps reach this point as DependencySource::Git with the
        // registry's repo URL and the version's SHA — same template path as `git:` deps.
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let config = rc(vec![ResolvedDependency {
            name: "neopkg".to_string(),
            source: DependencySource::Git {
                url: "https://github.com/NeoHaskell/neopkg.git".to_string(),
                rev: "abc123def".to_string(),
            },
        }]);
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert!(content.contains("location: https://github.com/NeoHaskell/neopkg.git"));
        assert!(content.contains("tag: abc123def"));
    }

    #[test]
    fn cabal_project_omits_hackage_deps() {
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let config = rc(vec![
            ResolvedDependency {
                name: "hackage-only".to_string(),
                source: DependencySource::Hackage(">=1.0".to_string()),
            },
            ResolvedDependency {
                name: "git-only".to_string(),
                source: DependencySource::Git {
                    url: "u".to_string(),
                    rev: "r".to_string(),
                },
            },
        ]);
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert!(!content.contains("hackage-only"), "hackage dep leaked into cabal.project: {}", content);
        assert!(content.contains("location: u"));
    }

    #[test]
    fn cabal_project_multiple_git_stanzas() {
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let config = rc(vec![
            ResolvedDependency {
                name: "a".to_string(),
                source: DependencySource::Git { url: "url-a".to_string(), rev: "r-a".to_string() },
            },
            ResolvedDependency {
                name: "b".to_string(),
                source: DependencySource::Git { url: "url-b".to_string(), rev: "r-b".to_string() },
            },
            ResolvedDependency {
                name: "c".to_string(),
                source: DependencySource::Git { url: "url-c".to_string(), rev: "r-c".to_string() },
            },
        ]);
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert_eq!(content.matches("source-repository-package").count(), 3);
    }

    #[test]
    fn cabal_project_only_neo_pinned_when_no_deps() {
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let config = rc(vec![]);
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert!(content.contains("sha: neo-sha"));
        assert!(!content.contains("source-repository-package"));
        // Testing is built in by default → the per-package tests stanza is always present.
        assert!(content.contains("tests: True"), "missing tests stanza:\n{}", content);
    }

    #[test]
    fn cabal_project_always_enables_tests_for_app_package() {
        // `package <name>\n tests: True` makes the (haskell.nix) plan include the
        // app's test-suite; scoped to the app, not the source-repo-package deps.
        let dir = tempdir().unwrap();
        let env = stanza_env();
        let mut config = rc(vec![]);
        config.name = "my-app".to_string();
        generate(dir.path(), &env, &config).unwrap();
        let content = fs::read_to_string(dir.path().join("cabal.project")).unwrap();
        assert!(content.contains("package my-app"), "missing per-package stanza:\n{}", content);
        assert!(content.contains("tests: True"), "missing tests: True:\n{}", content);
    }
}
