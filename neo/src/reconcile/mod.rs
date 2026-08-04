use minijinja::Environment;
use crate::config::NeoConfig;
use crate::errors::NeoError;

pub mod cabal;
pub mod cabal_project;
pub mod dep_spec;
pub mod flake;
pub mod modules;
pub mod resolve;
pub mod test_suite;

use std::path::Path;

pub async fn run<P: AsRef<Path>>(project_dir: P, config: &NeoConfig) -> miette::Result<()> {
    let project_dir = project_dir.as_ref();
    let mut env = Environment::new();

    env.add_template(
        "project.cabal",
        include_str!("../../assets/templates/project.cabal.j2"),
    )
    .map_err(|e| NeoError::TemplateError { template: "project.cabal".to_string(), reason: e.to_string() })?;
    env.add_template(
        "flake.nix",
        include_str!("../../assets/templates/flake.nix.j2"),
    )
    .map_err(|e| NeoError::TemplateError { template: "flake.nix".to_string(), reason: e.to_string() })?;
    env.add_template(
        "cabal.project",
        include_str!("../../assets/templates/cabal.project.j2"),
    )
    .map_err(|e| NeoError::TemplateError { template: "cabal.project".to_string(), reason: e.to_string() })?;

    let resolved = resolve::resolve(config).await?;
    let modules = modules::discover(project_dir.join("src"));

    // Testing is built in by default: every project has a `test-suite` so `neo test`
    // is always available. Guarantee the hspec-discover driver (`tests/Spec.hs`) so
    // `main-is: Spec.hs` always resolves, then list every sibling spec as other-modules.
    test_suite::ensure_driver(project_dir)?;
    let test_modules = test_suite::other_modules(project_dir);

    cabal::generate(project_dir, &env, &resolved, &modules, &test_modules)?;
    flake::generate(project_dir, &env, &resolved)?;
    cabal_project::generate(project_dir, &env, &resolved)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;
    use crate::config::NeoConfig;

    fn fixture_config() -> NeoConfig {
        NeoConfig {
            name: "my-project".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            description: Some("A test project".to_string()),
            author: Some("Neo".to_string()),
            license: "Apache-2.0".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: [
                ("hackage:text".to_string(), "^2.0.0".to_string()),
                ("my-git-lib".to_string(), "git:host/g.git#main".to_string()),
                ("aeson".to_string(), "^2.1.0".to_string()),
            ]
            .into_iter()
            .collect(),
            source_path: None,
            source_content: None,
        }
    }

    #[tokio::test]
    async fn test_reconcile_full() {
        let dir = tempdir().unwrap();
        let project_dir = dir.path();

        fs::create_dir_all(project_dir.join("src")).unwrap();
        fs::write(project_dir.join("src/Main.hs"), "").unwrap();
        fs::write(project_dir.join("src/Lib.hs"), "").unwrap();

        let config = fixture_config();

        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        run(project_dir, &config).await.unwrap();

        assert!(project_dir.join("my-project.cabal").exists());
        assert!(project_dir.join("flake.nix").exists());
        assert!(project_dir.join("cabal.project").exists());

        let cabal_content = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        assert!(cabal_content.contains("name: my-project"));
        assert!(cabal_content.contains("Lib"));
        // hackage:text with ^2.0.0 → cabal: text >=2.0.0 && <3.0.0
        assert!(
            cabal_content.contains("text >=2.0.0 && <3.0.0"),
            "expected translated hackage constraint, got: {}", cabal_content,
        );

        let cabal_project = fs::read_to_string(project_dir.join("cabal.project")).unwrap();
        // git: dep should appear as a source-repository-package stanza
        assert!(cabal_project.contains("location: https://host/g.git"), "got: {}", cabal_project);
        assert!(cabal_project.contains("tag: main"));
        // Bare neopackages dep (NEO_SKIP_NETWORK → stub) also routes to git stanza
        assert!(cabal_project.contains("https://example.invalid/aeson.git"));

        let flake_content = fs::read_to_string(project_dir.join("flake.nix")).unwrap();
        assert!(flake_content.contains("description = \"A test project\""));
    }

    #[tokio::test]
    async fn test_reconcile_emits_test_suite_and_ensures_driver() {
        let dir = tempdir().unwrap();
        let project_dir = dir.path();

        fs::create_dir_all(project_dir.join("src")).unwrap();
        fs::write(project_dir.join("src/App.hs"), "").unwrap();
        // A `tests/` with a spec but NO driver yet — reconcile must write the driver.
        fs::create_dir_all(project_dir.join("tests")).unwrap();
        fs::write(project_dir.join("tests/ExampleSpec.hs"), "").unwrap();

        let config = fixture_config();
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        run(project_dir, &config).await.unwrap();

        // hspec-discover driver was created.
        let driver = project_dir.join("tests/Spec.hs");
        assert!(driver.exists(), "reconcile must create the tests/Spec.hs driver");
        assert!(fs::read_to_string(&driver).unwrap().contains("hspec-discover"));

        // Generated cabal carries the test-suite with the discovered spec.
        let cabal = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        assert!(cabal.contains("test-suite my-project-test"), "got:\n{}", cabal);
        assert!(cabal.contains("main-is: Spec.hs"), "got:\n{}", cabal);
        assert!(cabal.contains("ExampleSpec"), "got:\n{}", cabal);
    }

    #[tokio::test]
    async fn test_reconcile_always_provides_test_suite_even_without_test_dir() {
        // Testing is built in by default: a project with no `tests/` at all still gets
        // the driver, the test-suite stanza, and `tests: True` — so `neo test` works.
        let dir = tempdir().unwrap();
        let project_dir = dir.path();

        fs::create_dir_all(project_dir.join("src")).unwrap();
        fs::write(project_dir.join("src/App.hs"), "").unwrap();

        let config = fixture_config();
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        run(project_dir, &config).await.unwrap();

        assert!(
            project_dir.join("tests/Spec.hs").exists(),
            "reconcile must create the hspec-discover driver even without a pre-existing tests/"
        );
        let cabal = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        assert!(cabal.contains("test-suite my-project-test"), "got:\n{}", cabal);
        let cabal_project = fs::read_to_string(project_dir.join("cabal.project")).unwrap();
        assert!(
            cabal_project.contains("package my-project") && cabal_project.contains("tests: True"),
            "cabal.project must enable tests:\n{}",
            cabal_project
        );
    }

    #[tokio::test]
    async fn test_reconcile_idempotent() {
        let dir = tempdir().unwrap();
        let project_dir = dir.path();

        fs::create_dir_all(project_dir.join("src")).unwrap();
        fs::write(project_dir.join("src/Main.hs"), "").unwrap();

        let config = fixture_config();
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }

        run(project_dir, &config).await.unwrap();
        let cabal1 = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        let proj1 = fs::read_to_string(project_dir.join("cabal.project")).unwrap();
        let flake1 = fs::read_to_string(project_dir.join("flake.nix")).unwrap();

        run(project_dir, &config).await.unwrap();
        let cabal2 = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        let proj2 = fs::read_to_string(project_dir.join("cabal.project")).unwrap();
        let flake2 = fs::read_to_string(project_dir.join("flake.nix")).unwrap();

        assert_eq!(cabal1, cabal2, "reconcile is not idempotent for .cabal");
        assert_eq!(proj1, proj2, "reconcile is not idempotent for cabal.project");
        assert_eq!(flake1, flake2, "reconcile is not idempotent for flake.nix");
    }
}
