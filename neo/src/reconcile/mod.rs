use minijinja::Environment;
use crate::config::NeoConfig;
use crate::errors::NeoError;
use fs2::FileExt;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::Path;

pub mod cabal;
pub mod cabal_project;
pub mod dep_spec;
pub mod flake;
pub mod guidance;
pub mod modules;
pub mod resolve;
pub mod source;
pub mod test_suite;

struct ProjectLock {
    _file: std::fs::File,
}

impl ProjectLock {
    async fn acquire(project_dir: &Path) -> miette::Result<Self> {
        let lock_dir = project_dir.join(".neo");
        fs::create_dir_all(&lock_dir)
            .map_err(|error| NeoError::io_at("creating the project state directory at", &lock_dir, error))?;
        let lock_path = lock_dir.join("project.lock");
        let file = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .open(&lock_path)
            .map_err(|error| NeoError::io_at("opening the project reconciliation lock at", &lock_path, error))?;
        let display_path = lock_path.clone();
        tokio::task::spawn_blocking(move || {
            FileExt::lock_exclusive(&file).map_err(|error| {
                NeoError::io_at("acquiring the project reconciliation lock at", &display_path, error)
            })?;
            Ok(Self { _file: file })
        })
        .await
        .map_err(|error| miette::miette!("waiting for the project reconciliation lock failed: {error}"))?
    }
}

fn atomic_write(path: &Path, content: &[u8]) -> std::io::Result<()> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent)?;
    let mut temp = tempfile::NamedTempFile::new_in(parent)?;
    temp.write_all(content)?;
    temp.as_file().sync_all()?;
    temp.persist(path).map_err(|error| error.error)?;
    if let Ok(directory) = std::fs::File::open(parent) {
        let _ = directory.sync_all();
    }
    Ok(())
}

pub async fn run_project<P: AsRef<Path>>(project_dir: P) -> miette::Result<()> {
    let project_dir = project_dir.as_ref();
    let _lock = ProjectLock::acquire(project_dir).await?;
    let config_path = project_dir.join("neo.json");
    let config = NeoConfig::load(&config_path)?;
    run_locked(project_dir, &config, false).await
}

pub async fn retry_guidance<P: AsRef<Path>>(project_dir: P) -> miette::Result<()> {
    let project_dir = project_dir.as_ref();
    let _lock = ProjectLock::acquire(project_dir).await?;
    let config_path = project_dir.join("neo.json");
    let config = NeoConfig::load(&config_path)?;
    run_locked(project_dir, &config, true).await
}

async fn run_locked(project_dir: &Path, config: &NeoConfig, explicit_retry: bool) -> miette::Result<()> {
    let source = source::FrameworkSource::resolve(&config.neo_version).await?;
    let guidance_plan = guidance::begin(project_dir, &source, explicit_retry)?;
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

    let resolved = resolve::resolve(config, source.clone()).await?;
    let modules = modules::discover(project_dir.join("src"));

    // Testing is built in by default: every project has a `test-suite` so `neo test`
    // is always available. Guarantee the hspec-discover driver (`tests/Spec.hs`) so
    // `main-is: Spec.hs` always resolves, then list every sibling spec as other-modules.
    test_suite::ensure_driver(project_dir)?;
    let test_modules = test_suite::other_modules(project_dir);

    cabal::generate(project_dir, &env, &resolved, &modules, &test_modules)?;
    flake::generate(project_dir, &env, &resolved)?;
    cabal_project::generate(project_dir, &env, &resolved)?;

    if let Some(warning) = guidance::finish(project_dir, &source, guidance_plan, explicit_retry).await {
        println!("[warn] {warning}");
        if explicit_retry {
            return Err(miette::miette!("framework guidance retrieval did not complete"));
        }
    }

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

    fn write_config(project_dir: &Path, config: &NeoConfig) {
        fs::write(project_dir.join("neo.json"), serde_json::to_vec(config).unwrap()).unwrap();
    }

    #[tokio::test]
    async fn test_reconcile_full() {
        let dir = tempdir().unwrap();
        let project_dir = dir.path();

        fs::create_dir_all(project_dir.join("src")).unwrap();
        fs::write(project_dir.join("src/Main.hs"), "").unwrap();
        fs::write(project_dir.join("src/Lib.hs"), "").unwrap();

        let mut config = fixture_config();
        config.neo_version = "0123456789012345678901234567890123456789".to_string();
        write_config(project_dir, &config);

        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        run_project(project_dir).await.unwrap();

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
        assert!(flake_content.contains(&config.neo_version));
        assert!(cabal_project.contains(&config.neo_version));
        let active = fs::read_to_string(project_dir.join(".neo/agent-guidance/active.md")).unwrap();
        assert!(active.contains("status: unavailable"));
        assert!(active.contains(&config.neo_version));
        assert!(active.contains("neo guidance retry"));
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
        write_config(project_dir, &config);
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        run_project(project_dir).await.unwrap();

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
        write_config(project_dir, &config);
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        run_project(project_dir).await.unwrap();

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
        write_config(project_dir, &config);
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }

        run_project(project_dir).await.unwrap();
        let cabal1 = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        let proj1 = fs::read_to_string(project_dir.join("cabal.project")).unwrap();
        let flake1 = fs::read_to_string(project_dir.join("flake.nix")).unwrap();

        run_project(project_dir).await.unwrap();
        let cabal2 = fs::read_to_string(project_dir.join("my-project.cabal")).unwrap();
        let proj2 = fs::read_to_string(project_dir.join("cabal.project")).unwrap();
        let flake2 = fs::read_to_string(project_dir.join("flake.nix")).unwrap();

        assert_eq!(cabal1, cabal2, "reconcile is not idempotent for .cabal");
        assert_eq!(proj1, proj2, "reconcile is not idempotent for cabal.project");
        assert_eq!(flake1, flake2, "reconcile is not idempotent for flake.nix");
    }

    #[tokio::test]
    async fn queued_reconcile_reads_config_after_acquiring_project_lock() {
        let dir = tempdir().unwrap();
        let project_dir = dir.path();
        fs::create_dir_all(project_dir.join("src")).unwrap();
        fs::write(project_dir.join("src/Main.hs"), "").unwrap();
        write_config(project_dir, &fixture_config());
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }

        let held_lock = ProjectLock::acquire(project_dir).await.unwrap();
        let requested_project = project_dir.to_path_buf();
        let waiting = tokio::spawn(async move { run_project(requested_project).await });
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;

        let mut latest = fixture_config();
        latest.name = "latest-project".to_string();
        write_config(project_dir, &latest);
        drop(held_lock);
        waiting.await.unwrap().unwrap();

        assert!(project_dir.join("latest-project.cabal").exists());
        assert!(!project_dir.join("my-project.cabal").exists());
    }
}
