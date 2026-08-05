use assert_cmd::Command;
use predicates::prelude::*;

fn neo_cmd() -> Command {
    Command::cargo_bin("neo").unwrap()
}

#[test]
fn test_version() {
    let mut cmd = neo_cmd();
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("neo 0.1.0"));
}

#[test]
fn bare_neo_prints_single_line_hint_no_mascot() {
    let mut cmd = neo_cmd();
    cmd.arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("The NeoHaskell CLI"))
        .stdout(predicate::str::contains("neo --help"))
        // Mascot art must not appear anywhere.
        .stdout(predicate::str::contains("╔═══╗").not())
        .stdout(predicate::str::contains("║ :)║").not())
        .stdout(predicate::str::contains("╚═══╝").not());
}

#[test]
fn test_help() {
    let mut cmd = neo_cmd();
    cmd.arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage: neo"));
}

#[test]
fn test_neo_new_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "test-project";
    
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);
    assert!(project_path.exists());
    assert!(project_path.join("neo.json").exists());
    assert!(project_path.join("src/App.hs").exists());
    assert!(project_path.join("launcher/Launcher.hs").exists());
    assert!(project_path.join(".envrc").exists());
    assert!(project_path.join(".git").exists());
    assert!(project_path.join(".git/hooks/pre-commit").exists());

    // A working Hspec test-suite is set up so `neo test` runs real Haskell tests.
    // Haskell specs live under `tests/` alongside `.hurl` scenarios; reconcile always
    // writes the hspec-discover driver `tests/Spec.hs`.
    assert!(
        project_path.join("tests/Spec.hs").exists(),
        "hspec-discover driver tests/Spec.hs should be present"
    );
    let cabal = std::fs::read_to_string(project_path.join(format!("{}.cabal", project_name))).unwrap();
    assert!(
        cabal.contains(&format!("test-suite {}-test", project_name)),
        "generated .cabal must declare a test-suite:\n{}",
        cabal
    );
    assert!(cabal.contains("main-is: Spec.hs"), "test-suite must use the hspec-discover driver:\n{}", cabal);
    assert!(cabal.contains("hs-source-dirs: tests"), "test-suite must source from tests/:\n{}", cabal);
    assert!(cabal.contains("QuickCheck"), "test-suite must bundle QuickCheck:\n{}", cabal);
    // cabal.project must enable the suite in the (haskell.nix) plan so it resolves offline.
    let cabal_project = std::fs::read_to_string(project_path.join("cabal.project")).unwrap();
    assert!(
        cabal_project.contains(&format!("package {}", project_name)) && cabal_project.contains("tests: True"),
        "cabal.project must enable tests for the project package:\n{}",
        cabal_project
    );

    // Verify neo.json content
    let config_content = std::fs::read_to_string(project_path.join("neo.json")).unwrap();
    assert!(config_content.contains(project_name));
    assert!(config_content.contains("\"neo-version\": \"main\""));

    // Verify git commit exists
    let git_log = std::process::Command::new("git")
        .args(["log", "--oneline"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    let log_stdout = String::from_utf8_lossy(&git_log.stdout);
    assert!(log_stdout.contains("Initial commit from NeoCLI"));
}

#[test]
fn test_neo_new_offline_scaffolds_full_starter() {
    // Offline-generation contract: with NEO_SKIP_NETWORK=1 (no network at all),
    // `neo new` must still scaffold a COMPLETE project from the embedded
    // `neo/starter/` template — proving the starter is baked into the binary and
    // generation does not depend on downloading anything. Run from a fresh temp
    // dir so this also proves cwd-independence of the embedded lookup.
    let temp = tempfile::tempdir().unwrap();
    let project_name = "offline-app";

    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .env("NEO_SKIP_NETWORK", "1")
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project = temp.path().join(project_name);
    // Load-bearing surfaces from the real starter (asserted by presence, never by
    // count): app entry point, executable launcher, a real domain module (not a
    // stub), dev flake, cabal project, and the starter's own test tree.
    assert!(project.join("src/App.hs").exists(), "offline scaffold missing src/App.hs");
    assert!(project.join("launcher/Launcher.hs").exists(), "offline scaffold missing launcher/Launcher.hs");
    assert!(
        project.join("src/Starter/Counter/Event.hs").exists(),
        "offline scaffold missing the embedded starter domain module — a stub, not the full starter, was written"
    );
    assert!(project.join("flake.nix").exists(), "offline scaffold missing flake.nix");
    assert!(project.join("cabal.project").exists(), "offline scaffold missing cabal.project");
    assert!(project.join("tests/Spec.hs").exists(), "offline scaffold missing tests/Spec.hs");

    // The monorepo-only provenance manifest must never leak into a generated project.
    assert!(!project.join("IMPORT.md").exists(), "IMPORT.md must not be scaffolded into a project");

    // The full pipeline (reconcile + git) still completes offline.
    assert!(project.join(format!("{}.cabal", project_name)).exists(), ".cabal not generated offline");
    assert!(project.join(".git").exists(), "git not initialized offline");
    let git_log = std::process::Command::new("git")
        .args(["log", "--oneline"])
        .current_dir(&project)
        .output()
        .unwrap();
    assert!(
        String::from_utf8_lossy(&git_log.stdout).contains("Initial commit from NeoCLI"),
        "initial commit missing after offline scaffold"
    );
}

#[test]
fn test_neo_new_library_ci() {
    // `--library` should produce a project with no launcher/Launcher.hs file
    // and a generated .cabal without the `executable <name>` stanza.
    // The neo.json file should record `"type": "library"`.
    let temp = tempfile::tempdir().unwrap();
    let project_name = "test-lib";

    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--library")
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);
    assert!(project_path.exists());
    assert!(project_path.join("neo.json").exists());
    assert!(project_path.join("src/App.hs").exists());

    // No launcher folder
    assert!(
        !project_path.join("launcher").exists(),
        "library project must not have a launcher/ directory"
    );
    assert!(
        !project_path.join("launcher/Launcher.hs").exists(),
        "library project must not have launcher/Launcher.hs"
    );

    // neo.json records type: library
    let config_content = std::fs::read_to_string(project_path.join("neo.json")).unwrap();
    let config: serde_json::Value = serde_json::from_str(&config_content).unwrap();
    assert_eq!(config["type"], "library", "neo.json should record type=library, got: {}", config_content);

    // Generated .cabal has no executable stanza
    let cabal_path = project_path.join(format!("{}.cabal", project_name));
    assert!(cabal_path.exists(), "{}.cabal should be generated", project_name);
    let cabal = std::fs::read_to_string(&cabal_path).unwrap();
    assert!(
        !cabal.contains(&format!("executable {}", project_name)),
        "library .cabal must not declare an executable stanza:\n{}",
        cabal
    );
    assert!(
        !cabal.contains("main-is: Launcher.hs"),
        "library .cabal must not reference Launcher.hs:\n{}",
        cabal
    );
    assert!(
        cabal.contains("library"),
        "library .cabal must keep the library stanza:\n{}",
        cabal
    );
    // Library projects are testable too: the test-suite is emitted regardless of kind.
    assert!(
        cabal.contains(&format!("test-suite {}-test", project_name)),
        "library .cabal must still declare a test-suite:\n{}",
        cabal
    );
    assert!(project_path.join("tests/Spec.hs").exists(), "library project should get the tests/Spec.hs driver");
}

#[test]
fn test_neo_new_with_custom_name() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "custom-project";
    
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);
    let config_content = std::fs::read_to_string(project_path.join("neo.json")).unwrap();
    let config: serde_json::Value = serde_json::from_str(&config_content).unwrap();
    assert_eq!(config["name"], project_name);
}

#[test]
fn test_neo_build_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "build-project";
    
    // First create a project
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);

    // Without the IOHK + NeoHaskell binary caches wired into the generated flake,
    // `neo build` would compile GHC and haskell.nix infrastructure from source —
    // the "takes hours instead of minutes" failure mode. Verify the template
    // configured both substituters before we attempt to build.
    let flake = std::fs::read_to_string(project_path.join("flake.nix")).unwrap();
    assert!(
        flake.contains("https://cache.iog.io"),
        "generated flake.nix is missing the `cache.iog.io` substituter — neo build would rebuild GHC from source"
    );
    assert!(
        flake.contains("https://neohaskell.cachix.org"),
        "generated flake.nix is missing the `neohaskell.cachix.org` substituter — neo build would rebuild project deps from source"
    );
    assert!(
        flake.contains("hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="),
        "generated flake.nix is missing the IOHK trusted-public-key — substituter URL alone won't trust the cache"
    );
    assert!(
        flake.contains("neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="),
        "generated flake.nix is missing the NeoHaskell trusted-public-key"
    );

    let mut cmd = neo_cmd();
    cmd.current_dir(&project_path)
        .arg("build")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Reconciling project artifacts"));
    assert!(project_path.join(format!("{}.cabal", project_name)).exists());
}

#[test]
fn test_neo_run_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "run-project";

    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);

    // `neo run --ci` launches the starter executable; the default starter is
    // server-style and runs forever, so a bare `assert_cmd`-style invocation
    // would hang the whole `cargo test` session. Wrap with coreutils
    // `timeout` (mirrors `run_ci_completes_or_runs_for_fresh_starter` in
    // tests/e2e.rs): accept exit 0 (finite program) OR 124 (SIGTERM by
    // timeout), and require both reconcile + run markers in stdout as
    // evidence we got past every interesting stage.
    let neo = assert_cmd::cargo::cargo_bin("neo");
    let out = std::process::Command::new("timeout")
        .args(["--signal=TERM", "180"])
        .arg(&neo)
        .args(["run", "--ci"])
        .current_dir(&project_path)
        .output()
        .expect("spawn `timeout` + neo failed (is coreutils `timeout` on PATH?)");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = out.status.code().unwrap_or(-1);

    assert!(
        stdout.contains("Reconciling project artifacts"),
        "missing reconcile marker; stdout=`{}` stderr=`{}` code={}",
        stdout, stderr, code
    );
    assert!(
        stdout.contains("Running project"),
        "missing run marker; stdout=`{}` stderr=`{}` code={}",
        stdout, stderr, code
    );
    assert!(
        out.status.success() || code == 124,
        "unexpected exit code {} (stderr: {})",
        code, stderr
    );
}

#[test]
fn test_neo_test_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "test-project-cmd";
    
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);

    // Full `neo test`: reconcile → compile+run the built-in Hspec suite → wait for
    // the app to become ready (readiness poll, not a fixed sleep) → run the starter's
    // Hurl scenarios. All must pass. (Depends on the embedded `neo/starter/` staying
    // coherent with upstream `neohaskell`: e.g. the `counter-flow.hurl`
    // `$.items[...]` jsonpath; a failure here signals the starter↔upstream contract
    // is broken — fix `neo/starter/` in this monorepo, per neo/AGENTS.md.)
    let mut cmd = neo_cmd();
    cmd.current_dir(&project_path)
        .arg("test")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Reconciling project artifacts"))
        .stdout(predicate::str::contains("Running unit tests"))
        .stdout(predicate::str::contains("Unit tests passed"));
}

#[test]
fn test_neo_test_hurl_discovery() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "hurl-project";
    
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);
    
    // Create a dummy hurl file
    let tests_dir = project_path.join("tests");
    std::fs::create_dir_all(&tests_dir).unwrap();
    std::fs::write(tests_dir.join("api.hurl"), "GET http://localhost:8080\nHTTP *\n").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(&project_path)
        .arg("test")
        .arg("--ci")
        .assert()
        .stdout(predicate::str::contains("Running 1 Hurl integration tests"));
}

#[test]
fn test_neo_build_no_workspace() {
    let temp = tempfile::tempdir().unwrap();
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("No `neo.json` found"));
}

#[test]
fn test_neo_new_existing_dir() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "existing-project";
    let project_path = temp.path().join(project_name);
    std::fs::create_dir_all(&project_path).unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains(format!("Directory `{}` already exists", project_name)));
}

// ============================================================
// Dependency-grammar input validation
// (fast: reconcile fails before cabal is invoked)
// ============================================================

fn write_minimal_project(dir: &std::path::Path, name: &str, deps_json: &str) {
    let neo_json = format!(
        "{{\n  \"name\": \"{}\",\n  \"version\": \"0.1.0\",\n  \"neo-version\": \"main\",\n  \"license\": \"MIT\",\n  \"dependencies\": {}\n}}\n",
        name, deps_json
    );
    std::fs::create_dir_all(dir.join("src")).unwrap();
    std::fs::write(dir.join("src/App.hs"), "module App where\n").unwrap();
    std::fs::create_dir_all(dir.join("launcher")).unwrap();
    std::fs::write(
        dir.join("launcher/Launcher.hs"),
        "module Main where\nmain :: IO ()\nmain = pure ()\n",
    )
    .unwrap();
    std::fs::write(dir.join("neo.json"), neo_json).unwrap();
}

#[test]
fn integration_build_invalid_semver_errors() {
    let temp = tempfile::tempdir().unwrap();
    write_minimal_project(temp.path(), "p", r#"{"foo":"not-a-version"}"#);
    neo_cmd()
        .current_dir(temp.path())
        .env("NEO_SKIP_NETWORK", "1")
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid dependency"));
}

#[test]
fn integration_build_unknown_protocol_errors() {
    let temp = tempfile::tempdir().unwrap();
    write_minimal_project(temp.path(), "p", r#"{"foo":"npm:bar"}"#);
    neo_cmd()
        .current_dir(temp.path())
        .env("NEO_SKIP_NETWORK", "1")
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("unknown protocol"));
}

#[test]
fn integration_build_conflicting_protocols_errors() {
    let temp = tempfile::tempdir().unwrap();
    write_minimal_project(temp.path(), "p", r#"{"hackage:foo":"git:host/r.git"}"#);
    neo_cmd()
        .current_dir(temp.path())
        .env("NEO_SKIP_NETWORK", "1")
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("both key and value"));
}

#[test]
fn integration_build_github_too_many_slashes_errors() {
    let temp = tempfile::tempdir().unwrap();
    write_minimal_project(temp.path(), "p", r#"{"foo":"github:owner/repo/sub"}"#);
    neo_cmd()
        .current_dir(temp.path())
        .env("NEO_SKIP_NETWORK", "1")
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("owner/repo"));
}

#[test]
fn test_neo_build_invalid_config() {
    let temp = tempfile::tempdir().unwrap();
    let project_name = "invalid-config-project";
    
    // Create a project
    let mut cmd = neo_cmd();
    cmd.current_dir(temp.path())
        .arg("new")
        .arg(project_name)
        .arg("--ci")
        .assert()
        .success();

    let project_path = temp.path().join(project_name);
    // Corrupt neo.json
    std::fs::write(project_path.join("neo.json"), "{ \"name\": \"oops\" ").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(&project_path)
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Failed to parse `neo.json`"))
        // The new GraphicalReportHandler renders a source-pointer block:
        // either with unicode `╭─[neo.json:` (TTY) or ASCII `,-[neo.json:` (pipe).
        // assert_cmd pipes stderr, so we get the ASCII fallback.
        .stderr(predicate::str::contains("neo.json:").and(
            predicate::str::contains("syntax error here")
        ));
}

#[test]
fn test_neo_lock_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Create domain files
    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    std::fs::write(commands_dir.join("CreateUser.hs"), "").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Locked and committed"));
}

#[test]
fn test_neo_lock_all_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Create domain files
    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    std::fs::write(commands_dir.join("CreateUser.hs"), "").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("--all")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Locked and committed"));
    
    assert!(project_path.join(".locked-files").exists());
}

#[test]
fn test_neo_lock_multiple_files_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Create multiple domain files
    let commands_dir = project_path.join("src/Domain/Commands");
    let events_dir = project_path.join("src/Domain/Events");
    std::fs::create_dir_all(&commands_dir).unwrap();
    std::fs::create_dir_all(&events_dir).unwrap();
    
    std::fs::write(commands_dir.join("CreateUser.hs"), "").unwrap();
    std::fs::write(events_dir.join("UserCreated.hs"), "").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("--all")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Locked and committed"));
    
    let manifest_content = std::fs::read_to_string(project_path.join(".locked-files")).unwrap();
    assert!(manifest_content.contains("src/Domain/Commands/CreateUser.hs"));
    assert!(manifest_content.contains("src/Domain/Events/UserCreated.hs"));
}

#[test]
fn test_neo_lock_search_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Create domain files
    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    std::fs::write(commands_dir.join("CreateUser.hs"), "").unwrap();
    std::fs::write(commands_dir.join("DeleteUser.hs"), "").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("Create")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Locked and committed"));
    
    let manifest_content = std::fs::read_to_string(project_path.join(".locked-files")).unwrap();
    assert!(manifest_content.contains("src/Domain/Commands/CreateUser.hs"));
    assert!(!manifest_content.contains("src/Domain/Commands/DeleteUser.hs"));
}

#[test]
fn test_neo_lock_install_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Create .git directory
    std::fs::create_dir_all(project_path.join(".git/hooks")).unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("install")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Lock hook installed"));
    
    assert!(project_path.join(".git/hooks/pre-commit").exists());
}

#[test]
fn test_neo_lock_check_violation() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();

    // 1. Init git
    std::process::Command::new("git").arg("init").current_dir(project_path).output().unwrap();
    std::process::Command::new("git").args(["config", "user.email", "test@example.com"]).current_dir(project_path).output().unwrap();
    std::process::Command::new("git").args(["config", "user.name", "Test User"]).current_dir(project_path).output().unwrap();

    // 2. Create a domain file
    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    let file_path = commands_dir.join("CreateUser.hs");
    std::fs::write(&file_path, "initial content").unwrap();

    // 3. Lock it (this also commits it)
    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("--ci")
        .assert()
        .success();

    // 4. Modify and stage it
    std::fs::write(&file_path, "modified content").unwrap();
    std::process::Command::new("git").args(["add", "src/Domain/Commands/CreateUser.hs"]).current_dir(project_path).output().unwrap();

    // 5. Check violation — railguard wording: explainer + V-bump recipe +
    //    worked example. The escape hatches (`neo lock --remove`,
    //    `--skip-lock-check`) must NOT appear; they live in `--help` for
    //    humans who already understand the model.
    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("check")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Build refused"))
        .stderr(predicate::str::contains("src/Domain/Commands/CreateUser.hs"))
        .stderr(predicate::str::contains("event-sourced"))
        .stderr(predicate::str::contains("CreateUserV2.hs"))
        .stderr(predicate::str::contains("neo lock --remove").not())
        .stderr(predicate::str::contains("--skip-lock-check").not());
}

#[test]
fn test_neo_lock_check_unstaged_violation() {
    // Widened semantics: `neo lock check` now catches unstaged modifications
    // too, not just staged ones. A user editing a locked file should see the
    // violation immediately, before they get a chance to `git add`.
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();

    std::process::Command::new("git").arg("init").current_dir(project_path).output().unwrap();
    std::process::Command::new("git").args(["config", "user.email", "test@example.com"]).current_dir(project_path).output().unwrap();
    std::process::Command::new("git").args(["config", "user.name", "Test User"]).current_dir(project_path).output().unwrap();

    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    let file_path = commands_dir.join("CreateUser.hs");
    std::fs::write(&file_path, "initial content").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path).arg("lock").arg("--ci").assert().success();

    // Modify WITHOUT staging.
    std::fs::write(&file_path, "modified content").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("check")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Build refused"))
        .stderr(predicate::str::contains("src/Domain/Commands/CreateUser.hs"));
}

#[test]
fn test_neo_lock_check_pass() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // 1. Init git
    std::process::Command::new("git").arg("init").current_dir(project_path).output().unwrap();

    // 2. Create a file (not locked)
    std::fs::write(project_path.join("README.md"), "hello").unwrap();
    std::process::Command::new("git").args(["add", "README.md"]).current_dir(project_path).output().unwrap();

    // 3. Check should pass even if no manifest exists
    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("check")
        .arg("--ci")
        .assert()
        .success();

    // 4. Create empty manifest and check
    std::fs::write(project_path.join(".locked-files"), "").unwrap();
    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("check")
        .arg("--ci")
        .assert()
        .success();
}

#[test]
fn test_neo_lock_check_missing_manifest() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Check should pass if manifest is missing
    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("check")
        .arg("--ci")
        .assert()
        .success();
}

#[test]
fn test_neo_lock_ambiguous_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    
    // Create multiple domain files
    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    std::fs::write(commands_dir.join("CreateUser.hs"), "").unwrap();
    std::fs::write(commands_dir.join("DeleteUser.hs"), "").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("User")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("Multiple matches found"));
    
    // Should not have created manifest since it was ambiguous
    assert!(!project_path.join(".locked-files").exists());
}

#[test]
fn test_neo_lock_no_matches_ci() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("lock")
        .arg("SomeQuery")
        .arg("--ci")
        .assert()
        .success()
        .stdout(predicate::str::contains("No matches found"));
}

// ---- Pre-build lock check ----
//
// These tests exercise the `--skip-lock-check` flag and the gate that aborts
// `neo build` when a locked file has been modified. The lock check fires
// after `NeoConfig::load` and before reconcile/nix-build, so violation tests
// fail fast (no Haskell compile in the loop). The "skip flag proceeds" test
// runs through real reconcile + nix build and is therefore as slow as
// `test_neo_build_ci`.

/// Hand-roll a minimal NeoHaskell workspace (no `neo new`) so violation tests
/// don't pay the starter-template download. Initializes git, writes the
/// minimal `neo.json` that `NeoConfig::load` accepts, and configures a git
/// identity so subsequent commits work.
fn minimal_workspace(project_path: &std::path::Path) {
    use std::process::Command as Cmd;
    Cmd::new("git").arg("init").current_dir(project_path).output().unwrap();
    Cmd::new("git").args(["config", "user.email", "test@example.com"]).current_dir(project_path).output().unwrap();
    Cmd::new("git").args(["config", "user.name", "Test User"]).current_dir(project_path).output().unwrap();
    std::fs::write(
        project_path.join("neo.json"),
        r#"{"name":"locktest","version":"0.1.0","neo-version":"0.1.0"}"#,
    )
    .unwrap();
}

#[test]
fn test_neo_build_refuses_modified_locked() {
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    minimal_workspace(project_path);

    // Create + lock + commit a domain file.
    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    let file_path = commands_dir.join("CreateUser.hs");
    std::fs::write(&file_path, "initial").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path).arg("lock").arg("--ci").assert().success();

    // Modify and stage the locked file.
    std::fs::write(&file_path, "modified").unwrap();
    std::process::Command::new("git")
        .args(["add", "src/Domain/Commands/CreateUser.hs"])
        .current_dir(project_path)
        .output()
        .unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Build refused"))
        .stderr(predicate::str::contains("src/Domain/Commands/CreateUser.hs"))
        .stderr(predicate::str::contains("event-sourced"))
        .stderr(predicate::str::contains("CreateUserV2.hs"))
        .stderr(predicate::str::contains("byte-identical"))
        .stderr(predicate::str::contains("--skip-lock-check").not())
        .stderr(predicate::str::contains("neo lock --remove").not())
        .stderr(predicate::str::contains("git checkout --").not());
}

#[test]
fn test_neo_build_unstaged_locked_modification_refused() {
    // Proves the widened semantics: unstaged edits to a locked file also
    // abort the build, not just staged ones.
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    minimal_workspace(project_path);

    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    let file_path = commands_dir.join("CreateUser.hs");
    std::fs::write(&file_path, "initial").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path).arg("lock").arg("--ci").assert().success();

    // Modify the file but do NOT stage.
    std::fs::write(&file_path, "modified").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Build refused"))
        .stderr(predicate::str::contains("src/Domain/Commands/CreateUser.hs"));
}

#[test]
fn test_neo_build_untracked_path_in_manifest_refused() {
    // Exercises the `??` porcelain status code: a path listed in
    // `.locked-files` exists on disk but is untracked. The lock check should
    // still flag it.
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    minimal_workspace(project_path);

    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    let ghost = commands_dir.join("Ghost.hs");
    std::fs::write(&ghost, "untracked").unwrap();
    std::fs::write(
        project_path.join(".locked-files"),
        "src/Domain/Commands/Ghost.hs",
    )
    .unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("build")
        .arg("--ci")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Build refused"))
        .stderr(predicate::str::contains("src/Domain/Commands/Ghost.hs"));
}

#[test]
fn test_neo_build_skip_lock_check_bypasses_check() {
    // The flag must let the build proceed past the lock-check stage even
    // with a modified locked file. We don't assert on overall success — the
    // hand-rolled workspace has no flake.nix or source code, so reconcile/
    // nix-build will fail downstream. What we DO assert is that the failure
    // is NOT the lock-violation diagnostic.
    let temp = tempfile::tempdir().unwrap();
    let project_path = temp.path();
    minimal_workspace(project_path);

    let commands_dir = project_path.join("src/Domain/Commands");
    std::fs::create_dir_all(&commands_dir).unwrap();
    let file_path = commands_dir.join("CreateUser.hs");
    std::fs::write(&file_path, "initial").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path).arg("lock").arg("--ci").assert().success();

    // Modify the locked file.
    std::fs::write(&file_path, "modified").unwrap();

    let mut cmd = neo_cmd();
    cmd.current_dir(project_path)
        .arg("build")
        .arg("--ci")
        .arg("--skip-lock-check")
        .assert()
        // Build may pass or fail downstream — we don't care. The point is
        // that the lock check did not block.
        .stderr(predicate::str::contains("Build refused").not())
        .stderr(predicate::str::contains("neo::lock_violation").not());
}

// =====================================================================
// `neo inspect sync` — code→model sync of event-model.json
//
// These need NO nix/network: the sync parses `.hs` text + reads/writes
// event-model.json. So they run fast against a hand-written src/ tree.
// =====================================================================

/// Write a tiny Cart domain (one event with fields, one command) plus an EMPTY
/// event-model.json, so the first `neo inspect sync` materialises + lays out the
/// whole structure (the full-heal path).
fn write_sync_fixture(root: &std::path::Path) {
    let write = |rel: &str, body: &str| {
        let p = root.join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, body).unwrap();
    };
    write(
        "src/App/Cart/Core.hs",
        "module App.Cart.Core where\n\
         data CartEvent = ItemAdded { stockId :: Uuid, quantity :: Int } deriving (Generic)\n",
    );
    write(
        "src/App/Cart/Commands/AddItem.hs",
        "module App.Cart.Commands.AddItem where\n\
         data AddItem = AddItem { stockId :: Uuid }\n\
         decide _ _ _ = Decider.acceptExisting [ItemAdded {}]\n",
    );
    write(
        "src/App/Cart/Queries/CartView.hs",
        "module App.Cart.Queries.CartView (CartView (..)) where\n\
         data CartView = CartView { cartId :: Uuid, itemCount :: Int } deriving (Generic)\n",
    );
    let model = serde_json::json!({
        "id": "m", "name": "demo", "chapters": [], "entities": [], "slices": [],
        "nodes": [], "edges": [],
        "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
    });
    std::fs::write(
        root.join("event-model.json"),
        serde_json::to_string_pretty(&model).unwrap(),
    )
    .unwrap();
}

fn read_event_model(root: &std::path::Path) -> serde_json::Value {
    serde_json::from_str(&std::fs::read_to_string(root.join("event-model.json")).unwrap()).unwrap()
}

#[test]
fn inspect_sync_updates_fields_from_source() {
    let temp = tempfile::tempdir().unwrap();
    write_sync_fixture(temp.path());

    neo_cmd()
        .args(["inspect", "sync"])
        .current_dir(temp.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok] synced event-model.json"));

    // The event + command were materialised and carry their source fields.
    let model = read_event_model(temp.path());
    let ev = model["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "ItemAdded").unwrap();
    let ev_names: Vec<&str> = ev["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
    assert_eq!(ev_names, vec!["stockId", "quantity"], "event node fields synced from source");
    let cmd = model["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "AddItem").expect("command materialised");
    let cmd_names: Vec<&str> = cmd["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
    assert_eq!(cmd_names, vec!["stockId"], "command node fields synced from source");
    // Query read-model fields sync too (the originally-missing case).
    let q = model["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "CartView").expect("query materialised");
    let q_names: Vec<&str> = q["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
    assert_eq!(q_names, vec!["cartId", "itemCount"], "query node fields synced from source");
}

#[test]
fn inspect_sync_payload_module_event_fields() {
    // The real starter event shape: a sum arm `Ctor Module.Event` whose fields
    // live in `Events/<Module>.hs`. This was the originally-missing case —
    // events showed no fields. Drive it through the shipped CLI path.
    let temp = tempfile::tempdir().unwrap();
    let write = |rel: &str, body: &str| {
        let p = temp.path().join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, body).unwrap();
    };
    write(
        "src/App/Counter/Event.hs",
        "module App.Counter.Event (CounterEvent (..)) where\n\
         data CounterEvent = CounterCreated CounterCreated.Event deriving (Generic, Show)\n",
    );
    write(
        "src/App/Counter/Events/CounterCreated.hs",
        "module App.Counter.Events.CounterCreated (Event (..)) where\n\
         data Event = Event { entityId :: Uuid, label :: Text } deriving (Generic, Show)\n",
    );
    // A command so the domain has a Commands/ dir and the event is reachable.
    write(
        "src/App/Counter/Commands/CreateCounter.hs",
        "module App.Counter.Commands.CreateCounter where\n\
         data CreateCounter = CreateCounter { label :: Text }\n\
         decide _ _ _ = Decider.acceptExisting [CounterCreated {}]\n",
    );
    write(
        "event-model.json",
        &serde_json::to_string_pretty(&serde_json::json!({
            "id": "m", "name": "demo", "chapters": [], "entities": [], "slices": [],
            "nodes": [], "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        }))
        .unwrap(),
    );

    neo_cmd().args(["inspect", "sync"]).current_dir(temp.path()).assert().success();

    let model = read_event_model(temp.path());
    let ev = model["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "CounterCreated").expect("event node");
    let names: Vec<&str> = ev["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
    assert_eq!(names, vec!["entityId", "label"], "payload-module event fields synced from Events/<Module>.hs");
}

#[test]
fn inspect_sync_existing_node_field_edit_is_data_only() {
    let temp = tempfile::tempdir().unwrap();
    write_sync_fixture(temp.path());

    // First sync materialises + lays out everything.
    neo_cmd().args(["inspect", "sync"]).current_dir(temp.path()).assert().success();
    let model = read_event_model(temp.path());
    let ev_id = model["nodes"].as_array().unwrap().iter()
        .find(|n| n["name"] == "ItemAdded").unwrap()["id"].as_str().unwrap().to_string();
    let pos_before = model["layout"]["nodePositions"][&ev_id].clone();
    assert!(pos_before.is_object(), "event node must be positioned after the first sync");

    // Edit ONLY the existing event's fields in source (add `note :: Text`).
    std::fs::write(
        temp.path().join("src/App/Cart/Core.hs"),
        "module App.Cart.Core where\n\
         data CartEvent = ItemAdded { stockId :: Uuid, quantity :: Int, note :: Text } deriving (Generic)\n",
    )
    .unwrap();

    neo_cmd()
        .args(["inspect", "sync"])
        .current_dir(temp.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("fields only"));

    let model2 = read_event_model(temp.path());
    let ev2 = model2["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "ItemAdded").unwrap();
    let names: Vec<&str> = ev2["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
    assert_eq!(names, vec!["stockId", "quantity", "note"], "the new field synced");
    assert_eq!(
        model2["layout"]["nodePositions"][&ev_id], pos_before,
        "editing fields of an EXISTING node must not move layout",
    );
}

#[test]
fn inspect_sync_idempotent_second_run() {
    let temp = tempfile::tempdir().unwrap();
    write_sync_fixture(temp.path());

    neo_cmd().args(["inspect", "sync"]).current_dir(temp.path()).assert().success();
    let after_first = std::fs::read_to_string(temp.path().join("event-model.json")).unwrap();

    neo_cmd()
        .args(["inspect", "sync"])
        .current_dir(temp.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("already in sync"));
    assert_eq!(
        std::fs::read_to_string(temp.path().join("event-model.json")).unwrap(),
        after_first,
        "second sync must leave event-model.json byte-identical",
    );
}

#[test]
fn inspect_sync_missing_event_model_errors() {
    // No event-model.json in the workspace → actionable failure, nonzero exit.
    let temp = tempfile::tempdir().unwrap();
    neo_cmd()
        .args(["inspect", "sync"])
        .current_dir(temp.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("event-model.json"));
}

// =====================================================================
// `neo ide` — JSON-RPC over WebSocket
//
// Each test:
//   1. Bind a probe TCP socket to grab a free port, drop it.
//   2. Spawn `neo --ci ide --port <p>` from a tempdir (so each test has its
//      own "workspace").
//   3. Connect a tokio-tungstenite WS client to `ws://127.0.0.1:<p>/ws`.
//   4. Exchange frames, assert, kill the child.
// =====================================================================

mod ide_ws {
    use futures_util::{SinkExt, StreamExt};
    use serde_json::json;
    use std::process::{Child, Stdio};
    use std::time::Duration;
    use tokio_tungstenite::tungstenite::Message;

    /// Reserve a port by binding-then-dropping. Tiny race window; rare in
    /// practice on a developer machine + CI.
    fn reserve_port() -> u16 {
        let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let port = l.local_addr().unwrap().port();
        drop(l);
        port
    }

    /// Spawn `neo --ci ide --port <port>` in `cwd` and wait until it is
    /// accepting connections (or panic on timeout).
    fn spawn_ide(cwd: &std::path::Path, port: u16) -> Child {
        let neo = assert_cmd::cargo::cargo_bin("neo");
        let child = std::process::Command::new(&neo)
            .current_dir(cwd)
            .arg("--ci")
            .arg("ide")
            .arg("--port")
            .arg(port.to_string())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn neo ide");

        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while std::time::Instant::now() < deadline {
            if std::net::TcpStream::connect(("127.0.0.1", port)).is_ok() {
                return child;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        panic!("neo ide did not start listening on port {port} within 10s");
    }

    fn kill(mut child: Child) {
        let _ = child.kill();
        let _ = child.wait();
    }

    async fn ws_connect(
        port: u16,
    ) -> tokio_tungstenite::WebSocketStream<tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>>
    {
        let url = format!("ws://127.0.0.1:{port}/ws");
        let (ws, _resp) = tokio_tungstenite::connect_async(&url)
            .await
            .expect("ws connect");
        ws
    }

    async fn send_recv(
        ws: &mut tokio_tungstenite::WebSocketStream<
            tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>,
        >,
        payload: serde_json::Value,
    ) -> serde_json::Value {
        ws.send(Message::Text(payload.to_string()))
            .await
            .expect("send");
        // Skip any server-pushed notifications (they have no `id`) and
        // return the first frame that looks like a JSON-RPC response.
        // Long-running handlers like heal stream `$/progress` events
        // before the response, so a naive one-recv loop wouldn't get
        // the answer.
        let deadline = std::time::Instant::now() + Duration::from_secs(60);
        loop {
            let recv = tokio::time::timeout(Duration::from_secs(30), ws.next()).await;
            let msg = recv
                .expect("recv timeout")
                .expect("stream closed")
                .expect("recv error");
            let text = match msg {
                Message::Text(t) => t,
                other => panic!("unexpected ws message: {other:?}"),
            };
            let value: serde_json::Value =
                serde_json::from_str(&text).expect("frame is JSON");
            if value.get("id").is_some() {
                return value;
            }
            if std::time::Instant::now() > deadline {
                panic!("never received a response, only notifications");
            }
        }
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_initialize_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": { "clientInfo": { "name": "it-test", "version": "0" } }
            }),
        )
        .await;

        assert_eq!(resp["id"], 1);
        assert!(resp["error"].is_null(), "no error expected: {resp}");
        let result = &resp["result"];
        assert_eq!(result["serverInfo"]["name"], "neo");
        assert_eq!(
            result["serverInfo"]["version"].as_str().unwrap(),
            env!("CARGO_PKG_VERSION"),
        );
        assert!(result["workspace"]["root"].is_string(), "workspace.root present");
        assert!(result["workspace"]["project"].is_null(), "no neo.json in tempdir");
        assert!(
            result["sessionId"].as_str().unwrap().starts_with("session_"),
            "session_id present: {result}",
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_unknown_method_returns_method_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":7,"method":"does/not/exist"}),
        )
        .await;
        assert_eq!(resp["id"], 7);
        assert_eq!(resp["error"]["code"], -32601);
        assert!(
            resp["error"]["message"].as_str().unwrap().contains("does/not/exist"),
            "method named in error: {resp}",
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_invalid_json_returns_parse_error() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        ws.send(Message::Text("{garbage".to_string()))
            .await
            .unwrap();
        let msg = tokio::time::timeout(Duration::from_secs(5), ws.next())
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        let resp: serde_json::Value = match msg {
            Message::Text(t) => serde_json::from_str(&t).unwrap(),
            other => panic!("unexpected: {other:?}"),
        };
        // Parse error → id null per spec.
        assert!(resp["id"].is_null(), "parse error id must be null: {resp}");
        assert_eq!(resp["error"]["code"], -32700);

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_multiple_concurrent_connections() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut a = ws_connect(port).await;
        let mut b = ws_connect(port).await;
        let resp_a = send_recv(
            &mut a,
            json!({"jsonrpc":"2.0","id":1,"method":"initialize",
                   "params":{"clientInfo":{"name":"a","version":"0"}}}),
        )
        .await;
        let resp_b = send_recv(
            &mut b,
            json!({"jsonrpc":"2.0","id":1,"method":"initialize",
                   "params":{"clientInfo":{"name":"b","version":"0"}}}),
        )
        .await;
        let sid_a = resp_a["result"]["sessionId"].as_str().unwrap().to_string();
        let sid_b = resp_b["result"]["sessionId"].as_str().unwrap().to_string();
        assert_ne!(sid_a, sid_b, "two connections must have distinct session ids");
        // Both see the same workspace.
        assert_eq!(resp_a["result"]["workspace"]["id"], resp_b["result"]["workspace"]["id"]);

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_static_assets_still_served_after_ws_mount() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let body = reqwest::get(format!("http://127.0.0.1:{port}/"))
            .await
            .unwrap()
            .text()
            .await
            .unwrap();
        assert!(
            body.contains("id=\"root\""),
            "static index.html still served (looking for React mount point): {body}",
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_event_model_write_then_read_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let payload = r#"{"name":"e2e","slices":[]}"#.to_string();

        // Write
        let resp_write = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/writeEventModel",
                   "params":{"content": payload}}),
        )
        .await;
        assert!(resp_write["error"].is_null(), "write failed: {resp_write}");
        assert!(
            resp_write["result"]["path"].as_str().unwrap().ends_with("event-model.json"),
            "result echoes the path: {resp_write}",
        );

        // File landed in the workspace cwd.
        let on_disk = std::fs::read_to_string(dir.path().join("event-model.json")).unwrap();
        assert_eq!(on_disk, payload, "file content matches write payload");

        // Read it back
        let resp_read = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":2,"method":"workspace/readEventModel","params":{}}),
        )
        .await;
        assert!(resp_read["error"].is_null(), "read failed: {resp_read}");
        assert_eq!(resp_read["result"]["content"].as_str().unwrap(), payload);

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_relayout_orders_by_flow_and_is_idempotent() {
        // Over the wire: a spaghetti model whose stored slice order is the
        // REVERSE of the causal flow. `workspace/relayoutEventModel` must
        // reorder by the wave (initializer command first) on the first call
        // and be a no-op (`applied == 0`) on the second. Pure-local — no
        // NeoHaskell project, no network, no LLM.
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        // Stored order First(0) < Second(1) < Third(2); causal flow is the
        // reverse: Third(initializer cmd) -> Second(integration) -> First.
        let spaghetti = json!({
            "id": "m", "name": "ws-relayout",
            "chapters": [],
            "entities": [{ "id": "e", "name": "E", "order": 0 }],
            "slices": [
                { "id": "s1", "name": "First",  "chapterId": null, "order": 0 },
                { "id": "s2", "name": "Second", "chapterId": null, "order": 1 },
                { "id": "s3", "name": "Third",  "chapterId": null, "order": 2 }
            ],
            "nodes": [
                { "id": "c0", "type": "command", "name": "Initiate",  "sliceId": "s3", "entityId": "e" },
                { "id": "e0", "type": "event",   "name": "Initiated", "sliceId": "s3", "entityId": "e" },
                { "id": "i0", "type": "integration", "name": "Bridge", "sliceId": "s2", "kind": "inbound" },
                { "id": "c1", "type": "command", "name": "Continue",  "sliceId": "s1", "entityId": "e" },
                { "id": "e1", "type": "event",   "name": "Continued", "sliceId": "s1", "entityId": "e" }
            ],
            "edges": [
                { "id": "x1", "type": "commandProducesEvent",       "sourceId": "c0", "targetId": "e0" },
                { "id": "x2", "type": "eventTriggersIntegration",   "sourceId": "e0", "targetId": "i0" },
                { "id": "x3", "type": "integrationTriggersCommand", "sourceId": "i0", "targetId": "c1" },
                { "id": "x4", "type": "commandProducesEvent",       "sourceId": "c1", "targetId": "e1" }
            ],
            "layout": {
                "nodePositions": {
                    "c0": { "x": 800, "y": 120 }, "e0": { "x": 800, "y": 400 },
                    "i0": { "x": 400, "y": 120 },
                    "c1": { "x": 40,  "y": 120 }, "e1": { "x": 40,  "y": 400 }
                },
                "viewport": { "x": 0, "y": 0, "zoom": 1 }
            }
        });
        std::fs::write(
            dir.path().join("event-model.json"),
            serde_json::to_string_pretty(&spaghetti).unwrap(),
        )
        .unwrap();

        let mut ws = ws_connect(port).await;

        // First relayout — should apply the wave order.
        let resp1 = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/relayoutEventModel","params":{}}),
        )
        .await;
        assert!(resp1["error"].is_null(), "relayout failed: {resp1}");
        assert!(
            resp1["result"]["applied"].as_u64().unwrap() > 0,
            "first relayout should reorder: {resp1}",
        );

        // The file on disk now follows the wave: Third < Second < First.
        let patched: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(dir.path().join("event-model.json")).unwrap(),
        )
        .unwrap();
        let order_of = |name: &str| {
            patched["slices"]
                .as_array()
                .unwrap()
                .iter()
                .find(|s| s["name"] == name)
                .unwrap()["order"]
                .as_f64()
                .unwrap()
        };
        assert!(
            order_of("Third") < order_of("Second") && order_of("Second") < order_of("First"),
            "wave order Third<Second<First; got T={} S={} F={}",
            order_of("Third"), order_of("Second"), order_of("First"),
        );

        // Second relayout — fixed point.
        let resp2 = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":2,"method":"workspace/relayoutEventModel","params":{}}),
        )
        .await;
        assert!(resp2["error"].is_null(), "second relayout failed: {resp2}");
        assert_eq!(
            resp2["result"]["applied"].as_u64().unwrap(),
            0,
            "second relayout must be a no-op: {resp2}",
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_event_model_read_returns_null_when_file_missing() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/readEventModel","params":{}}),
        )
        .await;
        assert!(resp["error"].is_null(), "read should succeed even when file missing: {resp}");
        assert!(resp["result"]["content"].is_null(), "content must be null: {resp}");

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_ws_initialize_reports_project_when_neo_json_present() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("neo.json"),
            r#"{"name":"wsproj","version":"0.9.0","neo-version":"0.1.0"}"#,
        )
        .unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"initialize",
                   "params":{"clientInfo":{"name":"t","version":"0"}}}),
        )
        .await;
        let project = &resp["result"]["workspace"]["project"];
        assert_eq!(project["name"], "wsproj");
        assert_eq!(project["version"], "0.9.0");
        assert_eq!(project["neoVersion"], "0.1.0");

        kill(child);
    }

    // ── validation-on-read ────────────────────────────────────────────────
    // The new `validation` field on `workspace/readEventModel` must surface
    // schema + referential errors over the wire so the frontend can show
    // the heal modal without re-validating client-side.

    const VALID_EVENT_MODEL: &str = r#"{
  "id": "m1",
  "name": "demo",
  "chapters": [],
  "entities": [],
  "slices": [],
  "nodes": [],
  "edges": [],
  "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
}"#;

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_read_event_model_reports_valid_over_ws() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), VALID_EVENT_MODEL).unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/readEventModel","params":{}}),
        )
        .await;
        assert_eq!(
            resp["result"]["validation"]["status"], "valid",
            "expected status=valid, got {resp}"
        );
        assert_eq!(
            resp["result"]["content"].as_str().unwrap(),
            VALID_EVENT_MODEL
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_read_event_model_reports_not_found_over_ws() {
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/readEventModel","params":{}}),
        )
        .await;
        assert_eq!(
            resp["result"]["validation"]["status"], "notFound",
            "expected status=notFound, got {resp}"
        );
        assert!(
            resp["result"]["content"].is_null(),
            "content must be null when notFound: {resp}"
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_read_event_model_reports_invalid_over_ws() {
        let dir = tempfile::tempdir().unwrap();
        // Valid JSON but missing the required `id` field.
        let bad = r#"{"name":"demo","chapters":[],"entities":[],"slices":[],"nodes":[],"edges":[],"layout":{"nodePositions":{},"viewport":{"x":0,"y":0,"zoom":1}}}"#;
        std::fs::write(dir.path().join("event-model.json"), bad).unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/readEventModel","params":{}}),
        )
        .await;
        assert_eq!(
            resp["result"]["validation"]["status"], "invalid",
            "expected status=invalid, got {resp}"
        );
        let errors = resp["result"]["validation"]["errors"]
            .as_array()
            .expect("errors array");
        assert!(!errors.is_empty(), "expected at least one error: {resp}");
        // Content is still returned so the modal can show context.
        assert_eq!(resp["result"]["content"].as_str().unwrap(), bad);

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_read_event_model_reports_malformed_json_over_ws() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), "{not json").unwrap();
        let port = reserve_port();
        let child = spawn_ide(dir.path(), port);

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/readEventModel","params":{}}),
        )
        .await;
        assert_eq!(
            resp["result"]["validation"]["status"], "malformedJson",
            "expected status=malformedJson, got {resp}"
        );
        assert!(
            resp["result"]["validation"]["parseError"].is_string(),
            "parseError must be populated: {resp}"
        );

        kill(child);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_logs_rpc_requests_to_stderr() {
        // Verifies the tracing subscriber installed by `commands/ide.rs`
        // surfaces RPC traffic on stderr — the "what is happening" signal
        // the heal flow relies on. We spawn `neo ide` with stderr piped,
        // send one initialize + one readEventModel, then assert both
        // method names appear in the captured stderr.
        use std::io::Read;
        let dir = tempfile::tempdir().unwrap();
        let port = reserve_port();
        let neo = assert_cmd::cargo::cargo_bin("neo");
        let mut child = std::process::Command::new(&neo)
            .current_dir(dir.path())
            .arg("--ci")
            .arg("ide")
            .arg("--port")
            .arg(port.to_string())
            .env("RUST_LOG", "neo=info")
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn neo ide");

        // Wait for listen.
        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while std::time::Instant::now() < deadline {
            if std::net::TcpStream::connect(("127.0.0.1", port)).is_ok() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }

        let mut ws = ws_connect(port).await;
        let _ = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"initialize",
                   "params":{"clientInfo":{"name":"t","version":"0"}}}),
        )
        .await;
        let _ = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":2,"method":"workspace/readEventModel","params":{}}),
        )
        .await;

        // Kill the child and harvest the buffered stderr.
        let _ = child.kill();
        let _ = child.wait();
        let mut stderr_buf = String::new();
        if let Some(mut stderr) = child.stderr.take() {
            let _ = stderr.read_to_string(&mut stderr_buf);
        }

        assert!(
            stderr_buf.contains("rpc request") && stderr_buf.contains("method=initialize"),
            "expected `rpc request` log for initialize, got stderr: {stderr_buf}",
        );
        assert!(
            stderr_buf.contains("method=workspace/readEventModel"),
            "expected `rpc request` log for readEventModel, got stderr: {stderr_buf}",
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_heal_event_model_missing_claude_surfaces_error() {
        // Spawn `neo ide` with PATH stripped down so `claude` is not
        // resolvable. The healing handler must surface a structured
        // RpcError with the `neo::ide::healing::claude_missing` code.
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), "{}").unwrap();
        let port = reserve_port();
        // spawn_ide currently inherits PATH; we need a custom spawn that
        // strips claude. Do it inline rather than generalising spawn_ide.
        use std::process::{Command, Stdio};
        let neo_bin = assert_cmd::cargo::cargo_bin("neo");
        // Minimal PATH that has nix-store paths for `git`/`ssh` etc but
        // explicitly excludes anywhere `claude` lives. We use only the
        // directory of `neo` itself.
        let bin_dir = neo_bin.parent().unwrap().display().to_string();
        let stripped_path = bin_dir;
        let mut child = Command::new(&neo_bin)
            .arg("--ci")
            .arg("ide")
            .arg("--port")
            .arg(port.to_string())
            .current_dir(dir.path())
            .env_clear()
            .env("PATH", stripped_path)
            .env("HOME", dir.path())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn neo ide with stripped PATH");

        // Wait for listen
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
        while std::time::Instant::now() < deadline {
            if std::net::TcpStream::connect(("127.0.0.1", port)).is_ok() {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
        }

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/healEventModel","params":{}}),
        )
        .await;

        assert_eq!(
            resp["error"]["code"], -32000,
            "expected app error -32000, got {resp}"
        );
        let code = resp["error"]["data"]["diagnosticCode"]
            .as_str()
            .unwrap_or("");
        assert_eq!(
            code, "neo::ide::healing::claude_missing",
            "diagnostic code must name the missing-claude variant, got {resp}"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_logs_heal_failure_dumps_full_subprocess_output() {
        // The user's report: "I only see that claude exited with code 1 but
        // no idea of what failed and how". This test stubs `claude` with a
        // shell script that prints distinct sentinels to stdout AND stderr
        // before exiting non-zero, then asserts both surface in neo's
        // captured stderr alongside the "claude failed" log line.
        use std::io::{Read, Write};
        use std::os::unix::fs::PermissionsExt;

        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), "{}").unwrap();

        // Stub claude that emits a stdout line, a stderr line, then exits 1.
        // `/bin/sh` (not `env bash`) so the script runs even though the
        // outer `env_clear()` strips PATH of bash.
        let stub_dir = tempfile::tempdir().unwrap();
        let stub = stub_dir.path().join("claude");
        let stub_body = "#!/bin/sh\n\
            echo 'STUB_STDOUT_SENTINEL: claude pretending to think'\n\
            echo 'STUB_STDERR_SENTINEL: claude pretending to fail' 1>&2\n\
            exit 1\n";
        {
            let mut f = std::fs::File::create(&stub).unwrap();
            f.write_all(stub_body.as_bytes()).unwrap();
        }
        let mut perms = std::fs::metadata(&stub).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&stub, perms).unwrap();

        // Spawn `neo ide` with PATH pointing only at the stub dir so `claude`
        // resolves to our shim. Also keep neo's own bin dir on PATH.
        let neo_bin = assert_cmd::cargo::cargo_bin("neo");
        let path_value = format!(
            "{}:{}",
            stub_dir.path().display(),
            neo_bin.parent().unwrap().display()
        );
        let port = reserve_port();
        let mut child = std::process::Command::new(&neo_bin)
            .arg("--ci")
            .arg("ide")
            .arg("--port")
            .arg(port.to_string())
            .current_dir(dir.path())
            .env_clear()
            .env("PATH", path_value)
            .env("HOME", dir.path())
            .env("RUST_LOG", "neo=info")
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn neo ide with stub claude");

        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while std::time::Instant::now() < deadline {
            if std::net::TcpStream::connect(("127.0.0.1", port)).is_ok() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }

        let mut ws = ws_connect(port).await;
        let resp = send_recv(
            &mut ws,
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/healEventModel","params":{}}),
        )
        .await;
        // The RPC itself fails — that's expected; we just want the logs.
        assert!(
            resp["error"]["data"]["diagnosticCode"]
                .as_str()
                .unwrap_or("")
                == "neo::ide::healing::failed",
            "expected healing::failed diagnostic, got {resp}"
        );

        let _ = child.kill();
        let _ = child.wait();
        let mut stderr_buf = String::new();
        if let Some(mut stderr) = child.stderr.take() {
            let _ = stderr.read_to_string(&mut stderr_buf);
        }

        // The full-dump log must include BOTH sentinels so the user sees
        // exactly what claude wrote without scrolling through interleaved
        // per-line streams.
        assert!(
            stderr_buf.contains("STUB_STDOUT_SENTINEL"),
            "stderr should include captured stdout content, got:\n{stderr_buf}",
        );
        assert!(
            stderr_buf.contains("STUB_STDERR_SENTINEL"),
            "stderr should include captured stderr content, got:\n{stderr_buf}",
        );
        // And the failure-summary log must be present.
        assert!(
            stderr_buf.contains("heal: claude failed"),
            "stderr should include the heal-failure summary log, got:\n{stderr_buf}",
        );
        // And the spawn-command log must echo the args so the user can copy-paste.
        assert!(
            stderr_buf.contains("--add-dir") && stderr_buf.contains("--allowed-tools"),
            "stderr should include the spawn args, got:\n{stderr_buf}",
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn ide_heal_streams_progress_notifications_over_ws() {
        // The healing flow pushes `$/progress` notifications with claude's
        // stdout/stderr lines so the frontend overlay can render them
        // live. This test stubs claude with a script that prints a
        // unique sentinel, runs heal over a real WS, drains every frame,
        // and asserts that at least one $/progress notification carrying
        // the sentinel arrived BEFORE the final RPC response.
        use std::io::Write;
        use std::os::unix::fs::PermissionsExt;

        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("event-model.json"), "{}").unwrap();

        let stub_dir = tempfile::tempdir().unwrap();
        let stub = stub_dir.path().join("claude");
        let stub_body = "#!/bin/sh\n\
            echo 'STREAMED_PROGRESS_SENTINEL'\n\
            exit 0\n";
        {
            let mut f = std::fs::File::create(&stub).unwrap();
            f.write_all(stub_body.as_bytes()).unwrap();
        }
        let mut perms = std::fs::metadata(&stub).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&stub, perms).unwrap();

        let neo_bin = assert_cmd::cargo::cargo_bin("neo");
        let path_value = format!(
            "{}:{}",
            stub_dir.path().display(),
            neo_bin.parent().unwrap().display()
        );
        let port = reserve_port();
        let mut child = std::process::Command::new(&neo_bin)
            .arg("--ci")
            .arg("ide")
            .arg("--port")
            .arg(port.to_string())
            .current_dir(dir.path())
            .env_clear()
            .env("PATH", path_value)
            .env("HOME", dir.path())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn neo ide");

        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while std::time::Instant::now() < deadline {
            if std::net::TcpStream::connect(("127.0.0.1", port)).is_ok() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }

        let mut ws = ws_connect(port).await;
        ws.send(Message::Text(
            json!({"jsonrpc":"2.0","id":1,"method":"workspace/healEventModel","params":{"mode":"validate"}}).to_string(),
        ))
        .await
        .expect("send heal request");

        // Drain frames until we've seen the response AND read everything
        // queued behind it (notifications can race past the response in
        // the select loop). We keep reading for up to 30 s for the
        // response, then a 1 s post-response drain to scoop up any
        // trailing $/progress frames.
        let mut progress_log_lines: Vec<String> = Vec::new();
        let mut got_response = false;
        let mut drain_until: Option<std::time::Instant> = None;
        let overall_deadline = std::time::Instant::now() + Duration::from_secs(30);
        loop {
            if std::time::Instant::now() > overall_deadline {
                break;
            }
            if let Some(t) = drain_until {
                if std::time::Instant::now() > t {
                    break;
                }
            }
            let recv = tokio::time::timeout(Duration::from_millis(500), ws.next()).await;
            let msg = match recv {
                Ok(Some(Ok(m))) => m,
                Ok(Some(Err(_))) => break,
                Ok(None) => break,
                Err(_) => {
                    if got_response {
                        break;
                    } else {
                        continue;
                    }
                }
            };
            let Message::Text(t) = msg else { continue };
            let value: serde_json::Value = match serde_json::from_str(&t) {
                Ok(v) => v,
                Err(_) => continue,
            };
            if value.get("id").is_some() {
                got_response = true;
                drain_until = Some(std::time::Instant::now() + Duration::from_secs(1));
                continue;
            }
            if value["method"] == "$/progress"
                && value["params"]["token"] == "healEventModel"
                && value["params"]["value"]["kind"] == "log"
            {
                if let Some(line) = value["params"]["value"]["line"].as_str() {
                    progress_log_lines.push(line.to_string());
                }
            }
        }

        let _ = child.kill();
        let _ = child.wait();

        assert!(got_response, "never received the final heal RPC response");
        assert!(
            progress_log_lines
                .iter()
                .any(|l| l.contains("STREAMED_PROGRESS_SENTINEL")),
            "expected a $/progress notification carrying the stub's stdout sentinel; got: {progress_log_lines:?}",
        );
    }
}

// =====================================================
// `neo skills setup` (hermetic: NEO_SKIP_NETWORK stub, isolated NEO_HOME cache)
// =====================================================
//
// Under NEO_SKIP_NETWORK the clone is replaced by a deterministic stub library
// with one skill (`sample-skill`). Each test points NEO_HOME at its own tempdir
// so the skills cache is isolated and the run is reproducible.

/// `(project_dir, neo_home)` tempdirs — keep both alive for the command.
fn skills_sandbox() -> (tempfile::TempDir, tempfile::TempDir) {
    (tempfile::tempdir().unwrap(), tempfile::tempdir().unwrap())
}

fn skills_cmd(project: &std::path::Path, neo_home: &std::path::Path) -> Command {
    let mut cmd = neo_cmd();
    cmd.current_dir(project)
        .env("NEO_SKIP_NETWORK", "1")
        .env("NEO_HOME", neo_home)
        .arg("skills")
        .arg("setup")
        .arg("--ci");
    cmd
}

#[test]
fn skills_setup_ci_all_tools_creates_dests() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .arg("--all-tools")
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok] installed"));

    assert!(proj.path().join(".claude/skills/sample-skill/SKILL.md").exists());
    // Codex installs to `.agents/skills`, NOT `.codex/skills`.
    assert!(proj.path().join(".agents/skills/sample-skill/SKILL.md").exists());
    assert!(!proj.path().join(".codex/skills").exists());
    assert!(proj.path().join(".kiro/skills/sample-skill/SKILL.md").exists());
    assert!(proj.path().join(".cursor/rules/sample-skill.mdc").exists());
    // There is no universal AGENTS.md skills dump. AGENTS.md exists only because
    // codex/kiro inline the (single, small) primer into it — never the skill bodies.
    let agents = std::fs::read_to_string(proj.path().join("AGENTS.md")).unwrap();
    assert!(!agents.contains("BEGIN neo skills"), "no universal skills block");
    assert!(!agents.contains("### sample-skill"), "skill bodies not inlined into AGENTS.md");
    assert!(agents.contains("<!-- BEGIN neohaskell-skills -->"), "primer inlined into AGENTS.md");
}

#[test]
fn skills_setup_idempotent_twice() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path()).arg("--all-tools").assert().success();
    // Second run: everything is unchanged. 4 skill items (one per tool) + 4
    // distinct primer files + 2 deduped primer wires (CLAUDE.md, AGENTS.md) = 10.
    skills_cmd(proj.path(), home.path())
        .arg("--all-tools")
        .assert()
        .success()
        .stdout(predicate::str::contains("skipped 10"));
}

#[test]
fn skills_setup_installs_primer_file_and_wiring() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .arg("--all-tools")
        .assert()
        .success()
        .stdout(predicate::str::contains("primer neohaskell.md"));

    // Primer file co-located with each tool's skills (deduped .agents shared).
    assert!(proj.path().join(".claude/neohaskell.md").exists());
    assert!(proj.path().join(".agents/neohaskell.md").exists());
    assert!(proj.path().join(".kiro/neohaskell.md").exists());
    // Cursor gets an always-apply `.mdc` rule (not an AGENTS.md inline).
    let cursor_primer = std::fs::read_to_string(proj.path().join(".cursor/rules/neohaskell.mdc")).unwrap();
    assert!(cursor_primer.contains("alwaysApply: true"), "cursor primer is a self-activating rule");

    // Claude wires via an `@`-import in CLAUDE.md; the managed markers wrap it.
    let claude_md = std::fs::read_to_string(proj.path().join("CLAUDE.md")).unwrap();
    assert!(claude_md.contains("<!-- BEGIN neohaskell-skills -->"));
    assert!(claude_md.contains("@.claude/neohaskell.md"));
    assert!(claude_md.contains("<!-- END neohaskell-skills -->"));

    // AGENTS.md carries only the inlined primer (no universal skills dump).
    let agents_md = std::fs::read_to_string(proj.path().join("AGENTS.md")).unwrap();
    assert!(!agents_md.contains("BEGIN neo skills"), "no universal skills block");
    assert!(agents_md.contains("<!-- BEGIN neohaskell-skills -->"), "primer block present");
    assert!(agents_md.contains("NeoHaskell primer"), "primer body inlined into AGENTS.md");
}

#[test]
fn skills_setup_no_primer_skips_primer() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .args(["--all-tools", "--no-primer"])
        .assert()
        .success();
    // Skills still install…
    assert!(proj.path().join(".claude/skills/sample-skill/SKILL.md").exists());
    // …but no primer file or CLAUDE.md wiring is written.
    assert!(!proj.path().join(".claude/neohaskell.md").exists());
    assert!(!proj.path().join("CLAUDE.md").exists());
    // AGENTS.md is only ever written to inline the primer; with `--no-primer`
    // (and no universal AGENTS.md skills dump) nothing writes it at all.
    assert!(!proj.path().join("AGENTS.md").exists(), "AGENTS.md not written with --no-primer");
}

#[test]
fn skills_setup_primer_preserves_user_content_outside_block() {
    let (proj, home) = skills_sandbox();
    // Pre-author a CLAUDE.md with user content and no managed block.
    std::fs::write(proj.path().join("CLAUDE.md"), "# My rules\n\nkeep me.\n").unwrap();
    skills_cmd(proj.path(), home.path()).args(["--tool", "claude"]).assert().success();

    let claude_md = std::fs::read_to_string(proj.path().join("CLAUDE.md")).unwrap();
    assert!(claude_md.contains("# My rules"), "user heading preserved");
    assert!(claude_md.contains("keep me."), "user body preserved");
    assert!(claude_md.contains("@.claude/neohaskell.md"), "primer import appended");

    // Re-run is a no-op on the file (idempotent): content unchanged.
    let before = claude_md;
    skills_cmd(proj.path(), home.path()).args(["--tool", "claude"]).assert().success();
    let after = std::fs::read_to_string(proj.path().join("CLAUDE.md")).unwrap();
    assert_eq!(before, after, "second run must not change CLAUDE.md");
}

#[test]
fn skills_setup_dry_run_writes_no_primer() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .args(["--all-tools", "--dry-run"])
        .assert()
        .success()
        .stdout(predicate::str::contains("neohaskell.md"));
    assert!(!proj.path().join(".claude/neohaskell.md").exists(), "dry run writes no primer");
    assert!(!proj.path().join("CLAUDE.md").exists(), "dry run writes no wiring");
}

#[test]
fn skills_setup_overwrite_without_force_refused() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path()).arg("--all-tools").assert().success();
    // Tamper with an installed destination so it no longer matches the source.
    std::fs::write(proj.path().join(".claude/skills/sample-skill/SKILL.md"), "tampered").unwrap();
    skills_cmd(proj.path(), home.path())
        .arg("--all-tools")
        .assert()
        .failure()
        .stderr(predicate::str::contains("already exist"))
        .stderr(predicate::str::contains("--force"));
}

#[test]
fn skills_setup_overwrite_with_force() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path()).arg("--all-tools").assert().success();
    std::fs::write(proj.path().join(".claude/skills/sample-skill/SKILL.md"), "tampered").unwrap();
    skills_cmd(proj.path(), home.path())
        .args(["--all-tools", "--force"])
        .assert()
        .success();
    let restored =
        std::fs::read_to_string(proj.path().join(".claude/skills/sample-skill/SKILL.md")).unwrap();
    assert!(restored.contains("name: sample-skill"), "force must restore from source");
}

#[test]
fn skills_setup_dry_run_writes_nothing() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .args(["--all-tools", "--dry-run"])
        .assert()
        .success()
        .stdout(predicate::str::contains("create"))
        .stdout(predicate::str::contains("dry run"));
    assert!(!proj.path().join(".claude").exists(), "dry run must not write");
    assert!(!proj.path().join("AGENTS.md").exists(), "dry run must not write");
}

#[test]
fn skills_setup_unknown_tool_errors() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .args(["--tool", "bogus"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("bogus"))
        .stderr(predicate::str::contains("not a supported tool"));
}

#[test]
fn skills_setup_unknown_skill_errors() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .args(["--all-tools", "--skill", "does-not-exist"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("does-not-exist"));
}

#[test]
fn skills_setup_ci_no_tool_defaults_all() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("defaulting to --all-tools"));
    assert!(proj.path().join(".claude/skills/sample-skill/SKILL.md").exists());
}

#[test]
fn skills_setup_skill_filter_and_single_tool() {
    let (proj, home) = skills_sandbox();
    skills_cmd(proj.path(), home.path())
        .args(["--tool", "claude", "--skill", "sample-skill"])
        .assert()
        .success();
    assert!(proj.path().join(".claude/skills/sample-skill/SKILL.md").exists());
    // Only the requested tool is installed.
    assert!(!proj.path().join(".agents/skills").exists());
    assert!(!proj.path().join(".cursor").exists());
    assert!(!proj.path().join("AGENTS.md").exists());
}

// ============================================================
// `neo validate` — event-model.json linter (exit-code contract)
//   0 valid · 1 IO failure · 2 invalid · 3 malformed JSON · 4 absent
// ============================================================

const VALID_MODEL: &str = r#"{
  "id": "m1",
  "name": "demo",
  "chapters": [],
  "entities": [],
  "slices": [],
  "nodes": [],
  "edges": [],
  "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
}"#;

fn write_model(dir: &std::path::Path, body: &str) -> std::path::PathBuf {
    let p = dir.join("event-model.json");
    std::fs::write(&p, body).unwrap();
    p
}

#[test]
fn validate_valid_model_exits_0() {
    let temp = tempfile::tempdir().unwrap();
    let path = write_model(temp.path(), VALID_MODEL);
    neo_cmd()
        .arg("validate")
        .arg(&path)
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok] event-model.json is valid"));
}

#[test]
fn validate_invalid_model_exits_2() {
    // Valid JSON, but the required root `id` is missing → schema error.
    let temp = tempfile::tempdir().unwrap();
    let bad = r#"{"name":"demo","chapters":[],"entities":[],"slices":[],"nodes":[],"edges":[],"layout":{"nodePositions":{},"viewport":{"x":0,"y":0,"zoom":1}}}"#;
    let path = write_model(temp.path(), bad);
    neo_cmd()
        .arg("validate")
        .arg(&path)
        .assert()
        .code(2)
        .stdout(predicate::str::contains("[error]"))
        .stdout(predicate::str::contains("[fail]"));
}

#[test]
fn validate_orphan_edge_exits_2() {
    // Schema-valid but an edge references a node id that is not in `nodes`.
    let temp = tempfile::tempdir().unwrap();
    let model = serde_json::json!({
        "id": "m1", "name": "demo",
        "chapters": [], "entities": [], "slices": [],
        "nodes": [{"id": "n1", "type": "event", "name": "Ev", "entityId": null, "sliceId": null}],
        "edges": [{"id": "ed1", "type": "commandProducesEvent", "sourceId": "missing", "targetId": "n1"}],
        "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
    });
    let path = write_model(temp.path(), &serde_json::to_string_pretty(&model).unwrap());
    neo_cmd()
        .arg("validate")
        .arg(&path)
        .assert()
        .code(2)
        // The referential message must carry the actionable fix recipe verbatim.
        .stdout(predicate::str::contains("missing"))
        .stdout(predicate::str::contains("delete this edge"));
}

#[test]
fn validate_malformed_json_exits_3() {
    let temp = tempfile::tempdir().unwrap();
    let path = write_model(temp.path(), "{not json");
    neo_cmd()
        .arg("validate")
        .arg(&path)
        .assert()
        .code(3)
        .stdout(predicate::str::contains("is not valid JSON"));
}

#[test]
fn validate_missing_file_exits_4() {
    // Empty dir, default path → <cwd>/event-model.json does not exist → exit 4.
    let temp = tempfile::tempdir().unwrap();
    neo_cmd()
        .current_dir(temp.path())
        .arg("validate")
        .assert()
        .code(4)
        .stdout(predicate::str::contains("no event-model.json"))
        .stdout(predicate::str::contains("neo ide"));
}

#[test]
fn validate_directory_arg_exits_1() {
    // Path points at a directory → genuine IO failure → miette Err → exit 1.
    let temp = tempfile::tempdir().unwrap();
    neo_cmd()
        .arg("validate")
        .arg(temp.path())
        .assert()
        .code(1)
        .stderr(predicate::str::contains("reading `event-model.json`"));
}

#[test]
fn validate_default_path_uses_cwd() {
    let temp = tempfile::tempdir().unwrap();
    write_model(temp.path(), VALID_MODEL);
    neo_cmd()
        .current_dir(temp.path())
        .arg("validate")
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok]"));
}

#[test]
fn validate_json_flag_pure_json_and_code() {
    let temp = tempfile::tempdir().unwrap();
    let bad = r#"{"name":"demo","chapters":[],"entities":[],"slices":[],"nodes":[],"edges":[],"layout":{"nodePositions":{},"viewport":{"x":0,"y":0,"zoom":1}}}"#;
    let path = write_model(temp.path(), bad);
    let assert = neo_cmd()
        .args(["validate", "--json", "--ci"])
        .arg(&path)
        .assert()
        .code(2);
    let out = assert.get_output();
    let stdout = String::from_utf8_lossy(&out.stdout);
    // stdout is PURE JSON — no human prefixes leaked in.
    assert!(!stdout.contains("[error]"), "json stdout must not contain [error]: {stdout}");
    assert!(!stdout.contains("[fail]"), "json stdout must not contain [fail]: {stdout}");
    let v: serde_json::Value =
        serde_json::from_str(stdout.trim()).expect("stdout must be a single pure-JSON document");
    assert_eq!(v["status"], "invalid");
    assert!(v["errors"].as_array().map(|a| !a.is_empty()).unwrap_or(false));
}

#[test]
fn validate_json_valid_status_exit_0() {
    let temp = tempfile::tempdir().unwrap();
    let path = write_model(temp.path(), VALID_MODEL);
    let assert = neo_cmd()
        .args(["validate", "--json", "--ci"])
        .arg(&path)
        .assert()
        .success();
    let out = assert.get_output();
    let v: serde_json::Value =
        serde_json::from_str(String::from_utf8_lossy(&out.stdout).trim()).unwrap();
    assert_eq!(v["status"], "valid");
}

#[test]
fn validate_is_deterministic() {
    let temp = tempfile::tempdir().unwrap();
    let model = serde_json::json!({
        "id": "m1", "name": "demo",
        "chapters": [], "entities": [], "slices": [],
        "nodes": [{"id": "n1", "type": "event", "name": "Ev", "entityId": "ghost", "sliceId": null}],
        "edges": [],
        "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
    });
    let path = write_model(temp.path(), &serde_json::to_string_pretty(&model).unwrap());
    let run = || {
        let out = neo_cmd().args(["validate", "--ci"]).arg(&path).assert().code(2);
        String::from_utf8_lossy(&out.get_output().stdout).to_string()
    };
    assert_eq!(run(), run(), "validate output must be byte-stable across runs");
}

#[test]
fn validate_does_not_modify_file() {
    // A linter must be read-only. An invalid file is byte-identical after the run.
    let temp = tempfile::tempdir().unwrap();
    let bad = r#"{"name":"demo","chapters":[],"entities":[],"slices":[],"nodes":[],"edges":[],"layout":{"nodePositions":{},"viewport":{"x":0,"y":0,"zoom":1}}}"#;
    let path = write_model(temp.path(), bad);
    let before = std::fs::read(&path).unwrap();
    neo_cmd().args(["validate", "--ci"]).arg(&path).assert().code(2);
    let after = std::fs::read(&path).unwrap();
    assert_eq!(before, after, "validate must not modify the target file");
}
