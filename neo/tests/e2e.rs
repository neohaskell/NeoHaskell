//! End-to-end harness for NeoCLI.
//!
//! Every test in this file shells out to the **nix-built** `result/bin/neo`
//! (NOT a cargo-built binary). All scenarios are gated with `#[ignore]` so
//! `cargo test` stays fast; run the full suite with:
//!
//!     cargo test --test e2e -- --ignored --test-threads=1
//!
//! Prerequisites:
//!   - `nix build` has been run at the repo root (so `result/bin/neo` exists).
//!   - Real network access (this suite intentionally does not stub GitHub).
//!   - Tools available on PATH: `nix`, `git`, `pgrep`, `timeout`
//!     (all present in `nix develop`).
//!
//! Set `NEO_E2E_KEEP=1` to preserve sandbox directories after each test for
//! post-mortem inspection. Failed tests always preserve their sandbox.

mod common;

use common::*;
use predicates::prelude::*;
use std::process::Command as StdCommand;
use std::time::Duration;

// =====================================================
// Group A: CLI surface
// =====================================================

#[test]
#[ignore]
fn surface_version() {
    let sb = Sandbox::new("surface_version");
    sb.neo(".")
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("neo 0.1.0"));
}

#[test]
#[ignore]
fn e2e_inspect_sync_force_from_cli() {
    // The shipped binary must sync node fields from source into
    // event-model.json. No nix/cabal build is needed — the sync only parses
    // `.hs` text + reads/writes JSON — so this is a fast CLI-surface scenario.
    let sb = Sandbox::new("e2e_inspect_sync_force_from_cli");
    let proj = sb.path("proj");
    let write = |rel: &str, body: &str| {
        let p = proj.join(rel);
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
        "event-model.json",
        &serde_json::to_string_pretty(&serde_json::json!({
            "id": "m", "name": "demo", "chapters": [], "entities": [], "slices": [],
            "nodes": [], "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        }))
        .unwrap(),
    );

    sb.neo("proj")
        .args(["inspect", "sync"])
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok] synced event-model.json"));

    let model: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(proj.join("event-model.json")).unwrap()).unwrap();
    let ev = model["nodes"].as_array().unwrap().iter().find(|n| n["name"] == "ItemAdded").expect("ItemAdded node");
    let names: Vec<&str> = ev["fields"].as_array().unwrap().iter().map(|f| f["name"].as_str().unwrap()).collect();
    assert_eq!(names, vec!["stockId", "quantity"], "shipped binary syncs fields from source");
}

#[test]
#[ignore]
fn e2e_validate_missing_then_valid_then_invalid() {
    // Prove the SHIPPED binary's `neo validate` exit-code contract end-to-end.
    // No nix/cabal build needed — validate only reads JSON — so this is a fast
    // CLI-surface scenario like `e2e_inspect_sync_force_from_cli`.
    let sb = Sandbox::new("e2e_validate_missing_then_valid_then_invalid");
    let proj = sb.path("proj");
    std::fs::create_dir_all(&proj).unwrap();
    let model_path = proj.join("event-model.json");

    // 1. Absent model → exit 4 (a missing model is a failure, not a no-op).
    sb.neo("proj")
        .arg("validate")
        .assert()
        .code(4)
        .stdout(predicate::str::contains("no event-model.json"));

    // 2. Valid model → exit 0.
    std::fs::write(
        &model_path,
        serde_json::to_string_pretty(&serde_json::json!({
            "id": "m1", "name": "demo",
            "chapters": [], "entities": [], "slices": [], "nodes": [], "edges": [],
            "layout": { "nodePositions": {}, "viewport": { "x": 0, "y": 0, "zoom": 1 } }
        }))
        .unwrap(),
    )
    .unwrap();
    sb.neo("proj")
        .arg("validate")
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok] event-model.json is valid"));

    // 3. Corrupt the file (unparseable) → exit 3, and the file is left untouched.
    std::fs::write(&model_path, "{ not json").unwrap();
    sb.neo("proj")
        .arg("validate")
        .assert()
        .code(3)
        .stdout(predicate::str::contains("is not valid JSON"));
    assert_eq!(
        std::fs::read_to_string(&model_path).unwrap(),
        "{ not json",
        "validate must not modify the file it lints",
    );
}

#[test]
#[ignore]
fn surface_help_lists_all_subcommands() {
    let sb = Sandbox::new("surface_help_lists_all_subcommands");
    sb.neo(".")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage: neo"))
        .stdout(predicate::str::contains("new"))
        .stdout(predicate::str::contains("build"))
        .stdout(predicate::str::contains("run"))
        .stdout(predicate::str::contains("test"))
        .stdout(predicate::str::contains("lock"))
        .stdout(predicate::str::contains("skills"))
        .stdout(predicate::str::contains("validate"));
}

#[test]
#[ignore]
fn surface_help_subcommand_lock() {
    let sb = Sandbox::new("surface_help_subcommand_lock");
    sb.neo(".")
        .args(["lock", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("install"));
}

#[test]
#[ignore]
fn surface_help_subcommand_skills() {
    let sb = Sandbox::new("surface_help_subcommand_skills");
    sb.neo(".")
        .args(["skills", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("setup"));
    sb.neo(".")
        .args(["skills", "setup", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--all-tools"))
        .stdout(predicate::str::contains("--tool"))
        .stdout(predicate::str::contains("--force"));
}

#[test]
#[ignore]
fn surface_unknown_subcommand_exits_nonzero() {
    let sb = Sandbox::new("surface_unknown_subcommand_exits_nonzero");
    sb.neo(".")
        .arg("bogus-cmd")
        .assert()
        .failure();
}

// =====================================================
// Group B: `neo new` happy paths (real network)
// =====================================================

#[test]
#[ignore]
fn new_ci_creates_full_project() {
    let sb = Sandbox::new("new_ci_creates_full_project");
    sb.neo(".")
        .args(["new", "my-app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();

    let project = sb.path("my-app");
    assert!(project.exists(), "project dir missing");

    let neo_json = read_neo_json(&project);
    assert_eq!(neo_json["name"], "my-app");
    assert_eq!(neo_json["neo-version"], "main");
    for field in ["version", "description", "author", "license"] {
        assert!(
            neo_json.get(field).is_some(),
            "neo.json missing field `{}`",
            field
        );
    }

    assert!(project.join("src/App.hs").exists(), "src/App.hs missing");
    assert!(
        project.join("launcher/Launcher.hs").exists(),
        "launcher/Launcher.hs missing"
    );
    assert!(file_contains(&project.join(".envrc"), "use flake"));
    assert!(file_contains(&project.join(".gitignore"), "!*.cabal"));
    assert!(project.join(".git/HEAD").exists());
    let hook = project.join(".git/hooks/pre-commit");
    assert!(hook.exists(), "pre-commit hook not installed");
    assert!(is_executable(&hook), "pre-commit hook not executable");
    assert!(project.join("my-app.cabal").exists(), ".cabal not generated");
    assert!(project.join("flake.nix").exists(), "flake.nix not generated");
    assert!(
        project.join("cabal.project").exists(),
        "cabal.project not generated"
    );

    // A working Hspec/QuickCheck test-suite ships with every new project. Haskell
    // specs live under `tests/` (alongside `.hurl`); reconcile writes the driver.
    assert!(project.join("tests/Spec.hs").exists(), "tests/Spec.hs (hspec-discover driver) missing");
    let cabal = std::fs::read_to_string(project.join("my-app.cabal")).unwrap();
    assert!(
        cabal.contains("test-suite my-app-test"),
        "generated .cabal missing test-suite stanza:\n{}",
        cabal
    );
    assert!(cabal.contains("main-is: Spec.hs"), "test-suite missing hspec-discover driver:\n{}", cabal);
    assert!(cabal.contains("hs-source-dirs: tests"), "test-suite must source from tests/:\n{}", cabal);
    // cabal.project must enable the suite in the plan so `cabal test all` resolves offline.
    let cabal_project = std::fs::read_to_string(project.join("cabal.project")).unwrap();
    assert!(
        cabal_project.contains("package my-app") && cabal_project.contains("tests: True"),
        "cabal.project must enable tests for the project package:\n{}",
        cabal_project
    );

    let flake = std::fs::read_to_string(project.join("flake.nix")).unwrap();
    assert!(
        !flake.contains("deadbeef"),
        "flake.nix still contains placeholder `deadbeef` SHA — real network path did not run"
    );
    let sha_re = regex_lite_match_40_hex(&flake);
    assert!(
        sha_re,
        "flake.nix does not contain a 40-char hex SHA — real network path did not run"
    );

    let log = sb.git("my-app", &["log", "--oneline"]);
    assert!(log.status.success(), "git log failed");
    let log_out = String::from_utf8_lossy(&log.stdout);
    assert!(
        log_out.contains("Initial commit from NeoCLI"),
        "initial commit missing: {}",
        log_out
    );
}

#[test]
#[ignore]
fn new_ci_default_name_when_omitted() {
    let sb = Sandbox::new("new_ci_default_name_when_omitted");
    sb.neo(".")
        .args(["new", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    assert!(sb.path("my-neo-app/neo.json").exists());
}

#[test]
#[ignore]
fn new_ci_two_sibling_projects_are_independent() {
    let sb = Sandbox::new("new_ci_two_sibling_projects_are_independent");
    sb.neo(".")
        .args(["new", "alpha", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo(".")
        .args(["new", "beta", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    assert!(sb.path("alpha/neo.json").exists());
    assert!(sb.path("beta/neo.json").exists());
    let alpha = read_neo_json(&sb.path("alpha"));
    let beta = read_neo_json(&sb.path("beta"));
    assert_eq!(alpha["name"], "alpha");
    assert_eq!(beta["name"], "beta");
}

// =====================================================
// Group C: `neo new` edge cases
// =====================================================

#[test]
#[ignore]
fn new_existing_dir_fails() {
    let sb = Sandbox::new("new_existing_dir_fails");
    std::fs::create_dir_all(sb.path("foo")).unwrap();
    sb.neo(".")
        .args(["new", "foo", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("already exists"));
}

#[test]
#[ignore]
fn new_existing_dir_with_dotfile_fails() {
    let sb = Sandbox::new("new_existing_dir_with_dotfile_fails");
    std::fs::create_dir_all(sb.path("foo")).unwrap();
    std::fs::write(sb.path("foo/.keep"), "").unwrap();
    sb.neo(".")
        .args(["new", "foo", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("already exists"));
}

#[test]
#[ignore]
fn new_name_with_dashes_accepted() {
    let sb = Sandbox::new("new_name_with_dashes_accepted");
    sb.neo(".")
        .args(["new", "my-app-name", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    let cfg = read_neo_json(&sb.path("my-app-name"));
    assert_eq!(cfg["name"], "my-app-name");
}

#[test]
#[ignore]
fn new_runs_in_nonempty_cwd_without_touching_siblings() {
    let sb = Sandbox::new("new_runs_in_nonempty_cwd_without_touching_siblings");
    std::fs::write(sb.path("sibling.txt"), "untouched").unwrap();
    sb.neo(".")
        .args(["new", "proj", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    let sibling = std::fs::read_to_string(sb.path("sibling.txt")).unwrap();
    assert_eq!(sibling, "untouched");
    assert!(sb.path("proj/neo.json").exists());
}

// =====================================================
// Group D: `neo build` happy paths (REQUIRED — will fail until jose upstream is fixed)
// =====================================================

#[test]
#[ignore]
fn build_ci_succeeds_in_fresh_project() {
    let sb = Sandbox::new("build_ci_succeeds_in_fresh_project");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo("app")
        .args(["build", "--ci"])
        .timeout(Duration::from_secs(1800))
        .assert()
        .success()
        .stdout(predicate::str::contains("Reconciling project artifacts"))
        .stdout(predicate::str::contains("[ok]"));
    assert!(
        sb.path("app/dist-newstyle").exists(),
        "dist-newstyle missing after build"
    );
}

#[test]
#[ignore]
fn build_ci_is_idempotent() {
    let sb = Sandbox::new("build_ci_is_idempotent");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo("app")
        .args(["build", "--ci"])
        .timeout(Duration::from_secs(1800))
        .assert()
        .success();
    sb.neo("app")
        .args(["build", "--ci"])
        .timeout(Duration::from_secs(900))
        .assert()
        .success();
}

#[test]
#[ignore]
fn build_ci_regenerates_cabal_after_neo_json_change() {
    let sb = Sandbox::new("build_ci_regenerates_cabal_after_neo_json_change");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo("app")
        .args(["build", "--ci"])
        .timeout(Duration::from_secs(1800))
        .assert()
        .success();
    let cabal = sb.path("app/app.cabal");
    let mtime_before = std::fs::metadata(&cabal).unwrap().modified().unwrap();

    std::thread::sleep(Duration::from_secs(2));
    let mut cfg = read_neo_json(&sb.path("app"));
    cfg["description"] = serde_json::Value::String("Updated description".into());
    std::fs::write(
        sb.path("app/neo.json"),
        serde_json::to_string_pretty(&cfg).unwrap(),
    )
    .unwrap();

    sb.neo("app")
        .args(["build", "--ci"])
        .timeout(Duration::from_secs(900))
        .assert()
        .success();
    let mtime_after = std::fs::metadata(&cabal).unwrap().modified().unwrap();
    assert!(
        mtime_after > mtime_before,
        ".cabal was not regenerated after neo.json change"
    );
}

// =====================================================
// Group E: `neo build` edge cases
// =====================================================

#[test]
#[ignore]
fn build_no_neo_json_fails_with_noworkspace() {
    let sb = Sandbox::new("build_no_neo_json_fails_with_noworkspace");
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("No `neo.json` found"));
}

#[test]
#[ignore]
fn build_invalid_neo_json_fails_with_parseerror() {
    let sb = Sandbox::new("build_invalid_neo_json_fails_with_parseerror");
    std::fs::write(sb.path("neo.json"), "{ \"name\": \"x\" ").unwrap();
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Failed to parse `neo.json`"))
        // GraphicalReportHandler renders a source-pointer block with the
        // filename + caret + label. assert_cmd captures stderr (non-TTY) so
        // we get the ASCII fallback `,-[neo.json:line:col]`.
        .stderr(predicate::str::contains("neo.json:").and(
            predicate::str::contains("syntax error here")
        ));
}

#[test]
#[ignore]
fn build_watch_in_ci_fails_fast() {
    let sb = Sandbox::new("build_watch_in_ci_fails_fast");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo("app")
        .args(["build", "--watch", "--ci"])
        .timeout(Duration::from_secs(30))
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Watch mode is not supported in CI mode",
        ));
}

#[test]
#[ignore]
fn build_without_nix_on_path_fails_with_nixmissing() {
    let sb = Sandbox::new("build_without_nix_on_path_fails_with_nixmissing");
    // Skip if a nix binary still resolves under a minimal PATH (e.g. /nix profile symlinks).
    if StdCommand::new("which")
        .arg("nix")
        .env_clear()
        .env("PATH", "/usr/bin:/bin")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("skipping: nix is reachable from a stripped PATH on this host");
        return;
    }
    std::fs::write(
        sb.path("neo.json"),
        r#"{"name":"app","version":"0.1.0","neo-version":"main","description":"x","author":"x","license":"MIT"}"#,
    )
    .unwrap();
    sb.neo(".")
        .args(["build", "--ci"])
        .env("PATH", "/usr/bin:/bin")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Nix is required but not found"));
}

// =====================================================
// Group E.2: `neo.json` dependency grammar (input validation)
// =====================================================

fn write_neo_json_with_deps(sb: &Sandbox, name: &str, deps_json: &str) {
    let neo_json = format!(
        "{{\n  \"name\": \"{}\",\n  \"version\": \"0.1.0\",\n  \"neo-version\": \"main\",\n  \"license\": \"MIT\",\n  \"dependencies\": {}\n}}\n",
        name, deps_json
    );
    std::fs::create_dir_all(sb.path("src")).unwrap();
    std::fs::write(sb.path("src/App.hs"), "module App where\n").unwrap();
    std::fs::create_dir_all(sb.path("launcher")).unwrap();
    std::fs::write(
        sb.path("launcher/Launcher.hs"),
        "module Main where\nmain :: IO ()\nmain = pure ()\n",
    )
    .unwrap();
    std::fs::write(sb.path("neo.json"), neo_json).unwrap();
}

#[test]
#[ignore]
fn e2e_build_invalid_semver_errors() {
    let sb = Sandbox::new("e2e_build_invalid_semver_errors");
    write_neo_json_with_deps(&sb, "p", r#"{"foo":"not-a-version"}"#);
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid dependency"));
}

#[test]
#[ignore]
fn e2e_build_unknown_protocol_errors() {
    let sb = Sandbox::new("e2e_build_unknown_protocol_errors");
    write_neo_json_with_deps(&sb, "p", r#"{"foo":"npm:bar"}"#);
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("unknown protocol"));
}

#[test]
#[ignore]
fn e2e_build_conflicting_protocols_errors() {
    let sb = Sandbox::new("e2e_build_conflicting_protocols_errors");
    write_neo_json_with_deps(&sb, "p", r#"{"hackage:foo":"git:host/r.git"}"#);
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("both key and value"));
}

// =====================================================
// Group F: `neo run`
// =====================================================

#[test]
#[ignore]
fn run_ci_completes_or_runs_for_fresh_starter() {
    let sb = Sandbox::new("run_ci_completes_or_runs_for_fresh_starter");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();

    // Wrap with `timeout(1)` from coreutils so a server-style starter doesn't block forever.
    // We accept either clean exit OR SIGTERM-by-timeout, but require that we got far enough
    // to see "Running project..." in stdout.
    let neo = neo_bin();
    let project = sb.path("app");
    let mut path = std::env::var("PATH").unwrap_or_default();
    path = format!("{}:{}", neo.parent().unwrap().display(), path);

    let out = StdCommand::new("timeout")
        .args(["--signal=TERM", "180", neo.to_str().unwrap(), "run", "--ci"])
        .current_dir(&project)
        .env("HOME", &sb.home)
        .env("PATH", &path)
        .output()
        .expect("spawn timeout+neo failed");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = out.status.code().unwrap_or(-1);

    assert!(
        stdout.contains("Running project..."),
        "expected `Running project...` in stdout; got stdout=`{}` stderr=`{}` code={}",
        stdout, stderr, code
    );

    // Acceptable: exit 0 (finite program), 124 (timeout killed it — server-style starter).
    assert!(
        out.status.success() || code == 124,
        "unexpected exit code {} (stderr: {})", code, stderr
    );
}

#[test]
#[ignore]
fn run_watch_in_ci_fails_fast() {
    let sb = Sandbox::new("run_watch_in_ci_fails_fast");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo("app")
        .args(["run", "--watch", "--ci"])
        .timeout(Duration::from_secs(30))
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Watch mode is not supported in CI mode",
        ));
}

// =====================================================
// Group G: `neo test`
// =====================================================

#[test]
#[ignore]
fn test_ci_no_hurl_runs_unit_only() {
    let sb = Sandbox::new("test_ci_no_hurl_runs_unit_only");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    // This scenario exercises the no-Hurl branch of `neo test` (the "No Hurl
    // integration tests found" path), so it needs a project with no `.hurl` files.
    // Haskell specs and Hurl share `tests/`, so remove only the starter's Hurl
    // subdirectories — the Haskell suite (tests/Spec.hs + specs) must remain and run.
    // The Hurl-present path is covered by `test_ci_with_hurl_discovers_and_runs_integration`
    // and the full unit+Hurl flow by integration's `test_neo_test_ci`.
    std::fs::remove_dir_all(sb.path("app/tests/integration")).ok();
    std::fs::remove_dir_all(sb.path("app/tests/scenarios")).ok();
    let assert = sb
        .neo("app")
        .args(["test", "--ci"])
        .timeout(Duration::from_secs(1800))
        .assert()
        .success()
        .stdout(predicate::str::contains("Running unit tests"))
        .stdout(predicate::str::contains("Unit tests passed"))
        .stdout(predicate::str::contains("No Hurl integration tests found"));
    // Prove the scaffolded Hspec suite actually compiled AND executed the example —
    // not a vacuous 0-spec run. `neo test` passes `--test-show-details=direct`, so
    // hspec's own summary streams through; check both streams (cabal splits output).
    let out = assert.get_output();
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        combined.contains("1 example, 0 failures"),
        "expected hspec to discover and run exactly the scaffolded ExampleSpec; combined output=`{}`",
        combined
    );
}

#[test]
#[ignore]
fn test_ci_with_hurl_discovers_and_runs_integration() {
    let sb = Sandbox::new("test_ci_with_hurl_discovers_and_runs_integration");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    std::fs::create_dir_all(sb.path("app/tests")).unwrap();
    std::fs::write(
        sb.path("app/tests/smoke.hurl"),
        "GET http://localhost:8080/\nHTTP *\n",
    )
    .unwrap();
    let assert = sb
        .neo("app")
        .args(["test", "--ci"])
        .timeout(Duration::from_secs(1800))
        .assert();
    let out = assert.get_output();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("Hurl integration tests"),
        "expected Hurl discovery message; stdout=`{}`",
        stdout
    );
}

#[test]
#[ignore]
fn test_ci_no_zombie_process_after_test() {
    let sb = Sandbox::new("test_ci_no_zombie_process_after_test");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    let _ = sb
        .neo("app")
        .args(["test", "--ci"])
        .timeout(Duration::from_secs(1800))
        .ok();
    // Give the OS a moment to reap.
    std::thread::sleep(Duration::from_secs(2));
    let leaks = count_processes_in(&sb.root);
    assert_eq!(leaks, 0, "found leaked processes in sandbox after test");
}

#[test]
#[ignore]
fn test_no_neo_json_fails() {
    let sb = Sandbox::new("test_no_neo_json_fails");
    sb.neo(".")
        .args(["test", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("No `neo.json` found"));
}

#[test]
#[ignore]
fn test_with_malformed_hurl_exits_nonzero() {
    let sb = Sandbox::new("test_with_malformed_hurl_exits_nonzero");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    std::fs::create_dir_all(sb.path("app/tests")).unwrap();
    std::fs::write(sb.path("app/tests/bad.hurl"), "this is not valid hurl").unwrap();
    sb.neo("app")
        .args(["test", "--ci"])
        .timeout(Duration::from_secs(1800))
        .assert()
        .failure();
}

#[test]
#[ignore]
fn test_watch_in_ci_fails_fast() {
    let sb = Sandbox::new("test_watch_in_ci_fails_fast");
    sb.neo(".")
        .args(["new", "app", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
    sb.neo("app")
        .args(["test", "--watch", "--ci"])
        .timeout(Duration::from_secs(30))
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Watch mode is not supported in CI mode",
        ));
}

// =====================================================
// Group I: `neo lock` happy paths
// =====================================================

fn scaffold_project_for_lock(sb: &Sandbox, name: &str) {
    sb.neo(".")
        .args(["new", name, "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
}

fn write_domain_file(sb: &Sandbox, project: &str, rel: &str, body: &str) {
    let full = sb.path(project).join(rel);
    std::fs::create_dir_all(full.parent().unwrap()).unwrap();
    std::fs::write(full, body).unwrap();
}

#[test]
#[ignore]
fn lock_install_writes_executable_hook() {
    let sb = Sandbox::new("lock_install_writes_executable_hook");
    scaffold_project_for_lock(&sb, "app");
    sb.neo("app")
        .args(["lock", "install", "--ci"])
        .assert()
        .success();
    let hook = sb.path("app/.git/hooks/pre-commit");
    assert!(hook.exists());
    assert!(is_executable(&hook));
    let body = std::fs::read_to_string(&hook).unwrap();
    assert!(body.contains("neo lock check"));
}

#[test]
#[ignore]
fn lock_single_match_locks_file() {
    let sb = Sandbox::new("lock_single_match_locks_file");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(
        &sb,
        "app",
        "src/Domain/Commands/CreateUser.hs",
        "module Domain.Commands.CreateUser where\n",
    );
    // Stage so lock's stage_and_commit can include it cleanly.
    let _ = sb.git("app", &["add", "."]);
    let _ = sb.git("app", &["commit", "--no-verify", "-m", "add domain file"]);

    sb.neo("app")
        .args(["lock", "CreateUser", "--ci"])
        .assert()
        .success();
    let manifest = std::fs::read_to_string(sb.path("app/.locked-files")).unwrap();
    assert!(
        manifest.contains("CreateUser.hs"),
        "manifest missing CreateUser.hs: {}",
        manifest
    );
}

#[test]
#[ignore]
fn lock_all_locks_every_domain_file() {
    let sb = Sandbox::new("lock_all_locks_every_domain_file");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(&sb, "app", "src/Domain/Commands/A.hs", "module Domain.Commands.A where\n");
    write_domain_file(&sb, "app", "src/Domain/Events/B.hs", "module Domain.Events.B where\n");
    write_domain_file(&sb, "app", "src/Domain/Queries/C.hs", "module Domain.Queries.C where\n");
    let _ = sb.git("app", &["add", "."]);
    let _ = sb.git("app", &["commit", "--no-verify", "-m", "domain"]);

    sb.neo("app")
        .args(["lock", "--all", "--ci"])
        .assert()
        .success();
    let manifest = std::fs::read_to_string(sb.path("app/.locked-files")).unwrap();
    for needle in ["A.hs", "B.hs", "C.hs"] {
        assert!(
            manifest.contains(needle),
            "manifest missing {}: {}",
            needle,
            manifest
        );
    }
}

#[test]
#[ignore]
fn lock_check_passes_when_nothing_staged() {
    let sb = Sandbox::new("lock_check_passes_when_nothing_staged");
    scaffold_project_for_lock(&sb, "app");
    sb.neo("app")
        .args(["lock", "check"])
        .assert()
        .success();
}

#[test]
#[ignore]
fn lock_check_fails_when_locked_file_staged() {
    let sb = Sandbox::new("lock_check_fails_when_locked_file_staged");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(
        &sb,
        "app",
        "src/Domain/Commands/Locked.hs",
        "module Domain.Commands.Locked where\n",
    );
    let _ = sb.git("app", &["add", "."]);
    let _ = sb.git("app", &["commit", "--no-verify", "-m", "add"]);
    sb.neo("app")
        .args(["lock", "Locked", "--ci"])
        .assert()
        .success();

    // Modify and stage the locked file.
    std::fs::write(
        sb.path("app/src/Domain/Commands/Locked.hs"),
        "module Domain.Commands.Locked where\n-- changed\n",
    )
    .unwrap();
    let _ = sb.git("app", &["add", "src/Domain/Commands/Locked.hs"]);

    sb.neo("app")
        .args(["lock", "check"])
        .assert()
        .failure();
}

#[test]
#[ignore]
fn lock_check_handles_missing_manifest() {
    let sb = Sandbox::new("lock_check_handles_missing_manifest");
    scaffold_project_for_lock(&sb, "app");
    assert!(!sb.path("app/.locked-files").exists());
    sb.neo("app")
        .args(["lock", "check"])
        .assert()
        .success();
}

// =====================================================
// Group J: `neo lock` edge cases
// =====================================================

#[test]
#[ignore]
fn lock_no_domain_files_prints_message() {
    let sb = Sandbox::new("lock_no_domain_files_prints_message");
    scaffold_project_for_lock(&sb, "app");
    sb.neo("app")
        .args(["lock", "--all", "--ci"])
        .assert()
        .success()
        .stdout(predicate::str::contains("No domain files found"));
}

#[test]
#[ignore]
fn lock_ambiguous_in_ci_does_not_lock() {
    let sb = Sandbox::new("lock_ambiguous_in_ci_does_not_lock");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(&sb, "app", "src/Domain/Commands/CreateUser.hs", "");
    write_domain_file(&sb, "app", "src/Domain/Events/UserCreated.hs", "");
    let _ = sb.git("app", &["add", "."]);
    let _ = sb.git("app", &["commit", "--no-verify", "-m", "x"]);

    sb.neo("app")
        .args(["lock", "Create", "--ci"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Multiple matches"));
    assert!(
        !sb.path("app/.locked-files").exists(),
        "manifest should not have been written for ambiguous match"
    );
}

#[test]
#[ignore]
fn lock_no_matches_prints_message() {
    let sb = Sandbox::new("lock_no_matches_prints_message");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(&sb, "app", "src/Domain/Commands/User.hs", "");
    sb.neo("app")
        .args(["lock", "zzzz-no-such-thing", "--ci"])
        .assert()
        .success()
        .stdout(predicate::str::contains("No matches"));
}

#[test]
#[ignore]
fn lock_hook_blocks_real_git_commit() {
    let sb = Sandbox::new("lock_hook_blocks_real_git_commit");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(
        &sb,
        "app",
        "src/Domain/Commands/Block.hs",
        "module Domain.Commands.Block where\n",
    );
    let _ = sb.git("app", &["add", "."]);
    let _ = sb.git("app", &["commit", "--no-verify", "-m", "add"]);
    sb.neo("app")
        .args(["lock", "Block", "--ci"])
        .assert()
        .success();

    // Now try to modify and commit the locked file via the real hook.
    std::fs::write(
        sb.path("app/src/Domain/Commands/Block.hs"),
        "module Domain.Commands.Block where\n-- tampered\n",
    )
    .unwrap();
    let _ = sb.git("app", &["add", "src/Domain/Commands/Block.hs"]);
    let out = sb.git("app", &["commit", "-m", "should be blocked"]);
    assert!(
        !out.status.success(),
        "expected commit to be blocked, got success. stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
#[ignore]
fn lock_hook_allows_non_locked_commit() {
    let sb = Sandbox::new("lock_hook_allows_non_locked_commit");
    scaffold_project_for_lock(&sb, "app");
    write_domain_file(
        &sb,
        "app",
        "src/Domain/Commands/Block.hs",
        "module Domain.Commands.Block where\n",
    );
    let _ = sb.git("app", &["add", "."]);
    let _ = sb.git("app", &["commit", "--no-verify", "-m", "add"]);
    sb.neo("app")
        .args(["lock", "Block", "--ci"])
        .assert()
        .success();

    // Touch an unrelated file and commit through the hook.
    std::fs::write(sb.path("app/README.md"), "hi\n").unwrap();
    let _ = sb.git("app", &["add", "README.md"]);
    let out = sb.git("app", &["commit", "-m", "unrelated change"]);
    assert!(
        out.status.success(),
        "expected commit to succeed, got failure. stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
#[ignore]
fn lock_install_overwrites_existing_hook() {
    let sb = Sandbox::new("lock_install_overwrites_existing_hook");
    scaffold_project_for_lock(&sb, "app");
    let hook = sb.path("app/.git/hooks/pre-commit");
    std::fs::write(&hook, "#!/bin/sh\necho bogus\n").unwrap();
    sb.neo("app")
        .args(["lock", "install", "--ci"])
        .assert()
        .success();
    let body = std::fs::read_to_string(&hook).unwrap();
    assert!(body.contains("neo lock check"));
    assert!(!body.contains("echo bogus"));
}

// =====================================================
// Group K: environment + global flags
// =====================================================

#[test]
#[ignore]
fn env_ci_var_disables_tui() {
    let sb = Sandbox::new("env_ci_var_disables_tui");
    let mut cmd = sb.neo(".");
    cmd.env("CI", "1");
    let out = cmd
        .args(["new", "envapp"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success()
        .get_output()
        .clone();
    let stdout = String::from_utf8_lossy(&out.stdout);
    // No ANSI escape sequences leaked from a TUI render.
    assert!(
        !stdout.contains("\x1b["),
        "CI=1 produced ANSI escape codes; stdout=`{}`",
        stdout
    );
    // CI prefixes show up.
    assert!(
        stdout.contains("[info]") || stdout.contains("[ok]"),
        "expected [info]/[ok] prefix; stdout=`{}`",
        stdout
    );
}

#[test]
#[ignore]
fn flag_verbose_accepted_no_op() {
    let sb = Sandbox::new("flag_verbose_accepted_no_op");
    sb.neo(".")
        .args(["--verbose", "new", "vrb", "--ci"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success();
}

// =====================================================
// Group L: error display format
// =====================================================

#[test]
#[ignore]
fn error_no_workspace_has_help_text() {
    let sb = Sandbox::new("error_no_workspace_has_help_text");
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("No `neo.json` found"))
        .stderr(predicate::str::contains("`neo new`"));
}

#[test]
#[ignore]
fn error_directory_exists_has_help_text() {
    let sb = Sandbox::new("error_directory_exists_has_help_text");
    std::fs::create_dir_all(sb.path("dupe")).unwrap();
    sb.neo(".")
        .args(["new", "dupe", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("already exists"))
        .stderr(predicate::str::contains("Choose a different name"));
}

#[test]
#[ignore]
fn error_invalid_config_mentions_neo_json() {
    let sb = Sandbox::new("error_invalid_config_mentions_neo_json");
    std::fs::write(sb.path("neo.json"), "{ \"name\": \"x\" ").unwrap();
    sb.neo(".")
        .args(["build", "--ci"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("neo.json"));
}

// =====================================================
// Group ? — neo ide JSON-RPC against the release artifact
// =====================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[ignore]
async fn ide_initialize_against_release_binary() {
    use futures_util::{SinkExt, StreamExt};
    use serde_json::json;
    use std::process::Stdio;
    use std::time::Duration;
    use tokio_tungstenite::tungstenite::Message;

    let sb = Sandbox::new("ide_initialize_against_release_binary");
    std::fs::write(
        sb.path("neo.json"),
        r#"{"name":"e2e-ide","version":"0.0.1","neo-version":"0.1.0"}"#,
    )
    .unwrap();

    // Reserve a port (drop-before-spawn, tiny race window).
    let probe = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
    let port = probe.local_addr().unwrap().port();
    drop(probe);

    let bin = neo_bin();
    let mut child = std::process::Command::new(&bin)
        .current_dir(&sb.root)
        .env("HOME", &sb.home)
        .arg("--ci")
        .arg("ide")
        .arg("--port")
        .arg(port.to_string())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn nix-built neo ide");

    // Poll until the server is listening, then connect and `initialize`.
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    while std::time::Instant::now() < deadline {
        if std::net::TcpStream::connect(("127.0.0.1", port)).is_ok() {
            break;
        }
        std::thread::sleep(Duration::from_millis(50));
    }

    let url = format!("ws://127.0.0.1:{port}/ws");
    let (mut ws, _) = tokio_tungstenite::connect_async(&url).await.expect("connect");
    ws.send(Message::Text(
        json!({"jsonrpc":"2.0","id":1,"method":"initialize",
               "params":{"clientInfo":{"name":"e2e","version":"0"}}})
        .to_string(),
    ))
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

    assert_eq!(resp["result"]["serverInfo"]["name"], "neo");
    assert_eq!(resp["result"]["workspace"]["project"]["name"], "e2e-ide");
    assert_eq!(resp["result"]["workspace"]["project"]["neoVersion"], "0.1.0");

    let _ = child.kill();
    let _ = child.wait();
}

// =====================================================
// Group M: `neo skills setup` (real clone of neohaskell/skills)
// =====================================================

#[test]
#[ignore]
fn skills_setup_real_clone_installs_skills_and_primer() {
    // The shipped binary must really clone github.com/neohaskell/skills into the
    // per-user cache (under the sandbox HOME) and install what it finds. Upstream
    // now ships a populated `skills/` tree plus a top-level `neohaskell.md` primer,
    // so a real install is the correct outcome.
    let sb = Sandbox::new("skills_setup_real_clone_installs_skills_and_primer");
    sb.neo("proj")
        .args(["skills", "setup", "--ci", "--all-tools"])
        .timeout(Duration::from_secs(120))
        .assert()
        .success()
        .stdout(predicate::str::contains("[ok] installed"));

    // A real clone landed in the cache.
    let checkout = sb.path("home/.neo/skills-cache/neohaskell-skills");
    assert!(checkout.exists(), "expected the skills repo to be cloned into the cache");

    // At least one skill folder was installed for Claude.
    assert!(sb.path("proj/.claude/skills").is_dir(), "skills installed for claude");

    // The primer shipped upstream → installed next to the skills and wired into
    // CLAUDE.md via an `@`-import inside the managed block.
    assert!(sb.path("proj/.claude/neohaskell.md").exists(), "primer file installed");
    let claude_md = std::fs::read_to_string(sb.path("proj/CLAUDE.md")).unwrap();
    assert!(claude_md.contains("<!-- BEGIN neohaskell-skills -->"), "primer block in CLAUDE.md");
    assert!(claude_md.contains("@.claude/neohaskell.md"), "primer @import wired");
}

// =====================================================
// Local helpers
// =====================================================

/// Returns true iff `text` contains at least one 40-char lowercase hex string.
fn regex_lite_match_40_hex(text: &str) -> bool {
    let bytes = text.as_bytes();
    let mut run = 0usize;
    for &b in bytes {
        let is_hex = matches!(b, b'0'..=b'9' | b'a'..=b'f');
        if is_hex {
            run += 1;
            if run >= 40 {
                return true;
            }
        } else {
            run = 0;
        }
    }
    false
}
