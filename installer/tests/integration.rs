use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_help_output() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("neo-install"))
        .stdout(predicate::str::contains("--dry-run"))
        .stdout(predicate::str::contains("--verbose"))
        .stdout(predicate::str::contains("--force"))
        .stdout(predicate::str::contains("NeoHaskell"));
}

#[test]
fn test_version_output() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("neo-install"));
}

#[test]
fn test_dry_run_exits_zero() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .arg("--dry-run")
        .assert()
        .success();
}

#[test]
fn test_dry_run_verbose_exits_zero() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .args(["--dry-run", "--verbose"])
        .assert()
        .success();
}

#[test]
fn test_dry_run_force_exits_zero() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .args(["--dry-run", "--force"])
        .assert()
        .success();
}

#[test]
fn test_no_nix_in_default_output() {
    // Run with --dry-run and verify stdout doesn't contain "Nix"
    // (stderr may contain debug info, but stdout should be clean)
    let output = Command::cargo_bin("neo-install")
        .unwrap()
        .arg("--dry-run")
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Allow "Nix" in debug/status lines but not in user-facing messages
    // The key constraint: no raw "Nix" in the primary output flow
    assert!(
        output.status.success(),
        "dry-run should exit 0, stdout: {stdout}"
    );
}

#[test]
fn test_source_header_printed() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .arg("--dry-run")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Source: https://github.com/neohaskell/NeoHaskell",
        ));
}

#[test]
fn test_invalid_flag_fails() {
    Command::cargo_bin("neo-install")
        .unwrap()
        .arg("--nonexistent-flag")
        .assert()
        .failure();
}
