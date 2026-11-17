use std::path::Path;
use std::process::{Command, Output};
use tempfile::tempdir;

fn run_installer(arguments: &[&str], empty_path: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_neo-installer"))
        .args(arguments)
        .env_clear()
        .env("PATH", empty_path)
        .output()
        .expect("run the native installer binary")
}

fn run_installer_with_home(arguments: &[&str], path: &Path, home: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_neo-installer"))
        .args(arguments)
        .env_clear()
        .env("PATH", path)
        .env("HOME", home)
        .output()
        .expect("run the native installer binary")
}

#[test]
fn help_and_version_work_without_nix_or_user_environment() {
    let empty_path = tempdir().expect("create an empty PATH directory");
    let help = run_installer(&["--help"], empty_path.path());
    assert!(help.status.success());
    let help = String::from_utf8(help.stdout).expect("help uses UTF-8");
    assert!(help.contains("NeoHaskell native installer"));
    assert!(help.contains("official Nix 2.34.9 Linux ARM64 daemon"));
    assert!(
        help.contains("New acquisition is blocked until configuration ownership is established.")
    );
    assert!(help.contains("This build does not install, configure, or repair Nix."));
    assert!(help.contains("neo-installer install [--version VERSION]"));
    assert!(help.contains("neo-installer update [--version VERSION | --rollback]"));

    let version = run_installer(&["--version"], empty_path.path());
    assert!(version.status.success());
    let version = String::from_utf8(version.stdout).expect("version uses UTF-8");
    let product_manifest: toml::Value =
        toml::from_str(include_str!("../../Cargo.toml")).expect("parse the parent manifest");
    let product_version = product_manifest["package"]["version"]
        .as_str()
        .expect("parent manifest defines package.version");
    assert!(version.starts_with(&format!("neo-installer {product_version} (")));
    assert!(version.contains(env!("NEO_BUILD_TARGET")));
    assert!(!version.contains("0.0.0"));
}

#[test]
fn unknown_arguments_are_rejected_without_user_state() {
    let empty_path = tempdir().expect("create an empty PATH directory");
    let output = run_installer(&["--unknown"], empty_path.path());
    assert_eq!(output.status.code(), Some(2));
    assert!(
        String::from_utf8(output.stderr)
            .expect("error uses UTF-8")
            .contains("Unknown arguments.")
    );
}

#[test]
fn internal_worker_requires_a_lease_before_any_public_cli_setup() {
    let empty_path = tempdir().unwrap();
    let output = run_installer(&["--internal-operation-worker"], empty_path.path());
    assert_eq!(output.status.code(), Some(125));
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
}

#[test]
fn lifecycle_requires_an_explicit_safe_home_without_touching_host_state() {
    let empty_path = tempdir().unwrap();
    let output = run_installer(&["install"], empty_path.path());
    assert_eq!(output.status.code(), Some(1));
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains("The lifecycle operation did not complete.")
    );
}

#[test]
fn update_rollback_is_accepted_before_safe_home_validation() {
    let empty_path = tempdir().unwrap();
    let sandbox = tempdir().unwrap();
    let home = sandbox.path().join("missing-home");
    let output = run_installer_with_home(&["update", "--rollback"], empty_path.path(), &home);
    assert_eq!(output.status.code(), Some(1));
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains("The lifecycle operation did not complete.")
    );
    assert!(!home.exists());
}

#[test]
fn rollback_rejects_conflicting_version_and_mixed_arguments() {
    let empty_path = tempdir().unwrap();
    let invalid = [
        &["update", "--version", "0.17.1", "--rollback"][..],
        &["update", "--rollback", "--version", "0.17.1"][..],
        &["rollback", "--version", "0.17.1"][..],
        &["install", "--rollback"][..],
        &["install", "--version", "0.17.1", "--rollback"][..],
    ];
    for arguments in invalid {
        let sandbox = tempdir().unwrap();
        let home = sandbox.path().join("missing-home");
        let output = run_installer_with_home(arguments, empty_path.path(), &home);
        assert_eq!(output.status.code(), Some(2), "{arguments:?}");
        assert!(
            String::from_utf8(output.stderr)
                .unwrap()
                .contains("Use install or update with an optional --version VERSION"),
            "{arguments:?}"
        );
        assert!(!home.exists(), "{arguments:?}");
    }
}
