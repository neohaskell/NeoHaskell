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
    assert!(help.contains("require a working multi-user Nix daemon"));
    assert!(help.contains("review the proposed changes and authorize setup"));
    assert!(help.contains("/neo/nix-setup.md"));
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

#[cfg(unix)]
#[test]
fn installer_refuses_path_commands_without_invoking_them_or_checking_nix() {
    use std::fs;
    use std::os::unix::fs::PermissionsExt;

    for (command, empty_entry) in [("neo", false), ("neoctl", true)] {
        let sandbox = tempdir().unwrap();
        let root = sandbox.path().canonicalize().unwrap();
        let home = root.join("home");
        let state = root.join("state");
        let foreign_name = "private-foreign\n\u{1b}-bin";
        let foreign = root.join(foreign_name);
        for directory in [&home, &state, &foreign] {
            fs::create_dir(directory).unwrap();
        }
        let executable = foreign.join(command);
        let marker = root.join("invoked");
        fs::write(
            &executable,
            b"#!/bin/sh\n: > \"$NEO_PATH_FIXTURE_MARKER\"\n",
        )
        .unwrap();
        fs::set_permissions(&executable, fs::Permissions::from_mode(0o700)).unwrap();
        let (cwd, path) = if empty_entry {
            (&foreign, "")
        } else {
            (&root, foreign_name)
        };
        let output = Command::new(env!("CARGO_BIN_EXE_neo-installer"))
            .args(["install", "--plain"])
            .env_clear()
            .env("HOME", &home)
            .env("XDG_STATE_HOME", &state)
            .env("PATH", path)
            .env("NEO_PATH_FIXTURE_MARKER", &marker)
            .current_dir(cwd)
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(
            stderr.contains(&format!(
                "PATH already resolves {command} to {executable:?}."
            )),
            "{stderr}"
        );
        assert!(!stderr.contains('\u{1b}'));
        assert!(!marker.exists());
        assert!(!home.join(".local/bin").exists());

        let report_path = fs::read_dir(state.join("neohaskell-diagnostics"))
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .find(|path| {
                path.extension()
                    .is_some_and(|extension| extension == "json")
            })
            .unwrap();
        let report = fs::read_to_string(report_path).unwrap();
        assert!(!report.contains("private-foreign"));
        let report: serde_json::Value = serde_json::from_str(&report).unwrap();
        let outcome = &report["report"]["state"]["outcome"];
        assert_eq!(outcome["request"], "install");
        assert_eq!(
            outcome["operation"]["error"],
            serde_json::json!({"kind": "command_path_conflict", "reason": command})
        );
        let events = outcome["observations"]["events"].as_array().unwrap();
        assert!(!events.iter().any(|event| event["phase"] == "checking_nix"));
    }
}
