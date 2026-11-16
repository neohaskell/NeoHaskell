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

#[test]
fn help_and_version_work_without_nix_or_user_environment() {
    let empty_path = tempdir().expect("create an empty PATH directory");
    let help = run_installer(&["--help"], empty_path.path());
    assert!(help.status.success());
    let help = String::from_utf8(help.stdout).expect("help uses UTF-8");
    assert!(help.contains("NeoHaskell native installer"));
    assert!(help.contains("Lifecycle operations are not available in this build."));
    assert!(!help.contains("Usage: neo-installer install"));

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
fn unavailable_mutating_arguments_are_not_advertised_or_run() {
    let empty_path = tempdir().expect("create an empty PATH directory");
    let output = run_installer(&["install"], empty_path.path());
    assert_eq!(output.status.code(), Some(2));
    assert!(
        String::from_utf8(output.stderr)
            .expect("error uses UTF-8")
            .contains("This build supports only --help and --version.")
    );
}
