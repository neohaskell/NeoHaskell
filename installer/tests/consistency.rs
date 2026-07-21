//! Consistency tests: the `curl | sh` bootstrap script (`scripts/bootstrap.sh`)
//! must download release assets from the same repository, and under the same
//! asset-naming scheme, that the monorepo release workflow
//! (`.github/workflows/installer-ci.yml`) actually publishes them under.
//!
//! A mismatch means a real bootstrap 404s against real releases, which no
//! amount of unit testing of the binary would catch. These tests read the two
//! sources of truth off disk and assert they agree.

use std::fs;
use std::path::PathBuf;

/// The installer crate root (`installer/`).
fn crate_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn read(rel: &str) -> String {
    let p = crate_dir().join(rel);
    fs::read_to_string(&p).unwrap_or_else(|e| panic!("failed to read {}: {e}", p.display()))
}

/// The monorepo slug (`owner/repo`) derived from the crate's declared
/// `repository` — the single source of truth for "where releases live".
fn monorepo_slug() -> String {
    let url = env!("CARGO_PKG_REPOSITORY"); // https://github.com/neohaskell/NeoHaskell
    url.trim_end_matches('/')
        .trim_end_matches(".git")
        .rsplit("github.com/")
        .next()
        .expect("CARGO_PKG_REPOSITORY should contain 'github.com/'")
        .to_string()
}

/// The `REPO="owner/repo"` value declared in bootstrap.sh.
fn bootstrap_repo(bootstrap: &str) -> String {
    let line = bootstrap
        .lines()
        .find(|l| l.trim_start().starts_with("REPO="))
        .expect("bootstrap.sh must define REPO=");
    line.split_once('=')
        .expect("REPO= line must have a value")
        .1
        .trim()
        .trim_matches('"')
        .to_string()
}

/// The release-asset base name the workflow publishes, e.g.
/// `installer-neo-install-` (a per-matrix target is appended to it).
fn workflow_asset_prefix(workflow: &str) -> &'static str {
    // The Package step copies the built binary to
    //   artifacts/installer-neo-install-${{ matrix.target }}
    // and uploads it under that same name.
    let prefix = "installer-neo-install-";
    assert!(
        workflow.contains(&format!("artifacts/{prefix}")),
        "installer-ci.yml no longer packages an '{prefix}<target>' asset — \
         update this test and bootstrap.sh together"
    );
    prefix
}

#[test]
fn bootstrap_repo_matches_monorepo() {
    let bootstrap = read("scripts/bootstrap.sh");
    assert_eq!(
        bootstrap_repo(&bootstrap),
        monorepo_slug(),
        "bootstrap.sh REPO must point at the monorepo that publishes installer releases"
    );
}

#[test]
fn bootstrap_downloads_workflow_asset_name() {
    let bootstrap = read("scripts/bootstrap.sh");
    let workflow = read("../.github/workflows/installer-ci.yml");
    let prefix = workflow_asset_prefix(&workflow);
    let expected = format!("{prefix}${{PLATFORM}}");
    assert!(
        bootstrap.contains(&expected),
        "bootstrap.sh must download the '{expected}' asset the workflow publishes; \
         download-related lines were: {:?}",
        bootstrap
            .lines()
            .filter(|l| l.contains("neo-install"))
            .collect::<Vec<_>>()
    );
}

#[test]
fn bootstrap_uses_installer_tag_prefix_for_pinned_versions() {
    // The workflow's release job only fires on `installer-v*` tags, and those
    // are the only tags carrying installer assets. A pinned NEO_INSTALLER_VERSION
    // must therefore be resolved as a tag under the same repo's releases.
    let bootstrap = read("scripts/bootstrap.sh");
    let workflow = read("../.github/workflows/installer-ci.yml");
    assert!(
        workflow.contains("installer-v"),
        "installer-ci.yml should key releases off the 'installer-v*' tag prefix"
    );
    assert!(
        bootstrap.contains("releases/download/${VERSION}/"),
        "bootstrap.sh must resolve a pinned NEO_INSTALLER_VERSION via \
         releases/download/<tag>/ on the same repo"
    );
}
