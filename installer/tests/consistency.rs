//! Consistency tests: the `curl | sh` bootstrap script (`scripts/bootstrap.sh`)
//! must download release assets from the same repository, and under the same
//! asset-naming scheme, that the monorepo release workflow
//! (`.github/workflows/installer-ci.yml`) actually publishes them under.
//!
//! A mismatch means a real bootstrap 404s against real releases, which no
//! amount of unit testing of the binary would catch. These tests read the two
//! sources of truth off disk and assert they agree.

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

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

#[test]
fn bootstrap_default_does_not_use_repo_wide_latest_release() {
    // `releases/latest/` redirects to the newest release of ANY tag on the
    // repo. Since installer assets ship ONLY on 'installer-v*' tags, a newer
    // core-library release would make that redirect 404 for the installer
    // asset. The default path must resolve the newest INSTALLER release instead.
    let bootstrap = read("scripts/bootstrap.sh");
    // Comments legitimately name the redirect to explain why it is avoided;
    // only executable lines matter.
    let offending: Vec<&str> = bootstrap
        .lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .filter(|l| l.contains("releases/latest"))
        .collect();
    assert!(
        offending.is_empty(),
        "bootstrap.sh must not use the repository-wide 'releases/latest' \
         redirect in executable code — it resolves non-installer releases too; \
         select the newest 'installer-v*' release explicitly. \
         Offending lines: {offending:?}"
    );
}

/// Load bootstrap.sh's shell functions without running the installer, then run
/// `newest_installer_tag` with `stdin` as the GitHub "list releases" payload.
/// Returns the function's stdout (the selected tag, or empty).
fn newest_installer_tag(releases_json: &str) -> String {
    let bootstrap = crate_dir().join("scripts/bootstrap.sh");
    let mut child = Command::new("sh")
        .arg("-c")
        // NEO_BOOTSTRAP_SOURCE_ONLY=1 loads the functions but skips the install
        // side effects, so this test never downloads or executes anything.
        .arg("export NEO_BOOTSTRAP_SOURCE_ONLY=1\n. \"$1\"\nnewest_installer_tag")
        .arg("sh") // $0
        .arg(bootstrap.to_str().expect("bootstrap path is valid UTF-8"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn sh to source bootstrap.sh");
    // Tolerate a broken pipe: if bootstrap.sh is misbuilt and exits before
    // reading stdin, the output assertions below report it cleanly.
    let _ = child
        .stdin
        .take()
        .expect("child stdin")
        .write_all(releases_json.as_bytes());
    let out = child.wait_with_output().expect("wait for sh");
    assert!(
        out.status.success(),
        "newest_installer_tag exited non-zero; stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn bootstrap_latest_ignores_newer_non_installer_release() {
    // Regression: the GitHub "list releases" API returns newest-first. A core
    // release published AFTER the latest installer release must NOT be picked;
    // the newest 'installer-v*' tag must win even though a newer tag precedes it.
    let releases = r#"[
      {"tag_name": "core-v9.9.9", "name": "core (newer, no installer asset)"},
      {"tag_name": "installer-v1.2.3", "name": "installer (the one we want)"},
      {"tag_name": "installer-v1.0.0", "name": "installer (older)"},
      {"tag_name": "core-v8.0.0", "name": "core (older)"}
    ]"#;
    assert_eq!(
        newest_installer_tag(releases),
        "installer-v1.2.3",
        "the newest 'installer-v*' release must be selected, ignoring the newer \
         non-installer 'core-v9.9.9' release"
    );
}

#[test]
fn bootstrap_latest_emits_nothing_when_no_installer_release_exists() {
    // Only non-installer releases → no tag selected, so the caller can fail
    // loudly instead of building a URL that would 404.
    let releases = r#"[
      {"tag_name": "core-v9.9.9"},
      {"tag_name": "core-v8.0.0"}
    ]"#;
    assert_eq!(
        newest_installer_tag(releases),
        "",
        "no 'installer-v*' release present → nothing should be selected"
    );
}
