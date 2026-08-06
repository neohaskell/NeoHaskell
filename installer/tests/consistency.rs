//! Consistency tests: the `curl | sh` bootstrap script (`scripts/bootstrap.sh`)
//! must download release assets from the same repository, and under the same
//! asset-naming scheme, that the monorepo release workflow
//! (`.github/workflows/installer-ci.yml`) actually publishes them under.
//!
//! A mismatch means a real bootstrap 404s against real releases, which no
//! amount of unit testing of the binary would catch. These tests read the two
//! sources of truth off disk and assert they agree.
//!
//! A second group ties the NATIVE `neo` release contract together: the
//! installer's own download logic (`neo_install::release`), the release workflow
//! (`.github/workflows/neo-release.yml`), and the shared naming/checksum script
//! (`scripts/neo-release`) must agree on the asset names, the supported targets,
//! the publishing repository, and the `neo-v*` tag prefix — and the installer
//! must NOT install `neo` by evaluating/compiling the `neo#neo-cli` flake.

use neo_install::release;
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

#[test]
fn bootstrap_stages_download_in_mktemp_not_predictable_path() {
    // A fixed /tmp/neo-install path is a TOCTOU/symlink hazard: another local
    // user can pre-create or swap the file between download, chmod, and exec.
    // The download must land in an unpredictable mktemp file instead, and no
    // executable line may reference the old hardcoded path.
    let bootstrap = read("scripts/bootstrap.sh");
    assert!(
        bootstrap.contains("mktemp"),
        "bootstrap.sh must stage the downloaded installer via mktemp"
    );
    let offending: Vec<&str> = bootstrap
        .lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .filter(|l| l.contains("/tmp/neo-install"))
        .collect();
    assert!(
        offending.is_empty(),
        "bootstrap.sh must not reference a hardcoded /tmp/neo-install path in \
         executable code — download, chmod, and exec must use the mktemp file. \
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

/// Source bootstrap.sh, redefine its `fetch_releases_page` network seam so it
/// serves the given per-page fixtures (page 1 = `pages[0]`, …) with an empty
/// `[]` array past the last one (mirroring the GitHub API), then run
/// `resolve_latest_installer_tag`. Returns its stdout (the selected tag, or
/// empty). No network, no download, no install side effect.
fn resolve_latest_installer_tag_paged(pages: &[&str]) -> String {
    let bootstrap = crate_dir().join("scripts/bootstrap.sh");
    // Build a POSIX `case` that maps a page number to its fixture JSON via a
    // quoted heredoc, defaulting to the empty-array terminator.
    let mut cases = String::new();
    for (i, body) in pages.iter().enumerate() {
        cases.push_str(&format!(
            "{page}) cat <<'NEO_EOF'\n{body}\nNEO_EOF\n;;\n",
            page = i + 1,
            body = body
        ));
    }
    cases.push_str("*) printf '[]\\n' ;;\n");
    let script = format!(
        "export NEO_BOOTSTRAP_SOURCE_ONLY=1\n\
         . \"$1\"\n\
         fetch_releases_page() {{ case \"$1\" in\n{cases}esac; }}\n\
         resolve_latest_installer_tag\n"
    );
    let out = Command::new("sh")
        .arg("-c")
        .arg(script)
        .arg("sh") // $0
        .arg(bootstrap.to_str().expect("bootstrap path is valid UTF-8"))
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("spawn sh to source bootstrap.sh");
    assert!(
        out.status.success(),
        "resolve_latest_installer_tag exited non-zero; stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn bootstrap_latest_resolves_installer_release_on_second_page() {
    // GitHub paginates releases newest-first across ALL tags. With more than one
    // page (100) of newer non-installer releases ahead of it, the newest
    // installer release lands on a later page; the resolver must page past the
    // first to find it rather than giving up after page one.
    let page1 = r#"[
      {"tag_name": "core-v9.9.9"},
      {"tag_name": "core-v9.9.8"}
    ]"#;
    let page2 = r#"[
      {"tag_name": "installer-v3.1.0", "name": "installer (newest installer)"},
      {"tag_name": "installer-v3.0.0", "name": "installer (older)"},
      {"tag_name": "core-v5.0.0"}
    ]"#;
    assert_eq!(
        resolve_latest_installer_tag_paged(&[page1, page2]),
        "installer-v3.1.0",
        "the newest 'installer-v*' release on page two must be resolved when \
         page one holds only newer non-installer releases"
    );
}

#[test]
fn bootstrap_latest_stops_at_empty_page_without_installer_release() {
    // Page one has only core releases; page two is the empty-array terminator
    // the GitHub API returns past the last page. The resolver must stop there
    // and select nothing (so the caller fails loudly) rather than looping.
    let page1 = r#"[{"tag_name": "core-v1.0.0"}]"#;
    assert_eq!(
        resolve_latest_installer_tag_paged(&[page1]),
        "",
        "no installer release across all pages → nothing selected"
    );
}

#[test]
fn bootstrap_latest_propagates_release_api_failure() {
    let bootstrap = crate_dir().join("scripts/bootstrap.sh");
    let out = Command::new("sh")
        .arg("-c")
        .arg(
            "export NEO_BOOTSTRAP_SOURCE_ONLY=1\n\
             . \"$1\"\n\
             fetch_releases_page() { return 42; }\n\
             resolve_latest_installer_tag",
        )
        .arg("sh")
        .arg(bootstrap.to_str().expect("bootstrap path is valid UTF-8"))
        .output()
        .expect("spawn sh to test release API failure propagation");
    assert!(
        !out.status.success(),
        "release API/network failure must propagate as non-zero, not masquerade as an empty release stream"
    );
}

// ── Native `neo` release contract: installer ↔ neo-release.yml ↔ neo-release ──

/// The repo root (`installer/`'s parent).
fn repo_root() -> PathBuf {
    crate_dir()
        .parent()
        .expect("installer/ has a parent")
        .to_path_buf()
}

fn read_repo(rel: &str) -> String {
    let p = repo_root().join(rel);
    fs::read_to_string(&p).unwrap_or_else(|e| panic!("failed to read {}: {e}", p.display()))
}

/// Run `scripts/neo-release <args...>` and return trimmed stdout (the shared
/// naming contract, executable). Panics on non-zero exit.
fn neo_release(args: &[&str]) -> String {
    let script = repo_root().join("scripts/neo-release");
    let out = Command::new(&script)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("spawn {}: {e}", script.display()));
    assert!(
        out.status.success(),
        "scripts/neo-release {args:?} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn installer_asset_names_match_the_shared_release_script() {
    // The installer downloads `neo_asset_name(target)`; the workflow packages via
    // `scripts/neo-release asset-name`. They must be byte-identical per target.
    for target in release::NEO_TARGETS {
        assert_eq!(
            release::neo_asset_name(target),
            neo_release(&["asset-name", target]),
            "installer asset name and scripts/neo-release disagree for {target}"
        );
    }
}

#[test]
fn installer_targets_match_the_shared_release_script() {
    let mut script_targets: Vec<String> = neo_release(&["targets"])
        .lines()
        .map(str::to_string)
        .collect();
    script_targets.sort();
    let mut installer_targets: Vec<String> =
        release::NEO_TARGETS.iter().map(|s| s.to_string()).collect();
    installer_targets.sort();
    assert_eq!(
        script_targets, installer_targets,
        "the installer's supported targets and scripts/neo-release have drifted"
    );
}

#[test]
fn neo_release_workflow_builds_every_installer_target() {
    let wf = read_repo(".github/workflows/neo-release.yml");
    for target in release::NEO_TARGETS {
        assert!(
            wf.contains(target),
            "neo-release.yml no longer builds '{target}' — a platform the installer \
             downloads would have no published asset"
        );
    }
}

#[test]
fn neo_release_workflow_uses_the_neo_v_tag_prefix() {
    let wf = read_repo(".github/workflows/neo-release.yml");
    assert!(
        wf.contains(release::NEO_TAG_PREFIX),
        "neo-release.yml must key releases off the '{}' tag prefix the installer resolves",
        release::NEO_TAG_PREFIX
    );
}

#[test]
fn neo_release_workflow_routes_naming_through_the_shared_script() {
    let wf = read_repo(".github/workflows/neo-release.yml");
    assert!(
        wf.contains("scripts/neo-release"),
        "neo-release.yml must package/checksum via scripts/neo-release so its asset \
         names cannot drift from what the installer downloads"
    );
}

#[test]
fn checksum_manifest_name_is_one_convention_across_both_trains() {
    // ONE checksum-manifest filename everywhere: the installer's native-download
    // path (release::SHA256SUMS), the shared script, and BOTH release workflows
    // must publish/read `SHA256SUMS` — never a divergent `SHA256SUMS.txt`.
    assert_eq!(release::SHA256SUMS, "SHA256SUMS");
    let txt = format!("{}.txt", release::SHA256SUMS);
    for wf in [
        ".github/workflows/neo-release.yml",
        ".github/workflows/installer-ci.yml",
    ] {
        let text = read_repo(wf);
        assert!(
            text.contains(release::SHA256SUMS),
            "{wf} must publish the '{}' manifest",
            release::SHA256SUMS
        );
        assert!(
            !text.contains(&txt),
            "{wf} still uses the divergent '{txt}' name — unify on '{}'",
            release::SHA256SUMS
        );
    }
}

#[test]
fn installer_publishes_to_the_declared_monorepo() {
    // The installer downloads `neo` releases from NEO_REPO; that must be the same
    // monorepo the crate declares (and therefore where the workflow publishes).
    assert_eq!(
        release::NEO_REPO,
        monorepo_slug(),
        "release::NEO_REPO must point at the monorepo the crate is published from"
    );
}

#[test]
fn installer_does_not_install_neo_via_the_neo_cli_flake() {
    // The native-download installer must NEVER evaluate/compile/install neo from
    // github:neohaskell/neo#neo-cli. The prose here names the forbidden ref, so
    // the guard scans EXECUTABLE Rust only: the `nix profile install` arg tuple
    // is the artifact that reappears if someone reintroduces that path.
    for src in ["src/install.rs", "src/release.rs", "src/lib.rs"] {
        let body = read(src);
        let executable: String = body
            .lines()
            .filter(|l| {
                let t = l.trim_start();
                !(t.starts_with("//") || t.starts_with('*'))
            })
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            !executable.contains("\"profile\", \"install\""),
            "{src} constructs a `nix profile install` command — the installer must \
             download a native neo release, not install the neo#neo-cli flake"
        );
        assert!(
            !executable.contains("neo#neo-cli"),
            "{src} references the neo#neo-cli flake in executable code — forbidden"
        );
    }
}
