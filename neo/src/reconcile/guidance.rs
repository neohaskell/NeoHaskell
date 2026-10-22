use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

use tokio::io::AsyncReadExt;
use tokio::process::Command;

use crate::reconcile::source::{FrameworkOrigin, FrameworkSource, FullSha};

use self::corpus::{
    CachedObjects, CorpusManifest, TreeEntry, git_blob_oid, parse_manifest, validate_cache,
    validate_corpus_path, validate_links,
};

mod corpus;

const MAX_TREE_BYTES: usize = 16 * 1024 * 1024;
const MAX_BLOB_BYTES: usize = 2 * 1024 * 1024;
const MAX_CORPUS_BYTES: usize = 8 * 1024 * 1024;
const MAX_CORPUS_FILES: usize = 128;
const GIT_TIMEOUT: Duration = Duration::from_secs(60);
const CORPUS_PREFIX: &str = "docs/agent/";

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum GuidancePlan {
    Ready,
    Retrieve,
    Placeholder,
    Offline(String),
    Failed(String),
}

#[derive(Debug)]
struct GitOutput {
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    success: bool,
}

pub(super) fn begin(
    project_dir: &Path,
    source: &FrameworkSource,
    explicit_retry: bool,
) -> miette::Result<GuidancePlan> {
    let active = active_path(project_dir);
    match source {
        FrameworkSource::OfflinePlaceholder => {
            write_unavailable(
                &active,
                "offline-placeholder",
                Some("The project uses the offline placeholder source."),
            )?;
            Ok(GuidancePlan::Placeholder)
        }
        FrameworkSource::Commit { sha, origin } => {
            let cache = cache_path(project_dir, sha);
            let cache_exists = cache.exists();
            let cache_valid = validate_cache(&cache, sha).is_ok();
            if cache_valid && active_matches_sha(&active, sha) {
                Ok(GuidancePlan::Ready)
            } else {
                let offline = matches!(origin, FrameworkOrigin::Upstream)
                    && std::env::var_os("NEO_SKIP_NETWORK").is_some()
                    && !cache_valid;
                write_unavailable(
                    &active,
                    sha.as_str(),
                    offline.then_some(
                        "`NEO_SKIP_NETWORK` is set, so no network retrieval was attempted.",
                    ),
                )?;
                if offline {
                    return Ok(GuidancePlan::Offline(format!(
                        "framework guidance for `{sha}` is unavailable because `NEO_SKIP_NETWORK` is set; unset it and run `neo guidance retry`"
                    )));
                }
                let failure = failure_path(project_dir, sha);
                let cached_invalid = cache_exists && !cache_valid;
                if explicit_retry {
                    Ok(GuidancePlan::Retrieve)
                } else if cached_invalid {
                    let warning = format!(
                        "cached framework guidance for `{sha}` is malformed or incomplete; retry with `neo guidance retry`"
                    );
                    let warning = match write_failure(&failure, &warning) {
                        Ok(()) => warning,
                        Err(error) => format!(
                            "{warning}; could not record the unavailable guidance state: {error}"
                        ),
                    };
                    Ok(GuidancePlan::Failed(warning))
                } else if let Some(warning) = read_failure(&failure) {
                    Ok(GuidancePlan::Failed(warning))
                } else {
                    Ok(GuidancePlan::Retrieve)
                }
            }
        }
    }
}

pub(super) async fn finish(
    project_dir: &Path,
    source: &FrameworkSource,
    plan: GuidancePlan,
    explicit_retry: bool,
) -> Option<String> {
    match plan {
        GuidancePlan::Ready => return None,
        GuidancePlan::Placeholder => {
            return Some(format!(
                "framework guidance is unavailable because this project uses the offline placeholder source; unset `NEO_SKIP_NETWORK` and run `neo guidance retry`"
            ));
        }
        GuidancePlan::Offline(warning) => return Some(warning),
        GuidancePlan::Failed(warning) => return Some(warning),
        GuidancePlan::Retrieve => {}
    }

    let FrameworkSource::Commit { sha, origin } = source else {
        return Some("framework guidance has no exact framework commit; run `neo guidance retry` after configuring a framework source".to_string());
    };
    let cache = cache_path(project_dir, sha);
    let failure = failure_path(project_dir, sha);
    if validate_cache(&cache, sha).is_ok() {
        if let Err(error) = write_ready(&active_path(project_dir), sha) {
            return Some(retrieval_warning(sha, &error.to_string()));
        }
        let _ = std::fs::remove_file(&failure);
        return None;
    }

    if cache.exists() {
        if !explicit_retry {
            return Some(format!(
                "cached framework guidance for `{sha}` is malformed or incomplete; it remains unavailable. Repair the source or cache, then run `neo guidance retry`"
            ));
        }
        if let Err(error) = quarantine_cache(&cache) {
            return Some(retrieval_warning(sha, &error.to_string()));
        }
    }

    if explicit_retry {
        let _ = std::fs::remove_file(&failure);
    }
    match retrieve_and_promote(project_dir, sha, origin).await {
        Ok(()) => match write_ready(&active_path(project_dir), sha) {
            Ok(()) => {
                let _ = std::fs::remove_file(&failure);
                None
            }
            Err(error) => Some(retrieval_warning(sha, &error.to_string())),
        },
        Err(error) => {
            let warning = retrieval_warning(sha, &error.to_string());
            if let Err(write_error) = write_failure(&failure, &warning) {
                return Some(format!(
                    "{warning}; recording the failure state also failed: {write_error}"
                ));
            }
            Some(warning)
        }
    }
}

fn retrieval_warning(sha: &FullSha, detail: &str) -> String {
    format!(
        "framework guidance for `{sha}` is unavailable: {detail}. Fix the source or network issue, then run `neo guidance retry`"
    )
}

fn active_path(project_dir: &Path) -> PathBuf {
    project_dir.join(".neo/agent-guidance/active.md")
}

fn cache_path(project_dir: &Path, sha: &FullSha) -> PathBuf {
    project_dir
        .join(".neo/agent-guidance/cache")
        .join(sha.as_str())
}

fn failure_path(project_dir: &Path, sha: &FullSha) -> PathBuf {
    project_dir
        .join(".neo/agent-guidance/failures")
        .join(format!("{}.txt", sha.as_str()))
}

fn write_failure(path: &Path, warning: &str) -> miette::Result<()> {
    let bounded = warning.chars().take(2048).collect::<String>();
    super::atomic_write(path, bounded.as_bytes()).map_err(|error| {
        miette::miette!(
            "recording unavailable guidance state at `{}`: {error}",
            path.display()
        )
    })
}

fn read_failure(path: &Path) -> Option<String> {
    let metadata = std::fs::symlink_metadata(path).ok()?;
    if !metadata.file_type().is_file() || metadata.file_type().is_symlink() || metadata.len() > 4096
    {
        return Some(
            "framework guidance retrieval previously failed; run `neo guidance retry`".to_string(),
        );
    }
    std::fs::read_to_string(path)
        .ok()
        .filter(|warning| !warning.trim().is_empty())
        .or_else(|| {
            Some(
                "framework guidance retrieval previously failed; run `neo guidance retry`"
                    .to_string(),
            )
        })
}

fn write_unavailable(path: &Path, source_sha: &str, reason: Option<&str>) -> miette::Result<()> {
    let reason = reason
        .map(|reason| format!(" {reason}"))
        .unwrap_or_default();
    let content = format!(
        "---\nstatus: unavailable\nsource-sha: {source_sha}\n---\n\nFramework guidance for this source is unavailable.{reason} Generated build files still use the selected framework source. Fix the reported source or network issue, then run `neo guidance retry`.\n"
    );
    super::atomic_write(path, content.as_bytes()).map_err(|error| {
        miette::miette!(
            "writing the unavailable framework guidance index at `{}`: {error}",
            path.display()
        )
    })
}

fn write_ready(path: &Path, sha: &FullSha) -> miette::Result<()> {
    let content = format!(
        "---\nstatus: ready\nsource-sha: {}\n---\n\nFramework API guidance is synchronized to the exact framework commit `{}`.\n\n[Open the guidance index](cache/{}/index.md).\n",
        sha.as_str(),
        sha.as_str(),
        sha.as_str(),
    );
    super::atomic_write(path, content.as_bytes()).map_err(|error| {
        miette::miette!(
            "writing the ready framework guidance index at `{}`: {error}",
            path.display()
        )
    })
}

fn active_matches_sha(path: &Path, sha: &FullSha) -> bool {
    let Ok(contents) = std::fs::read_to_string(path) else {
        return false;
    };
    contents.starts_with("---\nstatus: ready\n")
        && contents
            .lines()
            .any(|line| line == format!("source-sha: {}", sha.as_str()))
}

async fn retrieve_and_promote(
    project_dir: &Path,
    sha: &FullSha,
    origin: &FrameworkOrigin,
) -> miette::Result<()> {
    let cache_root = project_dir.join(".neo/agent-guidance/cache");
    std::fs::create_dir_all(&cache_root).map_err(|error| {
        miette::miette!(
            "creating guidance cache at `{}`: {error}",
            cache_root.display()
        )
    })?;
    cleanup_staging(&cache_root)?;
    let staging = tempfile::Builder::new()
        .prefix(".guidance-stage-")
        .tempdir_in(&cache_root)
        .map_err(|error| miette::miette!("creating guidance staging directory: {error}"))?;
    let upstream_git_dir;
    let git_dir = match origin {
        FrameworkOrigin::LocalGit { root } => {
            upstream_git_dir = None;
            root.clone()
        }
        FrameworkOrigin::Upstream => {
            let git_dir = staging.path().join("source.git");
            let path_arg = git_dir
                .to_str()
                .ok_or_else(|| miette::miette!("temporary Git path is not UTF-8"))?;
            run_git(None, &["init", "--bare", path_arg], 64 * 1024).await?;
            run_git(
                None,
                &[
                    "--git-dir",
                    path_arg,
                    "fetch",
                    "--depth=1",
                    "--filter=blob:none",
                    "https://github.com/neohaskell/neohaskell.git",
                    sha.as_str(),
                ],
                64 * 1024,
            )
            .await?;
            upstream_git_dir = Some(git_dir.clone());
            git_dir
        }
    };
    let commit_kind = git_stdout(&git_dir, &["cat-file", "-t", sha.as_str()], 1024).await?;
    if commit_kind.trim() != "commit" {
        return Err(miette::miette!(
            "requested framework object `{sha}` is not a commit"
        ));
    }
    let entries = list_tree(&git_dir, sha).await?;
    let manifest_entry = entries.get("manifest.json").ok_or_else(|| {
        miette::miette!("framework commit `{sha}` has no `docs/agent/manifest.json`")
    })?;
    let manifest_bytes = cat_blob(&git_dir, manifest_entry).await?;
    let manifest = parse_manifest(&manifest_bytes)?;
    let mut objects = BTreeMap::from([("manifest.json".to_string(), manifest_entry.oid.clone())]);
    let mut total = manifest_bytes.len();
    if total > MAX_CORPUS_BYTES {
        return Err(miette::miette!(
            "framework guidance corpus exceeds the {MAX_CORPUS_BYTES}-byte size limit"
        ));
    }
    for page in &manifest.pages {
        let entry = entries.get(page).ok_or_else(|| {
            miette::miette!("framework commit `{sha}` is missing listed page `docs/agent/{page}`")
        })?;
        let bytes = cat_blob(&git_dir, entry).await?;
        total = total.saturating_add(bytes.len());
        if total > MAX_CORPUS_BYTES {
            return Err(miette::miette!(
                "framework guidance corpus exceeds the {MAX_CORPUS_BYTES}-byte size limit"
            ));
        }
        String::from_utf8(bytes.clone())
            .map_err(|error| miette::miette!("guidance page `{page}` is not UTF-8: {error}"))?;
        write_staged(staging.path(), page, &bytes)?;
        objects.insert(page.clone(), entry.oid.clone());
    }
    write_staged(staging.path(), "manifest.json", &manifest_bytes)?;
    let staged_pages = load_staged_pages(staging.path(), &manifest, &objects)?;
    validate_links(&manifest, &staged_pages)?;
    if let Some(git_dir) = upstream_git_dir {
        std::fs::remove_dir_all(&git_dir).map_err(|error| {
            miette::miette!(
                "removing temporary source Git data at `{}`: {error}",
                git_dir.display()
            )
        })?;
    }
    let object_map = CachedObjects { schema: 1, objects };
    let object_map_path = staging.path().join(".neo-source-objects.json");
    let object_map_bytes = serde_json::to_vec_pretty(&object_map)
        .map_err(|error| miette::miette!("serializing guidance cache object map: {error}"))?;
    std::fs::write(&object_map_path, object_map_bytes)
        .map_err(|error| miette::miette!("writing guidance cache object map: {error}"))?;
    let target = cache_path(project_dir, sha);
    if target.exists() {
        return validate_cache(&target, sha);
    }
    std::fs::rename(staging.path(), &target).map_err(|error| {
        miette::miette!(
            "promoting guidance cache for `{sha}` to `{}`: {error}",
            target.display()
        )
    })?;
    validate_cache(&target, sha)
}

fn cleanup_staging(cache_root: &Path) -> miette::Result<()> {
    for entry in std::fs::read_dir(cache_root).map_err(|error| {
        miette::miette!(
            "listing guidance cache root `{}`: {error}",
            cache_root.display()
        )
    })? {
        let entry =
            entry.map_err(|error| miette::miette!("reading guidance cache entry: {error}"))?;
        if !entry
            .file_name()
            .to_string_lossy()
            .starts_with(".guidance-stage-")
        {
            continue;
        }
        let path = entry.path();
        let metadata = std::fs::symlink_metadata(&path).map_err(|error| {
            miette::miette!(
                "checking stale guidance stage `{}`: {error}",
                path.display()
            )
        })?;
        if metadata.file_type().is_dir() && !metadata.file_type().is_symlink() {
            std::fs::remove_dir_all(&path).map_err(|error| {
                miette::miette!(
                    "removing stale guidance stage `{}`: {error}",
                    path.display()
                )
            })?;
        } else {
            std::fs::remove_file(&path).map_err(|error| {
                miette::miette!(
                    "removing stale guidance stage `{}`: {error}",
                    path.display()
                )
            })?;
        }
    }
    Ok(())
}

async fn list_tree(git_dir: &Path, sha: &FullSha) -> miette::Result<BTreeMap<String, TreeEntry>> {
    let sha_arg = sha.as_str();
    let output = git_output(
        git_dir,
        &[
            "ls-tree",
            "-r",
            "-z",
            "--full-tree",
            sha_arg,
            "--",
            "docs/agent",
        ],
        MAX_TREE_BYTES,
    )
    .await?;
    ensure_git_success("listing the exact framework guidance tree", &output)?;
    let mut entries = BTreeMap::new();
    for record in output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
    {
        let tab = record
            .iter()
            .position(|byte| *byte == b'\t')
            .ok_or_else(|| miette::miette!("Git returned a malformed guidance tree entry"))?;
        let (metadata, raw_path) = (&record[..tab], &record[tab + 1..]);
        let metadata = std::str::from_utf8(metadata)
            .map_err(|error| miette::miette!("Git returned non-UTF-8 tree metadata: {error}"))?;
        let raw_path = std::str::from_utf8(raw_path).map_err(|error| {
            miette::miette!("framework guidance tree contains a non-UTF-8 path: {error}")
        })?;
        let mut parts = metadata.split_whitespace();
        let mode = parts.next().unwrap_or("");
        let object_type = parts.next().unwrap_or("");
        let oid = parts.next().unwrap_or("");
        if object_type != "blob" || mode != "100644" && mode != "100755" {
            return Err(miette::miette!(
                "framework guidance tree contains non-regular entry `{raw_path}`"
            ));
        }
        let relative = raw_path.strip_prefix(CORPUS_PREFIX).ok_or_else(|| {
            miette::miette!("Git returned a path outside `{CORPUS_PREFIX}`: `{raw_path}`")
        })?;
        validate_corpus_path(relative)?;
        if oid.len() != 40 || !oid.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(miette::miette!(
                "Git returned an invalid blob id for `{raw_path}`"
            ));
        }
        if entries
            .insert(
                relative.to_string(),
                TreeEntry {
                    oid: oid.to_string(),
                    path: raw_path.to_string(),
                },
            )
            .is_some()
        {
            return Err(miette::miette!(
                "framework guidance tree repeats `{raw_path}`"
            ));
        }
    }
    Ok(entries)
}

async fn cat_blob(git_dir: &Path, entry: &TreeEntry) -> miette::Result<Vec<u8>> {
    let output = git_output(git_dir, &["cat-file", "blob", &entry.oid], MAX_BLOB_BYTES).await?;
    ensure_git_success(
        &format!("reading framework guidance blob `{}`", entry.path),
        &output,
    )?;
    Ok(output.stdout)
}

fn write_staged(root: &Path, relative: &str, bytes: &[u8]) -> miette::Result<()> {
    validate_corpus_path(relative)?;
    let path = root.join(relative);
    let parent = path
        .parent()
        .ok_or_else(|| miette::miette!("guidance page `{relative}` has no parent directory"))?;
    std::fs::create_dir_all(parent)
        .map_err(|error| miette::miette!("creating staged guidance page directory: {error}"))?;
    std::fs::write(path, bytes)
        .map_err(|error| miette::miette!("writing staged guidance page `{relative}`: {error}"))
}

fn load_staged_pages(
    root: &Path,
    manifest: &CorpusManifest,
    objects: &BTreeMap<String, String>,
) -> miette::Result<BTreeMap<String, String>> {
    let mut pages = BTreeMap::new();
    for page in &manifest.pages {
        let bytes = std::fs::read(root.join(page))
            .map_err(|error| miette::miette!("reading staged guidance page `{page}`: {error}"))?;
        if git_blob_oid(&bytes) != *objects.get(page).unwrap_or(&String::new()) {
            return Err(miette::miette!(
                "staged guidance page `{page}` does not match its Git object"
            ));
        }
        pages.insert(
            page.clone(),
            String::from_utf8(bytes)
                .map_err(|error| miette::miette!("guidance page `{page}` is not UTF-8: {error}"))?,
        );
    }
    Ok(pages)
}

async fn git_stdout(git_dir: &Path, args: &[&str], limit: usize) -> miette::Result<String> {
    let output = git_output(git_dir, args, limit).await?;
    ensure_git_success("reading the exact framework Git object", &output)?;
    String::from_utf8(output.stdout)
        .map_err(|error| miette::miette!("Git returned non-UTF-8 output: {error}"))
}

async fn git_output(
    git_dir: &Path,
    args: &[&str],
    stdout_limit: usize,
) -> miette::Result<GitOutput> {
    if git_dir.join("HEAD").exists() && git_dir.join("objects").is_dir() {
        let path = git_dir
            .to_str()
            .ok_or_else(|| miette::miette!("Git directory path is not UTF-8"))?;
        let mut all_args = vec!["--git-dir", path];
        all_args.extend_from_slice(args);
        run_git(None, &all_args, stdout_limit).await
    } else {
        run_git(Some(git_dir), args, stdout_limit).await
    }
}

async fn run_git(
    cwd: Option<&Path>,
    args: &[&str],
    stdout_limit: usize,
) -> miette::Result<GitOutput> {
    let mut command = Command::new("git");
    if let Some(cwd) = cwd {
        command.current_dir(cwd);
    }
    command
        .args(args)
        .kill_on_drop(true)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut child = command
        .spawn()
        .map_err(|error| miette::miette!("starting Git for framework guidance: {error}"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| miette::miette!("Git stdout pipe was unavailable"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| miette::miette!("Git stderr pipe was unavailable"))?;
    let stdout_task = tokio::spawn(read_capped(stdout, stdout_limit));
    let stderr_task = tokio::spawn(read_capped(stderr, 64 * 1024));
    let status = match tokio::time::timeout(GIT_TIMEOUT, child.wait()).await {
        Ok(result) => result.map_err(|error| {
            miette::miette!("waiting for Git framework guidance command: {error}")
        })?,
        Err(_) => {
            let _ = child.kill().await;
            let _ = child.wait().await;
            return Err(miette::miette!(
                "Git framework guidance command exceeded the {} second limit",
                GIT_TIMEOUT.as_secs()
            ));
        }
    };
    let (stdout, stdout_overflow) = stdout_task
        .await
        .map_err(|error| miette::miette!("reading Git stdout: {error}"))?;
    let (stderr, stderr_overflow) = stderr_task
        .await
        .map_err(|error| miette::miette!("reading Git stderr: {error}"))?;
    if stdout_overflow || stderr_overflow {
        return Err(miette::miette!(
            "Git framework guidance output exceeded its size limit"
        ));
    }
    let output = GitOutput {
        stdout,
        stderr,
        success: status.success(),
    };
    ensure_git_success(&format!("`git {}`", args.join(" ")), &output)?;
    Ok(output)
}

async fn read_capped<R: tokio::io::AsyncRead + Unpin>(
    mut reader: R,
    limit: usize,
) -> (Vec<u8>, bool) {
    let mut output = Vec::new();
    let mut buffer = [0u8; 16 * 1024];
    let mut overflow = false;
    loop {
        match reader.read(&mut buffer).await {
            Ok(0) | Err(_) => break,
            Ok(count) => {
                let remaining = limit.saturating_sub(output.len());
                let kept = count.min(remaining);
                output.extend_from_slice(&buffer[..kept]);
                overflow |= kept < count;
            }
        }
    }
    (output, overflow)
}

fn ensure_git_success(operation: &str, output: &GitOutput) -> miette::Result<()> {
    if output.success {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    Err(miette::miette!("{operation} failed: {}", stderr.trim()))
}

fn quarantine_cache(cache: &Path) -> miette::Result<()> {
    let parent = cache
        .parent()
        .ok_or_else(|| miette::miette!("guidance cache path has no parent"))?;
    let base = cache
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("cache");
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    for attempt in 0..100 {
        let quarantine = parent.join(format!(
            "{base}.invalid.{}.{}.{}",
            std::process::id(),
            timestamp,
            attempt
        ));
        if quarantine.exists() {
            continue;
        }
        return std::fs::rename(cache, &quarantine).map_err(|error| {
            miette::miette!(
                "quarantining invalid guidance cache `{}`: {error}",
                cache.display()
            )
        });
    }
    Err(miette::miette!(
        "could not allocate a quarantine name for `{}`",
        cache.display()
    ))
}

#[cfg(test)]
#[path = "guidance/tests.rs"]
mod tests;
