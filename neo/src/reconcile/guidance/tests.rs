use super::*;
use std::process::Command as StdCommand;
use tempfile::tempdir;

fn git(root: &Path, args: &[&str]) -> String {
    let output = StdCommand::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "git {:?} failed: {}",
        args,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn repository() -> (tempfile::TempDir, FullSha) {
    let temp = tempdir().unwrap();
    let root = temp.path();
    git(root, &["init", "-q"]);
    git(root, &["config", "user.email", "test@example.invalid"]);
    git(root, &["config", "user.name", "Guidance Test"]);
    std::fs::create_dir_all(root.join("docs/agent")).unwrap();
    write_corpus(root, "committed guidance");
    git(root, &["add", "docs/agent"]);
    git(root, &["commit", "-qm", "guidance"]);
    let sha = FullSha::parse(&git(root, &["rev-parse", "HEAD"])).unwrap();
    (temp, sha)
}

fn write_corpus(root: &Path, text: &str) {
    let manifest = r#"{"schema":1,"pages":["index.md","topic.md"],"capabilities":[{"name":"topic","page":"topic.md","source":"core/Api.hs","scenario":"testbed/topic.hurl"}]}"#;
    std::fs::write(root.join("docs/agent/manifest.json"), manifest).unwrap();
    std::fs::write(
        root.join("docs/agent/index.md"),
        format!("# Index\n\n[topic](topic.md)\n\n{text}\n"),
    )
    .unwrap();
    std::fs::write(root.join("docs/agent/topic.md"), "# Topic\n").unwrap();
}

fn local_source(root: &Path, sha: FullSha) -> FrameworkSource {
    FrameworkSource::Commit {
        sha,
        origin: FrameworkOrigin::LocalGit {
            root: root.to_path_buf(),
        },
    }
}

#[tokio::test]
async fn local_source_uses_committed_blobs_when_worktree_is_dirty() {
    let (repo, sha) = repository();
    let root = repo.path();
    std::fs::write(root.join("docs/agent/index.md"), "# Dirty guidance\n").unwrap();
    let project = tempdir().unwrap();
    let source = local_source(root, sha.clone());
    let plan = begin(project.path(), &source, false).unwrap();
    assert!(finish(project.path(), &source, plan, false).await.is_none());
    let cached = project.path().join(format!(
        ".neo/agent-guidance/cache/{}/index.md",
        sha.as_str()
    ));
    let body = std::fs::read_to_string(cached).unwrap();
    assert!(body.contains("committed guidance"));
    assert!(!body.contains("Dirty guidance"));
}

#[tokio::test]
async fn malformed_corpus_stays_unavailable_and_retry_quarantines_it() {
    let (repo, first_sha) = repository();
    let root = repo.path();
    let project = tempdir().unwrap();
    let source = local_source(root, first_sha.clone());
    let cache = cache_path(project.path(), &first_sha);
    std::fs::create_dir_all(&cache).unwrap();
    std::fs::write(cache.join("manifest.json"), "{}").unwrap();

    let plan = begin(project.path(), &source, false).unwrap();
    assert!(active_path(project.path()).exists());
    assert!(
        finish(project.path(), &source, plan, false)
            .await
            .unwrap()
            .contains("neo guidance retry")
    );
    let unavailable = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(unavailable.contains("status: unavailable"));
    assert!(!unavailable.contains(&format!("cache/{}/index.md", first_sha.as_str())));

    write_corpus(root, "retry succeeded");
    git(root, &["add", "docs/agent"]);
    git(root, &["commit", "-qm", "repair corpus"]);
    let repaired_sha = FullSha::parse(&git(root, &["rev-parse", "HEAD"])).unwrap();
    let repaired_source = local_source(root, repaired_sha.clone());
    let retry_cache = cache_path(project.path(), &repaired_sha);
    std::fs::create_dir_all(&retry_cache).unwrap();
    std::fs::write(retry_cache.join("manifest.json"), "bad cache").unwrap();
    let plan = begin(project.path(), &repaired_source, true).unwrap();
    assert!(
        finish(project.path(), &repaired_source, plan, true)
            .await
            .is_none()
    );
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: ready"));
    assert!(active.contains(repaired_sha.as_str()));
    assert!(
        std::fs::read_dir(project.path().join(".neo/agent-guidance/cache"))
            .unwrap()
            .any(|entry| entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .contains(".invalid."))
    );
}

#[tokio::test]
async fn a_different_sha_cache_is_never_used_as_fallback() {
    let (repo, good_sha) = repository();
    let root = repo.path();
    let project = tempdir().unwrap();
    let good_source = local_source(root, good_sha.clone());
    let plan = begin(project.path(), &good_source, false).unwrap();
    assert!(
        finish(project.path(), &good_source, plan, false)
            .await
            .is_none()
    );

    std::fs::write(root.join("docs/agent/manifest.json"), "not json").unwrap();
    git(root, &["add", "docs/agent/manifest.json"]);
    git(root, &["commit", "-qm", "malformed corpus"]);
    let bad_sha = FullSha::parse(&git(root, &["rev-parse", "HEAD"])).unwrap();
    let bad_source = local_source(root, bad_sha.clone());
    let plan = begin(project.path(), &bad_source, false).unwrap();
    assert!(
        finish(project.path(), &bad_source, plan, false)
            .await
            .unwrap()
            .contains("neo guidance retry")
    );
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: unavailable"));
    assert!(active.contains(bad_sha.as_str()));
    assert!(!active.contains(&format!("cache/{}/index.md", good_sha.as_str())));
}

#[tokio::test]
async fn active_index_is_repaired_when_its_link_changes() {
    let (repo, sha) = repository();
    let project = tempdir().unwrap();
    let source = local_source(repo.path(), sha.clone());
    let plan = begin(project.path(), &source, false).unwrap();
    assert!(finish(project.path(), &source, plan, false).await.is_none());

    let active = active_path(project.path());
    let stale = ready_contents(&sha).replace(
        &format!("cache/{}/index.md", sha.as_str()),
        "cache/0000000000000000000000000000000000000000/index.md",
    );
    std::fs::write(&active, stale).unwrap();

    let plan = begin(project.path(), &source, false).unwrap();
    assert!(matches!(plan, GuidancePlan::Retrieve));
    assert!(finish(project.path(), &source, plan, false).await.is_none());
    assert_eq!(
        std::fs::read_to_string(active).unwrap(),
        ready_contents(&sha)
    );
}

#[tokio::test]
async fn concurrent_retrievals_converge_on_one_ready_cache() {
    let (repo, sha) = repository();
    let project = tempdir().unwrap();
    let source = local_source(repo.path(), sha.clone());
    let first_plan = begin(project.path(), &source, false).unwrap();
    let second_plan = begin(project.path(), &source, false).unwrap();
    let (first, second) = tokio::join!(
        finish(project.path(), &source, first_plan, false),
        finish(project.path(), &source, second_plan, false),
    );

    assert!(first.is_none(), "first retrieval failed: {first:?}");
    assert!(second.is_none(), "second retrieval failed: {second:?}");
    assert!(validate_cache(&cache_path(project.path(), &sha), &sha).is_ok());
    assert_eq!(
        std::fs::read_to_string(active_path(project.path())).unwrap(),
        ready_contents(&sha)
    );
}

#[tokio::test]
async fn copied_cache_cannot_activate_for_another_commit() {
    let (repo, first_sha) = repository();
    let root = repo.path();
    let project = tempdir().unwrap();
    let first_source = local_source(root, first_sha.clone());
    let plan = begin(project.path(), &first_source, false).unwrap();
    assert!(
        finish(project.path(), &first_source, plan, false)
            .await
            .is_none()
    );

    std::fs::write(root.join("README.md"), "another framework commit\n").unwrap();
    git(root, &["add", "README.md"]);
    git(root, &["commit", "-qm", "new framework commit"]);
    let second_sha = FullSha::parse(&git(root, &["rev-parse", "HEAD"])).unwrap();
    let copied_cache = cache_path(project.path(), &second_sha);
    let first_cache = cache_path(project.path(), &first_sha);
    for entry in walkdir::WalkDir::new(&first_cache).min_depth(1) {
        let entry = entry.unwrap();
        let destination = copied_cache.join(entry.path().strip_prefix(&first_cache).unwrap());
        if entry.file_type().is_dir() {
            std::fs::create_dir_all(destination).unwrap();
        } else {
            std::fs::create_dir_all(destination.parent().unwrap()).unwrap();
            std::fs::copy(entry.path(), destination).unwrap();
        }
    }
    assert!(validate_cache(&first_cache, &first_sha).is_ok());
    let second_source = local_source(root, second_sha.clone());

    assert!(validate_cache(&copied_cache, &second_sha).is_err());
    let plan = begin(project.path(), &second_source, false).unwrap();
    assert!(matches!(plan, GuidancePlan::Failed(_)));
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: unavailable"));
    assert!(active.contains(second_sha.as_str()));
    assert!(!active.contains(&format!("cache/{}/index.md", second_sha.as_str())));

    let plan = begin(project.path(), &second_source, true).unwrap();
    assert!(
        finish(project.path(), &second_source, plan, true)
            .await
            .is_none()
    );
    assert!(validate_cache(&copied_cache, &second_sha).is_ok());
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: ready"));
    assert!(active.contains(second_sha.as_str()));
}

#[tokio::test]
async fn missing_corpus_failure_is_persisted_until_explicit_retry() {
    let temp = tempdir().unwrap();
    let root = temp.path();
    git(root, &["init", "-q"]);
    git(root, &["config", "user.email", "test@example.invalid"]);
    git(root, &["config", "user.name", "Guidance Test"]);
    std::fs::write(root.join("README.md"), "no agent corpus\n").unwrap();
    git(root, &["add", "README.md"]);
    git(root, &["commit", "-qm", "without corpus"]);
    let sha = FullSha::parse(&git(root, &["rev-parse", "HEAD"])).unwrap();
    let source = local_source(root, sha.clone());
    let project = tempdir().unwrap();

    let plan = begin(project.path(), &source, false).unwrap();
    let first_warning = finish(project.path(), &source, plan, false).await.unwrap();
    assert!(first_warning.contains("manifest.json"));
    assert!(failure_path(project.path(), &sha).exists());
    std::fs::rename(root.join(".git"), root.join(".git-hidden")).unwrap();

    let plan = begin(project.path(), &source, false).unwrap();
    assert!(matches!(plan, GuidancePlan::Failed(_)));
    let second_warning = finish(project.path(), &source, plan, false).await.unwrap();
    assert_eq!(first_warning, second_warning);
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: unavailable"));
    assert!(active.contains(sha.as_str()));
}

#[test]
fn failure_marker_write_error_does_not_abort_reconcile() {
    let project = tempdir().unwrap();
    let sha = FullSha::parse("0123456789012345678901234567890123456789").unwrap();
    let source = local_source(Path::new("/unused"), sha.clone());
    let cache = cache_path(project.path(), &sha);
    std::fs::create_dir_all(&cache).unwrap();
    std::fs::write(cache.join("manifest.json"), "malformed cache").unwrap();
    let guidance_dir = project.path().join(".neo/agent-guidance");
    std::fs::create_dir_all(&guidance_dir).unwrap();
    std::fs::write(guidance_dir.join("failures"), "blocks failure markers").unwrap();

    let plan = begin(project.path(), &source, false).unwrap();
    let GuidancePlan::Failed(warning) = plan else {
        panic!("malformed cache should stay unavailable when its failure marker cannot be written")
    };

    assert!(warning.contains("neo guidance retry"));
    assert!(warning.contains("could not record"));
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: unavailable"));
    assert!(active.contains(sha.as_str()));
}

#[tokio::test]
async fn failed_resolution_keeps_cached_new_sha_unavailable_and_generated_files_old() {
    let (repo, old_sha) = repository();
    let root = repo.path();
    write_corpus(root, "new source guidance");
    git(root, &["add", "docs/agent"]);
    git(root, &["commit", "-qm", "new framework source"]);
    let new_sha = FullSha::parse(&git(root, &["rev-parse", "HEAD"])).unwrap();
    let new_source = local_source(root, new_sha.clone());
    let project = tempdir().unwrap();

    let plan = begin(project.path(), &new_source, false).unwrap();
    assert!(
        finish(project.path(), &new_source, plan, false)
            .await
            .is_none()
    );
    write_ready(&active_path(project.path()), &old_sha).unwrap();
    std::fs::write(project.path().join("flake.nix"), "old flake\n").unwrap();
    std::fs::write(project.path().join("cabal.project"), "old cabal project\n").unwrap();

    let config = crate::config::NeoConfig {
        name: "project".to_string(),
        version: "0.1.0".to_string(),
        neo_version: new_sha.as_str().to_string(),
        dependencies: [("bad".to_string(), "svn:unsupported".to_string())]
            .into_iter()
            .collect(),
        ..crate::config::NeoConfig::default()
    };
    let result = crate::reconcile::run_locked(project.path(), &config, false).await;
    assert!(result.is_err());

    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: unavailable"));
    assert!(active.contains(new_sha.as_str()));
    assert!(!active.contains(&format!("cache/{}/index.md", new_sha.as_str())));
    assert_eq!(
        std::fs::read_to_string(project.path().join("flake.nix")).unwrap(),
        "old flake\n"
    );
    assert_eq!(
        std::fs::read_to_string(project.path().join("cabal.project")).unwrap(),
        "old cabal project\n"
    );
}

#[test]
fn offline_full_sha_keeps_source_identity_and_actionable_index() {
    let previous = std::env::var_os("NEO_SKIP_NETWORK");
    unsafe {
        std::env::set_var("NEO_SKIP_NETWORK", "1");
    }
    let sha = FullSha::parse("0123456789012345678901234567890123456789").unwrap();
    let source = FrameworkSource::Commit {
        sha: sha.clone(),
        origin: FrameworkOrigin::Upstream,
    };
    let project = tempdir().unwrap();

    let plan = begin(project.path(), &source, false).unwrap();
    assert!(matches!(plan, GuidancePlan::Offline(_)));
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains(sha.as_str()));
    assert!(active.contains("`NEO_SKIP_NETWORK` is set"));
    assert!(active.contains("neo guidance retry"));

    unsafe {
        match previous {
            Some(value) => std::env::set_var("NEO_SKIP_NETWORK", value),
            None => std::env::remove_var("NEO_SKIP_NETWORK"),
        }
    }
}

#[test]
fn manifest_rejects_missing_index_and_escaping_pages() {
    let missing_index = br#"{"schema":1,"pages":["topic.md"],"capabilities":[]}"#;
    assert!(
        parse_manifest(missing_index)
            .unwrap_err()
            .to_string()
            .contains("index.md")
    );
    let escaping = br#"{"schema":1,"pages":["index.md","../secret.md"],"capabilities":[]}"#;
    assert!(parse_manifest(escaping).is_err());
}

#[test]
fn placeholder_index_names_the_retry_command() {
    let project = tempdir().unwrap();
    let plan = begin(project.path(), &FrameworkSource::OfflinePlaceholder, false).unwrap();
    assert_eq!(plan, GuidancePlan::Placeholder);
    let active = std::fs::read_to_string(active_path(project.path())).unwrap();
    assert!(active.contains("status: unavailable"));
    assert!(active.contains("neo guidance retry"));
}
