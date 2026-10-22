use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

use serde::{Deserialize, Serialize};
use sha1::{Digest as _, Sha1};

use crate::reconcile::source::FullSha;

use super::{MAX_BLOB_BYTES, MAX_CORPUS_BYTES, MAX_CORPUS_FILES};

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct CorpusManifest {
    pub(super) schema: u32,
    pub(super) pages: Vec<String>,
    capabilities: Vec<Capability>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Capability {
    name: String,
    page: String,
    source: String,
    scenario: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct CachedObjects {
    pub(super) schema: u32,
    pub(super) source_sha: String,
    pub(super) objects: BTreeMap<String, String>,
}

#[derive(Debug)]
pub(super) struct TreeEntry {
    pub(super) oid: String,
    pub(super) path: String,
}

pub(super) fn validate_cache(cache: &Path, sha: &FullSha) -> miette::Result<()> {
    let metadata = std::fs::symlink_metadata(cache).map_err(|error| {
        miette::miette!("reading guidance cache `{}`: {error}", cache.display())
    })?;
    if !metadata.file_type().is_dir() || metadata.file_type().is_symlink() {
        return Err(miette::miette!(
            "guidance cache `{}` is not a plain directory",
            cache.display()
        ));
    }
    let index = cache.join(".neo-source-objects.json");
    let index_metadata = std::fs::symlink_metadata(&index).map_err(|error| {
        miette::miette!("reading cache object map `{}`: {error}", index.display())
    })?;
    if !index_metadata.file_type().is_file()
        || index_metadata.file_type().is_symlink()
        || index_metadata.len() > 256 * 1024
    {
        return Err(miette::miette!(
            "guidance cache object map `{}` is not a bounded regular file",
            index.display()
        ));
    }
    let objects: CachedObjects =
        serde_json::from_slice(&std::fs::read(&index).map_err(|error| {
            miette::miette!("reading cache object map `{}`: {error}", index.display())
        })?)
        .map_err(|error| {
            miette::miette!("parsing cache object map `{}`: {error}", index.display())
        })?;
    if objects.schema != 2 {
        return Err(miette::miette!(
            "unsupported guidance cache object map schema {}",
            objects.schema
        ));
    }
    if objects.source_sha != sha.as_str() {
        return Err(miette::miette!(
            "guidance cache source `{}` does not match requested framework commit `{sha}`",
            objects.source_sha
        ));
    }
    let manifest_metadata = std::fs::symlink_metadata(cache.join("manifest.json"))
        .map_err(|error| miette::miette!("reading cached guidance manifest: {error}"))?;
    if manifest_metadata.len() > MAX_BLOB_BYTES as u64 {
        return Err(miette::miette!(
            "cached guidance manifest exceeds the size limit"
        ));
    }
    let manifest_bytes = read_cached_file(cache, "manifest.json", &objects.objects)?;
    let manifest = parse_manifest(&manifest_bytes)?;
    let mut expected = BTreeSet::from(["manifest.json".to_string()]);
    expected.extend(manifest.pages.iter().cloned());
    if objects.objects.keys().cloned().collect::<BTreeSet<_>>() != expected {
        return Err(miette::miette!(
            "guidance cache object map does not match its manifest"
        ));
    }
    let mut expected_files = expected;
    expected_files.insert(".neo-source-objects.json".to_string());
    let mut actual_files = BTreeSet::new();
    for entry in walkdir::WalkDir::new(cache)
        .min_depth(1)
        .follow_links(false)
    {
        let entry = entry.map_err(|error| {
            miette::miette!("walking guidance cache `{}`: {error}", cache.display())
        })?;
        let relative = entry
            .path()
            .strip_prefix(cache)
            .expect("guidance cache traversal remains under root");
        if entry.file_type().is_symlink() {
            return Err(miette::miette!(
                "guidance cache contains a symbolic link at `{}`",
                relative.display()
            ));
        }
        if entry.file_type().is_file() {
            let path = relative
                .to_str()
                .ok_or_else(|| miette::miette!("guidance cache contains a non-UTF-8 path"))?;
            actual_files.insert(path.replace('\\', "/"));
        } else if !entry.file_type().is_dir() {
            return Err(miette::miette!(
                "guidance cache contains a non-regular entry at `{}`",
                relative.display()
            ));
        }
    }
    if actual_files != expected_files {
        return Err(miette::miette!(
            "guidance cache contains missing or unexpected files"
        ));
    }
    let cached_bytes: u64 = expected_files
        .iter()
        .filter(|path| path.as_str() != ".neo-source-objects.json")
        .filter_map(|path| std::fs::metadata(cache.join(path)).ok())
        .map(|metadata| metadata.len())
        .sum();
    if cached_bytes > MAX_CORPUS_BYTES as u64 {
        return Err(miette::miette!(
            "guidance cache exceeds the {MAX_CORPUS_BYTES}-byte size limit"
        ));
    }
    let pages = read_pages(cache, &manifest, &objects.objects)?;
    validate_links(&manifest, &pages)?;
    if !manifest.pages.contains(&"index.md".to_string()) {
        return Err(miette::miette!(
            "guidance manifest does not list required `index.md` for `{sha}`"
        ));
    }
    Ok(())
}

fn read_cached_file(
    cache: &Path,
    relative: &str,
    objects: &BTreeMap<String, String>,
) -> miette::Result<Vec<u8>> {
    validate_corpus_path(relative)?;
    let expected_oid = objects.get(relative).ok_or_else(|| {
        miette::miette!("guidance cache is missing an object id for `{relative}`")
    })?;
    let path = cache.join(relative);
    let metadata = std::fs::symlink_metadata(&path).map_err(|error| {
        miette::miette!("reading cached guidance page `{}`: {error}", path.display())
    })?;
    if !metadata.file_type().is_file() || metadata.file_type().is_symlink() {
        return Err(miette::miette!(
            "cached guidance page `{}` is not a regular file",
            path.display()
        ));
    }
    if metadata.len() as usize > MAX_BLOB_BYTES {
        return Err(miette::miette!(
            "cached guidance page `{relative}` exceeds the size limit"
        ));
    }
    let bytes = std::fs::read(&path).map_err(|error| {
        miette::miette!("reading cached guidance page `{}`: {error}", path.display())
    })?;
    if git_blob_oid(&bytes) != *expected_oid {
        return Err(miette::miette!(
            "cached guidance page `{relative}` does not match its source object"
        ));
    }
    Ok(bytes)
}

pub(super) fn parse_manifest(bytes: &[u8]) -> miette::Result<CorpusManifest> {
    let manifest: CorpusManifest = serde_json::from_slice(bytes)
        .map_err(|error| miette::miette!("parsing `docs/agent/manifest.json`: {error}"))?;
    if manifest.schema != 1 {
        return Err(miette::miette!(
            "unsupported guidance manifest schema {}; expected 1",
            manifest.schema
        ));
    }
    if manifest.pages.len() > MAX_CORPUS_FILES || manifest.pages.is_empty() {
        return Err(miette::miette!(
            "guidance manifest must list between 1 and {MAX_CORPUS_FILES} pages"
        ));
    }
    let mut pages: BTreeSet<String> = BTreeSet::new();
    for page in &manifest.pages {
        validate_corpus_path(page)?;
        if !page.ends_with(".md") {
            return Err(miette::miette!(
                "guidance page `{page}` must be a Markdown file"
            ));
        }
        if !pages.insert(page.clone()) {
            return Err(miette::miette!(
                "guidance manifest lists `{page}` more than once"
            ));
        }
    }
    if !pages.contains("index.md") {
        return Err(miette::miette!(
            "guidance manifest must list required `index.md`"
        ));
    }
    let mut capabilities: BTreeSet<String> = BTreeSet::new();
    for capability in &manifest.capabilities {
        if capability.name.trim().is_empty()
            || capability.source.trim().is_empty()
            || capability.scenario.trim().is_empty()
        {
            return Err(miette::miette!(
                "guidance capability fields must be nonempty"
            ));
        }
        if !pages.contains(&capability.page) {
            return Err(miette::miette!(
                "capability `{}` points to unlisted page `{}`",
                capability.name,
                capability.page
            ));
        }
        if !capabilities.insert(capability.name.clone()) {
            return Err(miette::miette!(
                "guidance manifest repeats capability `{}`",
                capability.name
            ));
        }
    }
    Ok(manifest)
}

pub(super) fn validate_corpus_path(path: &str) -> miette::Result<()> {
    if path.is_empty() || path.starts_with('/') || path.contains('\\') {
        return Err(miette::miette!("unsafe guidance corpus path `{path}`"));
    }
    if path
        .split('/')
        .any(|part| part.is_empty() || part == "." || part == "..")
    {
        return Err(miette::miette!("unsafe guidance corpus path `{path}`"));
    }
    let parsed = Path::new(path);
    if parsed
        .components()
        .any(|component| !matches!(component, Component::Normal(_)))
    {
        return Err(miette::miette!("unsafe guidance corpus path `{path}`"));
    }
    Ok(())
}

fn read_pages(
    root: &Path,
    manifest: &CorpusManifest,
    objects: &BTreeMap<String, String>,
) -> miette::Result<BTreeMap<String, String>> {
    let mut pages = BTreeMap::new();
    for page in &manifest.pages {
        let bytes = read_cached_file(root, page, objects)?;
        let body = String::from_utf8(bytes)
            .map_err(|error| miette::miette!("guidance page `{page}` is not UTF-8: {error}"))?;
        pages.insert(page.clone(), body);
    }
    Ok(pages)
}

pub(super) fn validate_links(
    manifest: &CorpusManifest,
    pages: &BTreeMap<String, String>,
) -> miette::Result<()> {
    let listed: BTreeSet<&str> = manifest.pages.iter().map(String::as_str).collect();
    for (page, body) in pages {
        for destination in markdown_links(body) {
            let destination = destination
                .trim()
                .trim_start_matches('<')
                .trim_end_matches('>');
            if destination.is_empty()
                || destination.starts_with('#')
                || is_external_link(destination)
            {
                continue;
            }
            let path_part = destination.split(['#', '?']).next().unwrap_or("");
            if path_part.is_empty() {
                continue;
            }
            if path_part.starts_with('/') || path_part.contains('\\') {
                return Err(miette::miette!(
                    "guidance page `{page}` links outside its corpus with `{destination}`"
                ));
            }
            let joined = Path::new(page)
                .parent()
                .unwrap_or_else(|| Path::new(""))
                .join(path_part);
            let normalized = normalize_relative(&joined).ok_or_else(|| {
                miette::miette!("guidance page `{page}` has an escaping link `{destination}`")
            })?;
            if !listed.contains(normalized.as_str()) {
                return Err(miette::miette!(
                    "guidance page `{page}` links to unlisted page `{normalized}`"
                ));
            }
        }
    }
    Ok(())
}

fn markdown_links(body: &str) -> Vec<String> {
    let mut links = Vec::new();
    let mut in_fence = false;
    for line in body.lines() {
        if line.trim_start().starts_with("```") || line.trim_start().starts_with("~~~") {
            in_fence = !in_fence;
            continue;
        }
        if in_fence {
            continue;
        }
        let mut rest = line;
        while let Some(start) = rest.find("](") {
            rest = &rest[start + 2..];
            let Some(end) = rest.find(')') else { break };
            let value = rest[..end].split_whitespace().next().unwrap_or("");
            links.push(value.to_string());
            rest = &rest[end + 1..];
        }
    }
    links
}

fn is_external_link(value: &str) -> bool {
    value.starts_with("//")
        || ["http:", "https:", "mailto:", "data:", "tel:", "ftp:"]
            .iter()
            .any(|prefix| value.starts_with(prefix))
}

fn normalize_relative(path: &Path) -> Option<String> {
    let mut parts = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::Normal(part) => parts.push(part.to_str()?.to_string()),
            Component::ParentDir => {
                parts.pop()?;
            }
            Component::RootDir | Component::Prefix(_) => return None,
        }
    }
    Some(parts.join("/"))
}

pub(super) fn git_blob_oid(bytes: &[u8]) -> String {
    let mut digest = Sha1::new();
    digest.update(format!("blob {}\0", bytes.len()).as_bytes());
    digest.update(bytes);
    format!("{:x}", digest.finalize())
}
