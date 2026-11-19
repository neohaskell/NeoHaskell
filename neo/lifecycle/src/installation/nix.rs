use crate::candidate::{MetadataError, OutputPath};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fs;
use std::io;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Path, PathBuf};

const CLIENT: &str = "/nix/var/nix/profiles/default/bin/nix";
const SYSTEM_PROFILE_CLIENT: &str = "/run/current-system/sw/bin/nix";

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum NixBlock {
    MissingClient,
    UnprovedClient,
    DaemonUnavailable,
    DaemonMismatch,
    PairAcquisition,
}

pub(super) const DAEMON: &str = "unix:///nix/var/nix/daemon-socket/socket";
const DAEMON_SOCKET: &str = "/nix/var/nix/daemon-socket/socket";

#[derive(PartialEq, Eq)]
pub(super) struct LocalDaemon {
    paths: DaemonPaths,
    store: StoreInfo,
}

#[derive(PartialEq, Eq)]
pub(super) struct DaemonPaths {
    client: SystemFile,
    socket: SystemFile,
}

#[derive(PartialEq, Eq)]
struct SystemFile {
    path: PathBuf,
    device: u64,
    inode: u64,
    changed: (i64, i64),
}

#[derive(serde::Deserialize, PartialEq, Eq)]
struct StoreInfo {
    url: String,
    version: String,
    trusted: bool,
}

impl LocalDaemon {
    pub(super) fn paths() -> std::result::Result<DaemonPaths, NixBlock> {
        let client = system_client()?;
        let socket =
            system_file(Path::new(DAEMON_SOCKET), true).map_err(|_| NixBlock::DaemonUnavailable)?;
        Ok(DaemonPaths { client, socket })
    }
}

impl DaemonPaths {
    pub(super) fn with_response(
        self,
        response: &[u8],
    ) -> std::result::Result<LocalDaemon, NixBlock> {
        let store: StoreInfo =
            serde_json::from_slice(response).map_err(|_| NixBlock::DaemonMismatch)?;
        if store.url != "daemon" || store.version.is_empty() {
            return Err(NixBlock::DaemonMismatch);
        }
        Ok(LocalDaemon { paths: self, store })
    }
}

pub(super) fn system_client_path() -> io::Result<PathBuf> {
    system_client()
        .map(|file| file.path)
        .map_err(|_| io::Error::other("unproved system Nix client"))
}

pub(super) fn legacy_store_path() -> io::Result<PathBuf> {
    let client = system_client_path()?;
    let directory = client
        .parent()
        .ok_or_else(|| io::Error::other("Nix client directory"))?;
    let sibling = directory.join("nix-store");
    if system_file(&sibling, false)?.path.parent() != Some(directory) {
        return Err(io::Error::other("unproved legacy Nix client"));
    }
    Ok(sibling)
}

fn system_client() -> std::result::Result<SystemFile, NixBlock> {
    let client = Path::new(CLIENT);
    let selected = system_file(client, false).or_else(|error| {
        if error.kind() == io::ErrorKind::NotFound
            && fs::symlink_metadata(client)
                .is_err_and(|error| error.kind() == io::ErrorKind::NotFound)
        {
            system_file(Path::new(SYSTEM_PROFILE_CLIENT), false)
        } else {
            Err(error)
        }
    });
    selected.map_err(|error| match error.kind() {
        io::ErrorKind::NotFound => NixBlock::MissingClient,
        _ => NixBlock::UnprovedClient,
    })
}

pub(super) fn system_daemon() -> io::Result<&'static str> {
    system_file(Path::new(DAEMON_SOCKET), true)?;
    Ok(DAEMON)
}

fn system_file(path: &Path, socket: bool) -> io::Result<SystemFile> {
    let mut remaining: VecDeque<_> = path.iter().map(ToOwned::to_owned).collect();
    let mut resolved = PathBuf::from("/");
    check_system_owner(&resolved, &fs::symlink_metadata(&resolved)?, false)?;
    let mut links = 0;
    while let Some(part) = remaining.pop_front() {
        match part.as_encoded_bytes() {
            b"/" => resolved = PathBuf::from("/"),
            b"." => {}
            b".." => {
                resolved.pop();
            }
            _ => {
                let next = resolved.join(part);
                let metadata = fs::symlink_metadata(&next)?;
                check_system_owner(&next, &metadata, socket && remaining.is_empty())?;
                if metadata.is_symlink() {
                    links += 1;
                    if links > 40 {
                        return Err(io::Error::other("system Nix symlink limit"));
                    }
                    let target = fs::read_link(&next)?;
                    for component in target.iter().rev() {
                        remaining.push_front(component.to_owned());
                    }
                } else {
                    resolved = next;
                }
            }
        }
    }
    let metadata = fs::symlink_metadata(&resolved)?;
    check_system_owner(&resolved, &metadata, socket)?;
    let kind_matches = if socket {
        metadata.file_type().is_socket()
    } else {
        metadata.is_file() && metadata.mode() & 0o111 != 0
    };
    if !kind_matches || path.canonicalize()? != resolved {
        return Err(io::Error::other("unproved system Nix file"));
    }
    Ok(SystemFile {
        path: resolved,
        device: metadata.dev(),
        inode: metadata.ino(),
        changed: (metadata.ctime(), metadata.ctime_nsec()),
    })
}

fn check_system_owner(path: &Path, metadata: &fs::Metadata, socket: bool) -> io::Result<()> {
    let shared_store =
        path == Path::new("/nix/store") && metadata.is_dir() && metadata.mode() & 0o1000 != 0;
    let system_runtime = cfg!(target_os = "macos")
        && path == Path::new("/private/var/run")
        && metadata.is_dir()
        && metadata.gid() == 1
        && metadata.mode() & 0o002 == 0;
    if metadata.uid() != 0
        || (metadata.mode() & 0o022 != 0
            && !metadata.is_symlink()
            && !(socket && metadata.file_type().is_socket())
            && !shared_store
            && !system_runtime)
    {
        return Err(io::Error::other("unproved system Nix ownership"));
    }
    Ok(())
}

type Result<T> = std::result::Result<T, MetadataError>;

#[derive(Debug, PartialEq, Eq)]
pub(super) struct ObservedNar {
    pub(super) hash: String,
    pub(super) size: u64,
    pub(super) references: BTreeSet<OutputPath>,
}

pub(super) fn path_info(
    bytes: &[u8],
    requested: &[OutputPath],
) -> Result<BTreeMap<OutputPath, ObservedNar>> {
    if requested.is_empty() || requested.len() > 64 {
        return Err(MetadataError::Bounds);
    }
    let value = crate::candidate::decode(bytes, 16 * 1024 * 1024, 8, 100_000, true, false)?;
    let rows = value.as_object().ok_or(MetadataError::Shape)?;
    let names: BTreeSet<_> = requested.iter().map(OutputPath::as_str).collect();
    if names.len() != requested.len() || names != rows.keys().map(String::as_str).collect() {
        return Err(MetadataError::Identity);
    }
    let mut result = BTreeMap::new();
    for path in requested {
        let row = rows[path.as_str()]
            .as_object()
            .ok_or(MetadataError::Shape)?;
        let hash = row
            .get("narHash")
            .and_then(|value| value.as_str())
            .ok_or(MetadataError::Shape)?;
        let size = row
            .get("narSize")
            .and_then(|value| value.as_u64())
            .filter(|size| *size > 0)
            .ok_or(MetadataError::Shape)?;
        let references = row
            .get("references")
            .and_then(|value| value.as_array())
            .ok_or(MetadataError::Shape)?;
        if references.len() > 512 {
            return Err(MetadataError::Bounds);
        }
        let observed = references
            .iter()
            .map(|reference| OutputPath::parse(reference.as_str().ok_or(MetadataError::Shape)?))
            .collect::<Result<BTreeSet<_>>>()?;
        if observed.len() != references.len() {
            return Err(MetadataError::Identity);
        }
        result.insert(
            path.clone(),
            ObservedNar {
                hash: hash.into(),
                size,
                references: observed,
            },
        );
    }
    Ok(result)
}

pub(super) fn converted_hashes<'a>(
    bytes: &[u8],
    hashes: &[&'a str],
) -> Result<BTreeMap<&'a str, String>> {
    if hashes.is_empty() || hashes.len() > 64 || bytes.len() > hashes.len() * 53 {
        return Err(MetadataError::Bounds);
    }
    let text = std::str::from_utf8(bytes).map_err(|_| MetadataError::Shape)?;
    if !text.ends_with('\n') {
        return Err(MetadataError::Shape);
    }
    let lines = text.lines().collect::<Vec<_>>();
    if lines.len() != hashes.len() {
        return Err(MetadataError::Identity);
    }
    let mut converted = BTreeMap::new();
    for (hash, line) in hashes.iter().zip(lines) {
        if line.len() != 52
            || !line
                .bytes()
                .all(|b| b"0123456789abcdfghijklmnpqrsvwxyz".contains(&b))
        {
            return Err(MetadataError::Shape);
        }
        if let Some(previous) = converted.insert(*hash, line.to_owned()) {
            if previous != line {
                return Err(MetadataError::Identity);
            }
        }
    }
    Ok(converted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metadata_accepts_mutable_signatures_but_rejects_missing_extra_and_duplicate_rows() {
        let path = OutputPath::parse("/nix/store/00000000000000000000000000000000-pair").unwrap();
        let row = serde_json::json!({"narHash": "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=", "narSize": 1, "references": [], "signatures": ["extra:opaque"]});
        let good = serde_json::json!({path.as_str(): row.clone()});
        let expected = std::slice::from_ref(&path);
        assert_eq!(
            path_info(&serde_json::to_vec(&good).unwrap(), expected).unwrap()[&path].size,
            1
        );
        for bad in [
            serde_json::json!({}),
            serde_json::json!({path.as_str(): null}),
            serde_json::json!({path.as_str(): row.clone(), "extra": row.clone()}),
        ] {
            assert!(path_info(&serde_json::to_vec(&bad).unwrap(), expected).is_err());
        }
        let duplicate = format!("{{{0:?}:{1},{0:?}:{1}}}", path.as_str(), row);
        assert!(path_info(duplicate.as_bytes(), expected).is_err());
    }

    #[test]
    fn maximum_reference_batch_fits_the_semantic_budget() {
        let paths: Vec<_> = (0..64)
            .map(|n| {
                OutputPath::parse(&format!(
                    "/nix/store/00000000000000000000000000000000-row-{n}"
                ))
                .unwrap()
            })
            .collect();
        let references: Vec<_> = (0..512)
            .map(|n| {
                format!(
                    "/nix/store/00000000000000000000000000000000-{}-{n:03}",
                    "r".repeat(207)
                )
            })
            .collect();
        let rows: serde_json::Map<_, _> = paths.iter().map(|path| (path.as_str().to_owned(), serde_json::json!({"narHash": "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=", "narSize": 1, "references": references, "signatures": vec!["s".repeat(256); 64]}))).collect();
        let bytes = serde_json::to_vec(&rows).unwrap();
        let observed = path_info(&bytes, &paths).unwrap();
        assert_eq!(observed.len(), 64);
        assert!(observed.values().all(|row| row.references.len() == 512));
    }

    #[test]
    fn hash_conversion_rejects_truncation_extra_rows_and_wrong_alphabet() {
        let good = format!("{}\n", "0".repeat(52));
        assert_eq!(
            converted_hashes(good.as_bytes(), &["hash"]).unwrap()["hash"],
            "0".repeat(52)
        );
        for bad in [
            good.trim_end().to_owned(),
            format!("{good}{good}"),
            format!("sha256:{good}"),
            format!("{}\n", "e".repeat(52)),
        ] {
            assert!(converted_hashes(bad.as_bytes(), &["hash"]).is_err());
        }
    }
}
