use super::process::WorkerRequest;
use crate::candidate::{self, MetadataInputs, MetadataManifest, ParsedCandidate};
use reqwest::blocking::Client;
use reqwest::{StatusCode, Url};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::io::{Read, Write};
use std::os::fd::AsFd;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

const API: &str = "https://api.github.com/repos/neohaskell/NeoHaskell/releases";
const ASSETS: &str = "https://github.com/neohaskell/NeoHaskell/releases/download";
pub(crate) const CLIENT_ARGUMENT: &str = "--internal-release-client";
const PAGE_LIMIT: usize = 4 * 1024 * 1024;
const CLIENT_REQUEST_LIMIT: usize = 16 * 1024;

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct PinnedSelection {
    pub(super) version: String,
    pub(super) candidate_id: String,
}

pub(super) struct Snapshot {
    pub(super) core: Vec<u8>,
    pub(super) files: BTreeMap<String, Vec<u8>>,
}

impl Snapshot {
    pub(super) fn parse(&self) -> Result<ParsedCandidate, ReleaseError> {
        candidate::parse(MetadataInputs {
            core: &self.core,
            files: self
                .files
                .iter()
                .map(|(name, bytes)| (name.as_str(), bytes.as_slice()))
                .collect(),
        })
        .map_err(|_| ReleaseError::Metadata)
    }
}

#[derive(Debug)]
pub(super) enum ReleaseError {
    Network,
    Missing,
    Bounds,
    Metadata,
    Identity,
    NoCompleteRelease,
    Cancelled,
    ContainmentLost,
}

impl ReleaseError {
    pub(super) fn transaction(self, label: &'static str) -> super::TransactionError {
        match self {
            Self::Cancelled => super::TransactionError::StopRequested,
            Self::ContainmentLost => super::TransactionError::CleanupUnknown,
            _ => super::TransactionError::Verification(label),
        }
    }
}

pub(super) struct ReleaseSource {
    effects: super::Effects,
    target: &'static str,
    #[cfg(test)]
    authority: Option<FixtureAuthority>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) enum ReleaseFetch {
    Page {
        page: u8,
    },
    Core {
        version: String,
    },
    Metadata {
        version: String,
        name: String,
        size: usize,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FetchRequest {
    pub(super) fetch: ReleaseFetch,
    #[cfg(test)]
    pub(super) authority: Option<FixtureAuthority>,
}

#[cfg(test)]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) enum FixtureAuthority {
    Loopback { port: u16 },
    BlockedResolver,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ClientRequest {
    lease: PathBuf,
    request: FetchRequest,
}

fn stable(version: &str) -> Result<(), ReleaseError> {
    let parsed = semver::Version::parse(version).map_err(|_| ReleaseError::Identity)?;
    if parsed.to_string() != version || !parsed.pre.is_empty() || !parsed.build.is_empty() {
        return Err(ReleaseError::Identity);
    }
    Ok(())
}

impl FetchRequest {
    pub(super) fn limit(&self) -> Result<usize, ReleaseError> {
        match &self.fetch {
            ReleaseFetch::Page { page } if (1..=20).contains(page) => Ok(PAGE_LIMIT),
            ReleaseFetch::Page { .. } => Err(ReleaseError::Bounds),
            ReleaseFetch::Core { version } => {
                stable(version)?;
                candidate::release_core_limit().map_err(|_| ReleaseError::Bounds)
            }
            ReleaseFetch::Metadata {
                version,
                name,
                size,
            } => {
                stable(version)?;
                let maximum =
                    candidate::release_metadata_limit(name).map_err(|_| ReleaseError::Identity)?;
                if *size == 0 || *size > maximum {
                    return Err(ReleaseError::Bounds);
                }
                Ok(*size)
            }
        }
    }

    fn url(&self) -> Result<Url, ReleaseError> {
        self.limit()?;
        let text = match &self.fetch {
            ReleaseFetch::Page { page } => format!("{API}?per_page=100&page={page}"),
            ReleaseFetch::Core { version } => format!("{ASSETS}/neo-v{version}/candidate.json"),
            ReleaseFetch::Metadata { version, name, .. } => {
                format!("{ASSETS}/neo-v{version}/{name}")
            }
        };
        let url = Url::parse(&text).map_err(|_| ReleaseError::Identity)?;
        #[cfg(test)]
        if let Some(FixtureAuthority::Loopback { port }) = self.authority {
            let mut local = Url::parse(&format!("http://127.0.0.1:{port}"))
                .map_err(|_| ReleaseError::Identity)?;
            local.set_path(url.path());
            local.set_query(url.query());
            return Ok(local);
        }
        Ok(url)
    }
}

pub(super) fn client_command(request: &FetchRequest, lease: &Path) -> std::io::Result<Command> {
    request
        .limit()
        .map_err(|_| std::io::Error::other("release request"))?;
    let encoded = serde_json::to_string(&ClientRequest {
        lease: lease.to_owned(),
        request: request.clone(),
    })?;
    if encoded.len() > CLIENT_REQUEST_LIMIT {
        return Err(std::io::Error::other("release request bounds"));
    }
    let mut command = Command::new(std::env::current_exe()?);
    #[cfg(not(test))]
    command.args([CLIENT_ARGUMENT, &encoded]);
    #[cfg(test)]
    command.args([
        "--exact",
        "installation::release::tests::client",
        "--ignored",
        "--nocapture",
        "--quiet",
        "--skip",
        &encoded,
    ]);
    Ok(command)
}

pub(crate) fn run_client(encoded: &std::ffi::OsStr) -> i32 {
    let result = (|| {
        let encoded = encoded.to_str().ok_or(ReleaseError::Identity)?;
        if encoded.len() > CLIENT_REQUEST_LIMIT {
            return Err(ReleaseError::Bounds);
        }
        let request: ClientRequest =
            serde_json::from_str(encoded).map_err(|_| ReleaseError::Identity)?;
        let lease = std::fs::File::from(
            std::io::stdin()
                .as_fd()
                .try_clone_to_owned()
                .map_err(|_| ReleaseError::Identity)?,
        );
        super::process::validate_inherited_lease(&lease, &request.lease)
            .map_err(|_| ReleaseError::Identity)?;
        #[cfg(test)]
        std::fs::write(
            request.lease.parent().unwrap().join("release-client.json"),
            format!(
                "[{}, {}, {}]",
                std::process::id(),
                unsafe { libc::getpgrp() },
                unsafe { libc::getppid() }
            ),
        )
        .map_err(|_| ReleaseError::Identity)?;
        let bytes = transport(&request.request)?;
        std::io::stdout()
            .write_all(&bytes)
            .map_err(|_| ReleaseError::Network)?;
        drop(lease);
        Ok(())
    })();
    match result {
        Ok(()) => 0,
        Err(ReleaseError::Missing) => 40,
        Err(ReleaseError::Bounds) => 41,
        Err(ReleaseError::Identity) => 42,
        Err(_) => 43,
    }
}

#[cfg(test)]
struct BlockedResolver;

#[cfg(test)]
impl reqwest::dns::Resolve for BlockedResolver {
    fn resolve(&self, _name: reqwest::dns::Name) -> reqwest::dns::Resolving {
        std::thread::spawn(|| {
            std::fs::write("resolver-entered", b"synthetic blocked resolver thread").unwrap();
            loop {
                std::thread::park();
            }
        });
        Box::pin(std::future::pending())
    }
}

fn transport(request: &FetchRequest) -> Result<Vec<u8>, ReleaseError> {
    let limit = request.limit()?;
    let url = request.url()?;
    let local = cfg!(test) && {
        #[cfg(test)]
        {
            matches!(request.authority, Some(FixtureAuthority::Loopback { .. }))
        }
        #[cfg(not(test))]
        {
            false
        }
    };
    let builder = Client::builder().https_only(!local);
    #[cfg(test)]
    let builder = if matches!(request.authority, Some(FixtureAuthority::BlockedResolver)) {
        builder.dns_resolver(std::sync::Arc::new(BlockedResolver))
    } else {
        builder
    };
    let client = builder
        .redirect(reqwest::redirect::Policy::custom(|attempt| {
            let url = attempt.url();
            let allowed = matches!(
                url.host_str(),
                Some(
                    "github.com"
                        | "api.github.com"
                        | "release-assets.githubusercontent.com"
                        | "objects.githubusercontent.com"
                )
            );
            if attempt.previous().len() >= 5
                || url.scheme() != "https"
                || !allowed
                || !url.username().is_empty()
                || url.password().is_some()
                || url.port().is_some_and(|port| port != 443)
            {
                attempt.error("release redirect refused")
            } else {
                attempt.follow()
            }
        }))
        .timeout(Duration::from_secs(120))
        .connect_timeout(Duration::from_secs(15))
        .user_agent(concat!("neo-installer/", env!("NEO_PRODUCT_VERSION")))
        .build()
        .map_err(|_| ReleaseError::Network)?;
    let mut response = client.get(url).send().map_err(|_| ReleaseError::Network)?;
    if response.status() == StatusCode::NOT_FOUND {
        return Err(ReleaseError::Missing);
    }
    if !response.status().is_success() {
        return Err(ReleaseError::Network);
    }
    if response
        .content_length()
        .is_some_and(|size| size > limit as u64)
    {
        return Err(ReleaseError::Bounds);
    }
    let mut bytes = Vec::new();
    Read::by_ref(&mut response)
        .take(limit as u64 + 1)
        .read_to_end(&mut bytes)
        .map_err(|_| ReleaseError::Network)?;
    if bytes.len() > limit {
        return Err(ReleaseError::Bounds);
    }
    Ok(bytes)
}

#[derive(Deserialize)]
struct Release {
    tag_name: String,
    draft: bool,
    prerelease: bool,
    assets: Vec<Asset>,
}

#[derive(Deserialize)]
struct Asset {
    name: String,
    size: usize,
    state: String,
}

impl ReleaseSource {
    #[cfg(test)]
    pub(super) fn bootstrap_fixture(
        target: &'static str,
        lease: &super::Effects,
        port: u16,
    ) -> Self {
        Self {
            effects: lease.clone(),
            target,
            authority: Some(FixtureAuthority::Loopback { port }),
        }
    }

    #[cfg(test)]
    pub(super) fn unavailable_fixture(lease: &super::Effects) -> Self {
        Self {
            effects: lease.clone(),
            target: "aarch64-apple-darwin",
            authority: Some(FixtureAuthority::Loopback { port: 9 }),
        }
    }

    #[cfg(test)]
    pub(super) fn owned_fixture(target: &'static str, lease: &super::Effects) -> Self {
        Self {
            effects: lease.clone(),
            target,
            authority: Some(FixtureAuthority::Loopback { port: 18766 }),
        }
    }

    pub(super) fn production(
        target: &'static str,
        lease: &super::Effects,
    ) -> Result<Self, ReleaseError> {
        Ok(Self {
            effects: lease.clone(),
            target,
            #[cfg(test)]
            authority: None,
        })
    }

    async fn get(&self, fetch: ReleaseFetch) -> Result<Vec<u8>, ReleaseError> {
        let request = FetchRequest {
            fetch,
            #[cfg(test)]
            authority: self.authority.clone(),
        };
        request.limit()?;
        let result = self
            .effects
            .process(WorkerRequest::Release(request))
            .await
            .map_err(|error| match error {
                super::TransactionError::StopRequested => ReleaseError::Cancelled,
                super::TransactionError::CleanupUnknown => ReleaseError::ContainmentLost,
                _ => ReleaseError::Network,
            })?;
        if result.failure.is_some() {
            return Err(ReleaseError::Network);
        }
        match result.exit_code {
            Some(0) if result.stderr.is_empty() => Ok(result.stdout),
            Some(40) => Err(ReleaseError::Missing),
            Some(41) => Err(ReleaseError::Bounds),
            Some(42) => Err(ReleaseError::Identity),
            _ => Err(ReleaseError::Network),
        }
    }

    pub(super) async fn core(
        &self,
        version: &str,
    ) -> Result<(Vec<u8>, MetadataManifest), ReleaseError> {
        let core = self
            .get(ReleaseFetch::Core {
                version: version.into(),
            })
            .await?;
        let manifest =
            candidate::metadata_manifest(&core, self.target).map_err(|_| ReleaseError::Metadata)?;
        if manifest.version != version {
            return Err(ReleaseError::Identity);
        }
        Ok((core, manifest))
    }

    pub(super) async fn select_exact(
        &self,
        version: &str,
    ) -> Result<PinnedSelection, ReleaseError> {
        let (_, manifest) = self.core(version).await?;
        Ok(PinnedSelection {
            version: manifest.version,
            candidate_id: manifest.candidate_id,
        })
    }

    pub(super) async fn file(
        &self,
        selected: &PinnedSelection,
        file: &candidate::FileIdentity,
    ) -> Result<Vec<u8>, ReleaseError> {
        let bytes = self
            .get(ReleaseFetch::Metadata {
                version: selected.version.clone(),
                name: file.name.clone(),
                size: file.size,
            })
            .await?;
        file.verify(&bytes).map_err(|_| ReleaseError::Identity)?;
        Ok(bytes)
    }

    #[cfg(test)]
    pub(super) async fn snapshot(
        &self,
        selected: &PinnedSelection,
    ) -> Result<Snapshot, ReleaseError> {
        let (core, manifest) = self.core(&selected.version).await?;
        if manifest.candidate_id != selected.candidate_id {
            return Err(ReleaseError::Identity);
        }
        let mut files = BTreeMap::new();
        for file in manifest.metadata {
            let bytes = self
                .get(ReleaseFetch::Metadata {
                    version: selected.version.clone(),
                    name: file.name.clone(),
                    size: file.size,
                })
                .await?;
            file.verify(&bytes).map_err(|_| ReleaseError::Identity)?;
            files.insert(file.name, bytes);
        }
        let snapshot = Snapshot { core, files };
        snapshot.parse()?;
        Ok(snapshot)
    }

    pub(super) async fn select_latest(&self) -> Result<PinnedSelection, ReleaseError> {
        let core_limit = candidate::release_core_limit().map_err(|_| ReleaseError::Bounds)?;
        let mut versions = Vec::new();
        for page in 1..=20 {
            let bytes = self.get(ReleaseFetch::Page { page }).await?;
            let releases: Vec<Release> =
                serde_json::from_slice(&bytes).map_err(|_| ReleaseError::Metadata)?;
            if releases.len() > 100 {
                return Err(ReleaseError::Bounds);
            }
            let finished = releases.len() < 100;
            for release in releases {
                if release.draft || release.prerelease {
                    continue;
                }
                let Some(text) = release.tag_name.strip_prefix("neo-v") else {
                    continue;
                };
                let Ok(version) = semver::Version::parse(text) else {
                    continue;
                };
                if version.pre.is_empty() && version.build.is_empty() && version.to_string() == text
                {
                    versions.push((version, release.assets));
                }
            }
            if finished {
                break;
            }
            if page == 20 {
                return Err(ReleaseError::Bounds);
            }
        }
        versions.sort_unstable_by(|left, right| left.0.cmp(&right.0));
        for (version, assets) in versions.into_iter().rev() {
            if !assets.iter().any(|asset| {
                asset.name == "candidate.json"
                    && asset.state == "uploaded"
                    && asset.size <= core_limit
            }) {
                continue;
            }
            let (core, manifest) = match self.core(&version.to_string()).await {
                Ok(value) => value,
                Err(ReleaseError::Missing | ReleaseError::Metadata | ReleaseError::Identity) => {
                    continue;
                }
                Err(error) => return Err(error),
            };
            let mut published = BTreeMap::new();
            for asset in assets {
                if asset.state != "uploaded" || published.insert(asset.name, asset.size).is_some() {
                    return Err(ReleaseError::Metadata);
                }
            }
            if published.get("candidate.json") != Some(&core.len())
                || !manifest
                    .required_assets
                    .iter()
                    .all(|file| published.get(&file.name) == Some(&file.size))
            {
                continue;
            }
            return Ok(PinnedSelection {
                version: manifest.version,
                candidate_id: manifest.candidate_id,
            });
        }
        Err(ReleaseError::NoCompleteRelease)
    }
}

#[cfg(test)]
mod tests {
    use super::super::RootLease;
    use super::super::process::{OperationOwner, ProcessPoll};
    use super::*;
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpListener;
    use std::path::Path;
    use std::sync::{Arc, Mutex};

    #[test]
    #[ignore]
    fn client() {
        let encoded = std::env::args_os().last().unwrap();
        std::process::exit(run_client(&encoded));
    }

    fn setup() -> (tempfile::TempDir, RootLease) {
        use std::os::unix::fs::PermissionsExt;
        let temporary = tempfile::tempdir().unwrap();
        std::fs::set_permissions(temporary.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
        let lease =
            RootLease::acquire(&temporary.path().canonicalize().unwrap().join("lock")).unwrap();
        (temporary, lease)
    }

    fn busy(path: &Path) -> bool {
        matches!(
            RootLease::acquire(path),
            Err(super::super::TransactionError::Busy)
        )
    }

    fn absent(pid: i32) {
        assert_eq!(
            unsafe { libc::kill(pid, 0) },
            -1,
            "process or group still exists {pid}"
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ESRCH)
        );
    }

    fn absence_receipt(directory: &Path) {
        let ids: [i32; 3] =
            serde_json::from_slice(&std::fs::read(directory.join("release-client.json")).unwrap())
                .unwrap();
        absent(ids[0]);
        absent(-ids[1]);
        absent(ids[2]);
        println!(
            "release absence receipt client={} group={} worker={}",
            ids[0], ids[1], ids[2]
        );
    }

    fn finish(
        owner: &mut OperationOwner,
        lock: &Path,
    ) -> std::io::Result<super::super::process::OperationResult> {
        let deadline = std::time::Instant::now() + Duration::from_secs(15);
        loop {
            assert!(
                std::time::Instant::now() < deadline,
                "release completion deadline"
            );
            match owner.poll() {
                ProcessPoll::Pending => {
                    assert!(busy(lock));
                    owner.wait_for_interest();
                }
                ProcessPoll::Quiescent(result) => return result,
                ProcessPoll::ContainmentLost(error) => panic!("containment lost {error}"),
                ProcessPoll::Ready => panic!("request not submitted"),
            }
        }
    }

    fn request(port: u16, fetch: ReleaseFetch) -> FetchRequest {
        FetchRequest {
            fetch,
            authority: Some(FixtureAuthority::Loopback { port }),
        }
    }

    fn read_request(stream: &std::net::TcpStream) -> String {
        stream
            .set_read_timeout(Some(Duration::from_secs(5)))
            .unwrap();
        let mut reader = BufReader::new(stream.try_clone().unwrap());
        let mut request = String::new();
        reader.read_line(&mut request).unwrap();
        loop {
            let mut line = String::new();
            reader.read_line(&mut line).unwrap();
            if line == "\r\n" {
                break;
            }
            assert!(!line.is_empty());
        }
        request
    }

    #[test]
    fn contained_responses_enforce_status_body_bounds_and_redirect_policy() {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let valid = vec![b'x'; 256 * 1024];
        let cases = [
            (
                format!("HTTP/1.1 200 OK\r\nContent-Length: {}\r\n\r\n", valid.len()),
                valid.clone(),
                0,
            ),
            (
                format!(
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\n\r\n",
                    PAGE_LIMIT + 1
                ),
                Vec::new(),
                41,
            ),
            (
                "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n".into(),
                vec![b'x'; PAGE_LIMIT + 1],
                41,
            ),
            (
                "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\n".into(),
                b"short".to_vec(),
                43,
            ),
            (
                "HTTP/1.1 404 Not Found\r\nContent-Length: 6\r\n\r\n".into(),
                b"secret".to_vec(),
                40,
            ),
            (
                "HTTP/1.1 500 Failure\r\nContent-Length: 6\r\n\r\n".into(),
                b"secret".to_vec(),
                43,
            ),
            (
                format!(
                    "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:{port}/unsafe\r\nContent-Length: 0\r\n\r\n"
                ),
                Vec::new(),
                43,
            ),
        ];
        for (header, body, expected) in cases {
            let (temporary, lease) = setup();
            let lock = lease.path.clone();
            let mut owner = OperationOwner::start(&lease).unwrap();
            let listener = listener.try_clone().unwrap();
            let server = std::thread::spawn(move || {
                let (mut stream, _) = listener.accept().unwrap();
                let request = read_request(&stream);
                assert!(request.contains("?per_page=100&page=2"));
                stream
                    .set_write_timeout(Some(Duration::from_secs(5)))
                    .unwrap();
                let _ = stream.write_all(header.as_bytes());
                let _ = stream.write_all(&body);
            });
            owner
                .submit(WorkerRequest::Release(request(
                    port,
                    ReleaseFetch::Page { page: 2 },
                )))
                .unwrap();
            drop(lease);
            let result = finish(&mut owner, &lock).unwrap();
            server.join().unwrap();
            assert_eq!(result.exit_code, Some(expected), "{:?}", result.failure);
            assert!(result.failure.is_none());
            assert!(result.stderr.is_empty());
            if expected == 0 {
                assert!(result.stdout == valid, "bounded valid body differs");
            } else {
                assert!(result.stdout.is_empty());
            }
            absence_receipt(temporary.path());
            assert!(!busy(&lock));
        }
    }

    #[test]
    fn maximum_metadata_body_uses_its_contract_bound() {
        let contract: serde_json::Value =
            serde_json::from_slice(include_bytes!("../../contract/registry.json")).unwrap();
        let name = contract["resolved"]["auxiliary_assets"][0]
            .as_str()
            .unwrap();
        let size = candidate::release_metadata_limit(name).unwrap();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let server = std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            read_request(&stream);
            stream
                .set_write_timeout(Some(Duration::from_secs(10)))
                .unwrap();
            write!(stream, "HTTP/1.1 200 OK\r\nContent-Length: {size}\r\n\r\n").unwrap();
            let chunk = vec![0xa7; 65536];
            for _ in 0..size / chunk.len() {
                stream.write_all(&chunk).unwrap();
            }
            stream.write_all(&chunk[..size % chunk.len()]).unwrap();
        });
        let (temporary, lease) = setup();
        let lock = lease.path.clone();
        let mut owner = OperationOwner::start(&lease).unwrap();
        owner
            .submit(WorkerRequest::Release(request(
                port,
                ReleaseFetch::Metadata {
                    version: "1.0.0".into(),
                    name: name.into(),
                    size,
                },
            )))
            .unwrap();
        drop(lease);
        let result = finish(&mut owner, &lock).unwrap();
        server.join().unwrap();
        assert_eq!(result.exit_code, Some(0));
        assert!(result.failure.is_none());
        assert_eq!(result.stdout.len(), size);
        assert!(result.stdout.iter().all(|byte| *byte == 0xa7));
        absence_receipt(temporary.path());
        assert!(!busy(&lock));
    }

    #[test]
    fn constructing_release_source_has_no_network_or_process_effect() {
        let (temporary, _lease) = setup();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        listener.set_nonblocking(true).unwrap();
        let source = ReleaseSource {
            effects: super::super::Effects::for_tests(),
            target: "aarch64-apple-darwin",
            authority: Some(FixtureAuthority::Loopback {
                port: listener.local_addr().unwrap().port(),
            }),
        };
        drop(source);
        drop(
            ReleaseSource::production("aarch64-apple-darwin", &super::super::Effects::for_tests())
                .unwrap(),
        );
        assert!(!temporary.path().join("release-client.json").exists());
        assert_eq!(
            listener.accept().unwrap_err().kind(),
            std::io::ErrorKind::WouldBlock
        );
    }

    #[test]
    fn contained_cancellation_retains_lease_until_client_group_and_worker_absence() {
        for mode in ["headers", "body", "resolver"] {
            let (temporary, lease) = setup();
            let lock = lease.path.clone();
            let listener = TcpListener::bind("127.0.0.1:0").unwrap();
            let port = listener.local_addr().unwrap().port();
            let (ready_tx, ready_rx) = std::sync::mpsc::channel();
            let server = if mode != "resolver" {
                Some(std::thread::spawn(move || {
                    let (mut stream, _) = listener.accept().unwrap();
                    read_request(&stream);
                    if mode == "body" {
                        stream
                            .write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 1048576\r\n\r\npartial")
                            .unwrap();
                    }
                    ready_tx.send(()).unwrap();
                    stream
                        .set_read_timeout(Some(Duration::from_secs(10)))
                        .unwrap();
                    let mut byte = [0];
                    assert_eq!(stream.read(&mut byte).unwrap(), 0);
                }))
            } else {
                None
            };
            let mut owner = OperationOwner::start(&lease).unwrap();
            let authority = if mode == "resolver" {
                FixtureAuthority::BlockedResolver
            } else {
                FixtureAuthority::Loopback { port }
            };
            owner
                .submit(WorkerRequest::Release(FetchRequest {
                    fetch: ReleaseFetch::Page { page: 1 },
                    authority: Some(authority),
                }))
                .unwrap();
            if mode == "resolver" {
                let deadline = std::time::Instant::now() + Duration::from_secs(10);
                let mut delay = Duration::from_millis(1);
                while !temporary.path().join("resolver-entered").exists() {
                    assert!(std::time::Instant::now() < deadline);
                    assert!(matches!(owner.poll(), ProcessPoll::Pending));
                    std::thread::sleep(delay);
                    delay *= 2;
                }
            } else {
                loop {
                    assert!(matches!(owner.poll(), ProcessPoll::Pending));
                    match ready_rx.try_recv() {
                        Ok(()) => break,
                        Err(std::sync::mpsc::TryRecvError::Empty) => owner.wait_for_interest(),
                        Err(error) => panic!("server failed {error}"),
                    }
                }
            }
            drop(lease);
            assert!(busy(&lock));
            owner.request_cancel();
            assert!(busy(&lock));
            assert_eq!(
                finish(&mut owner, &lock).unwrap_err().kind(),
                std::io::ErrorKind::Interrupted
            );
            if let Some(server) = server {
                server.join().unwrap();
            }
            absence_receipt(temporary.path());
            assert!(!busy(&lock));
            println!("contained cancellation mode={mode} complete");
        }
    }

    #[test]
    fn selection_stops_after_worker_loss_and_surviving_client_keeps_lease() {
        let (temporary, lease) = setup();
        let lock = lease.path.clone();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let server_listener = listener.try_clone().unwrap();
        let directory = temporary.path().to_owned();
        let server = std::thread::spawn(move || {
            let (mut page, _) = server_listener.accept().unwrap();
            assert!(read_request(&page).contains("page=1"));
            let releases: Vec<_> = ["2.0.0", "1.0.0"]
                .into_iter()
                .map(|version| {
                    serde_json::json!({
                        "tag_name": format!("neo-v{version}"), "draft": false, "prerelease": false,
                        "assets": [{"name": "candidate.json", "size": 1, "state": "uploaded"}]
                    })
                })
                .collect();
            let body = serde_json::to_vec(&releases).unwrap();
            write!(
                page,
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\n\r\n",
                body.len()
            )
            .unwrap();
            page.write_all(&body).unwrap();
            drop(page);
            let (mut core, _) = server_listener.accept().unwrap();
            assert!(read_request(&core).contains("/neo-v2.0.0/candidate.json"));
            let ids: [i32; 3] = serde_json::from_slice(
                &std::fs::read(directory.join("release-client.json")).unwrap(),
            )
            .unwrap();
            assert_eq!(unsafe { libc::kill(ids[2], libc::SIGKILL) }, 0);
            core.set_read_timeout(Some(Duration::from_secs(10)))
                .unwrap();
            let mut byte = [0];
            assert_eq!(core.read(&mut byte).unwrap(), 0);
        });
        let source = ReleaseSource {
            effects: super::super::Effects::for_tests(),
            target: "aarch64-apple-darwin",
            authority: Some(FixtureAuthority::Loopback { port }),
        };
        assert!(matches!(
            super::super::coordinator::test_with(&source.effects, &lease, source.select_latest()),
            Err(ReleaseError::ContainmentLost)
        ));
        drop(source);
        drop(lease);
        let ids: [i32; 3] = serde_json::from_slice(
            &std::fs::read(temporary.path().join("release-client.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(unsafe { libc::kill(ids[0], 0) }, 0);
        assert!(busy(&lock));
        absent(ids[2]);
        listener.set_nonblocking(true).unwrap();
        assert_eq!(
            listener.accept().unwrap_err().kind(),
            std::io::ErrorKind::WouldBlock
        );
        assert_eq!(unsafe { libc::kill(-ids[1], libc::SIGKILL) }, 0);
        server.join().unwrap();
        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        let mut delay = Duration::from_millis(1);
        while unsafe { libc::kill(-ids[1], 0) } == 0 {
            assert!(std::time::Instant::now() < deadline);
            std::thread::sleep(delay);
            delay *= 2;
        }
        absence_receipt(temporary.path());
        assert!(!busy(&lock));
        println!("selection containment loss stopped before older candidate request");
    }

    #[test]
    fn release_cancel_before_admission_has_no_request_or_client() {
        let (temporary, lease) = setup();
        let lock = lease.path.clone();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        listener.set_nonblocking(true).unwrap();
        let mut owner = OperationOwner::begin(&lease).unwrap();
        drop(lease);
        owner.request_cancel();
        assert_eq!(
            finish(&mut owner, &lock).unwrap_err().kind(),
            std::io::ErrorKind::Interrupted
        );
        assert!(!temporary.path().join("release-client.json").exists());
        assert_eq!(
            listener.accept().unwrap_err().kind(),
            std::io::ErrorKind::WouldBlock
        );
        assert!(!busy(&lock));
    }

    #[test]
    fn closed_fetch_rejects_unadmitted_files_versions_pages_and_bounds() {
        assert!(request(1, ReleaseFetch::Page { page: 21 }).limit().is_err());
        for version in ["1.0.0-pre", "1.0.0+build", "01.0.0", "../1.0.0"] {
            assert!(
                request(
                    1,
                    ReleaseFetch::Core {
                        version: version.into()
                    }
                )
                .limit()
                .is_err()
            );
        }
        for name in [
            "../candidate.json",
            "neo-installer-aarch64-apple-darwin",
            "unowned.json",
        ] {
            assert!(
                request(
                    1,
                    ReleaseFetch::Metadata {
                        version: "1.0.0".into(),
                        name: name.into(),
                        size: 1
                    }
                )
                .limit()
                .is_err()
            );
        }
        let contract: serde_json::Value =
            serde_json::from_slice(include_bytes!("../../contract/registry.json")).unwrap();
        for name in contract["resolved"]["auxiliary_assets"].as_array().unwrap() {
            let name = name.as_str().unwrap();
            let limit = candidate::release_metadata_limit(name).unwrap();
            assert_eq!(
                request(
                    1,
                    ReleaseFetch::Metadata {
                        version: "1.0.0".into(),
                        name: name.into(),
                        size: limit
                    }
                )
                .limit()
                .unwrap(),
                limit
            );
            assert!(
                request(
                    1,
                    ReleaseFetch::Metadata {
                        version: "1.0.0".into(),
                        name: name.into(),
                        size: limit + 1
                    }
                )
                .limit()
                .is_err()
            );
        }
    }

    #[test]
    fn pinned_snapshot_fetches_exact_files_without_latest_or_native_payloads() {
        let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("contract/fixtures/v1");
        let index: serde_json::Value =
            serde_json::from_slice(&std::fs::read(fixtures.join("fixture-set.json")).unwrap())
                .unwrap();
        let name = index["candidate_ids"]
            .as_object()
            .unwrap()
            .keys()
            .next()
            .unwrap();
        let directory = fixtures.join(name);
        let core = std::fs::read(directory.join("candidate.json")).unwrap();
        let manifest = candidate::metadata_manifest(&core, "aarch64-apple-darwin").unwrap();
        let requests = Arc::new(Mutex::new(Vec::new()));
        let recorded = requests.clone();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        let count = manifest.metadata.len() + 2;
        let server = std::thread::spawn(move || {
            for _ in 0..count {
                let (mut stream, _) = listener.accept().unwrap();
                stream
                    .set_read_timeout(Some(Duration::from_secs(5)))
                    .unwrap();
                let mut reader = BufReader::new(stream.try_clone().unwrap());
                let mut line = String::new();
                reader.read_line(&mut line).unwrap();
                let path = line.split_whitespace().nth(1).unwrap().to_owned();
                recorded.lock().unwrap().push(path.clone());
                loop {
                    line.clear();
                    reader.read_line(&mut line).unwrap();
                    if line == "\r\n" {
                        break;
                    }
                }
                let name = path.rsplit('/').next().unwrap();
                let body = std::fs::read(directory.join(name)).unwrap();
                write!(
                    stream,
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    body.len()
                )
                .unwrap();
                stream.write_all(&body).unwrap();
            }
        });
        use std::os::unix::fs::PermissionsExt;
        let temporary = tempfile::tempdir().unwrap();
        std::fs::set_permissions(temporary.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
        let lease =
            RootLease::acquire(&temporary.path().canonicalize().unwrap().join("lock")).unwrap();
        let source = ReleaseSource {
            effects: super::super::Effects::for_tests(),
            target: "aarch64-apple-darwin",
            authority: Some(FixtureAuthority::Loopback {
                port: address.port(),
            }),
        };
        let selected = super::super::coordinator::test_with(
            &source.effects,
            &lease,
            source.select_exact(&manifest.version),
        )
        .unwrap();
        let snapshot = super::super::coordinator::test_with(
            &source.effects,
            &lease,
            source.snapshot(&selected),
        )
        .unwrap();
        assert_eq!(snapshot.parse().unwrap().id(), selected.candidate_id);
        server.join().unwrap();
        let requests = requests.lock().unwrap();
        assert_eq!(requests.len(), count);
        assert!(requests.iter().all(|path| path.starts_with(&format!(
            "/neohaskell/NeoHaskell/releases/download/neo-v{}/",
            manifest.version
        ))));
        assert!(
            requests
                .iter()
                .all(|path| !path.contains("neo-installer-") && !path.ends_with("/releases"))
        );
    }
    #[test]
    fn review_inspection_process_cancel_waits_for_actual_group_absence() {
        use std::future::Future;
        let (temporary, lease) = setup();
        let lock = lease.path.clone();
        let journal = temporary.path().join("installation.json");
        std::fs::write(&journal, b"old pending journal stays unchanged").unwrap();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let (ready_tx, ready_rx) = std::sync::mpsc::channel();
        let server = std::thread::spawn(move || {
            let (mut connection, _) = listener.accept().unwrap();
            read_request(&connection);
            ready_tx.send(()).unwrap();
            connection
                .set_read_timeout(Some(Duration::from_secs(10)))
                .unwrap();
            let mut byte = [0];
            assert_eq!(connection.read(&mut byte).unwrap(), 0);
        });
        let source = ReleaseSource {
            effects: super::super::Effects::for_tests(),
            target: "aarch64-apple-darwin",
            authority: Some(FixtureAuthority::Loopback { port }),
        };
        let mut pending = Box::pin(source.get(ReleaseFetch::Page { page: 1 }));
        let mut cancelled = false;
        let result = super::super::coordinator::test_with(
            &source.effects,
            &lease,
            std::future::poll_fn(|context| {
                if !cancelled && ready_rx.try_recv().is_ok() {
                    assert!(busy(&lock));
                    source.effects.cancel_for_test();
                    cancelled = true;
                }
                pending.as_mut().poll(context)
            }),
        );
        assert!(cancelled);
        assert!(matches!(result, Err(ReleaseError::Cancelled)));
        server.join().unwrap();
        absence_receipt(temporary.path());
        assert_eq!(
            std::fs::read(journal).unwrap(),
            b"old pending journal stays unchanged"
        );
        drop(lease);
        assert!(!busy(&lock));
    }
    #[test]
    fn review_new_latest_does_not_replace_the_prepared_candidate_or_refetch_metadata() {
        use std::sync::atomic::{AtomicBool, Ordering};
        let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("contract/fixtures/v1");
        let index: serde_json::Value =
            serde_json::from_slice(&std::fs::read(fixtures.join("fixture-set.json")).unwrap())
                .unwrap();
        let directory = fixtures.join(
            index["candidate_ids"]
                .as_object()
                .unwrap()
                .keys()
                .next()
                .unwrap(),
        );
        let core = std::fs::read(directory.join("candidate.json")).unwrap();
        let manifest = candidate::metadata_manifest(&core, "aarch64-apple-darwin").unwrap();
        let original_version = manifest.version.clone();
        let expected_requests = manifest.metadata.len() + 3;
        let assets: Vec<_> = std::iter::once(
            serde_json::json!({"name": "candidate.json", "size": core.len(), "state": "uploaded"}),
        )
        .chain(manifest.required_assets.iter().map(
            |file| serde_json::json!({"name": file.name, "size": file.size, "state": "uploaded"}),
        ))
        .collect();
        let changed = Arc::new(AtomicBool::new(false));
        let server_changed = changed.clone();
        let requests = Arc::new(Mutex::new(Vec::new()));
        let server_requests = requests.clone();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        listener.set_nonblocking(true).unwrap();
        let (stop, stopped) = std::sync::mpsc::channel();
        let server = std::thread::spawn(move || {
            let deadline = std::time::Instant::now() + Duration::from_secs(20);
            while stopped.try_recv().is_err() {
                assert!(
                    std::time::Instant::now() < deadline,
                    "review metadata server deadline"
                );
                let (mut stream, _) = match listener.accept() {
                    Ok(connection) => connection,
                    Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                        std::thread::sleep(Duration::from_millis(5));
                        continue;
                    }
                    Err(error) => panic!("fixture listener failed {error}"),
                };
                stream.set_nonblocking(false).unwrap();
                let request = read_request(&stream);
                server_requests.lock().unwrap().push(request.clone());
                let body = if request.contains("page=") {
                    let version = if server_changed.load(Ordering::Acquire) {
                        "999.0.0"
                    } else {
                        &original_version
                    };
                    serde_json::to_vec(&serde_json::json!([{"tag_name": format!("neo-v{version}"), "draft": false, "prerelease": false, "assets": assets}])).unwrap()
                } else {
                    let path = request.split_whitespace().nth(1).unwrap();
                    std::fs::read(directory.join(path.rsplit('/').next().unwrap())).unwrap()
                };
                write!(
                    stream,
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    body.len()
                )
                .unwrap();
                stream.write_all(&body).unwrap();
            }
        });
        let (temporary, lease) = setup();
        let owner = super::super::InstallationOwner::new(super::super::Layout::new(
            temporary.path().canonicalize().unwrap(),
            temporary.path().canonicalize().unwrap().join("bin"),
        ));
        let source = ReleaseSource {
            effects: owner.effects.clone(),
            target: "aarch64-apple-darwin",
            authority: Some(FixtureAuthority::Loopback { port }),
        };
        assert!(!changed.load(Ordering::Acquire));
        let result = super::super::coordinator::test_with_review_signal(
            &owner.effects,
            &lease,
            Some(&changed),
            async {
                let selected = source
                    .select_latest()
                    .await
                    .map_err(|error| error.transaction("fixture latest"))?;
                let prepared = owner
                    .prepare_snapshot(&selected, &source, "aarch64-apple-darwin", false)
                    .await?;
                let identity = owner
                    .effects
                    .review(super::super::coordinator::ReviewSummary {
                        operation: super::super::coordinator::ReviewOperation::Install,
                        version: Some(selected.version.clone()),
                        recovery: super::super::coordinator::RecoveryPending::None,
                        changes: vec![super::super::coordinator::ReviewedChange::Selection],
                    })
                    .await?;
                owner.recheck_snapshot(&prepared).await?;
                owner.effects.admit_reviewed(identity)?;
                owner
                    .publish_prepared_snapshot(&selected, prepared, "aarch64-apple-darwin")
                    .await
            },
        );
        stop.send(()).unwrap();
        server.join().unwrap();
        assert!(changed.load(Ordering::Acquire), "review offer reached");
        assert_eq!(result.unwrap().id(), manifest.candidate_id);
        let requests = requests.lock().unwrap();
        assert_eq!(requests.len(), expected_requests);
        assert_eq!(
            requests
                .iter()
                .filter(|request| request.contains("page="))
                .count(),
            1
        );
        assert!(!requests.iter().any(|request| request.contains("999.0.0")));
    }
}
