use super::{InstallationOwner, TransactionError, process};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Path, PathBuf};

pub(super) const CLIENT: &str = "/nix/var/nix/profiles/default/bin/nix";
const SOCKET: &str = "/nix/var/nix/daemon-socket/socket";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct NixTargetIdentity {
    package_root: &'static str,
    client_sha256: &'static str,
    service_sha256: &'static str,
    socket_sha256: &'static str,
}

const ARM64_IDENTITY: NixTargetIdentity = NixTargetIdentity {
    package_root: "/nix/store/82nvpklg6giadsmk81fz7diiix246c0d-nix-2.34.9",
    client_sha256: "7f63128a37db39e945cbc965caa2d59fbd5b66c218295625b2080c0423ee365d",
    service_sha256: "b9ec9b86bc4f8fc03d6276151264d6dca290155404853c97ff56fb8067dd1d32",
    socket_sha256: "3802372e8ccfd129929aa0887fb3f98d7ec2977e5c7a3dc774d29875c6651b16",
};

const X86_64_GNU_IDENTITY: NixTargetIdentity = NixTargetIdentity {
    package_root: "/nix/store/ssswpqihnyi8231b9r515k2hmpnk0h1n-nix-2.34.9",
    client_sha256: "d476ea692162bcede8c46a492e0350862139c4e4a0d8a5a36bc81d340bac4c6d",
    service_sha256: "08698149a115eb07a0478cdfff9ba30ce0e41035ab18192a7a2f20af00d3608e",
    socket_sha256: "3802372e8ccfd129929aa0887fb3f98d7ec2977e5c7a3dc774d29875c6651b16",
};

fn target_identity() -> Result<&'static NixTargetIdentity, NixBlock> {
    if cfg!(all(target_os = "linux", target_arch = "aarch64")) {
        Ok(&ARM64_IDENTITY)
    } else if cfg!(all(
        target_os = "linux",
        target_arch = "x86_64",
        target_env = "gnu",
        target_pointer_width = "64"
    )) {
        Ok(&X86_64_GNU_IDENTITY)
    } else {
        Err(NixBlock::UnprovedPlatform)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum NixBlock {
    MissingClient,
    UnprovedClient,
    UnprovedPlatform,
    InactiveService,
    UnprovedService,
    DaemonUnavailable,
    DaemonMismatch,
    ConfigurationOwnerUnknown,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub(super) struct ObservedNixBase {
    client_digest: String,
    daemon: DaemonObservation,
    service: ServiceObservation,
    configuration: ConfigurationObservation,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
struct DaemonObservation {
    trusted: bool,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
struct ServiceObservation {
    main_pid: u32,
    started: u64,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
struct ConfigurationObservation {
    digest: Option<String>,
}

impl ObservedNixBase {
    pub(super) fn require_configuration_authority(&self) -> Result<(), TransactionError> {
        Err(TransactionError::NixBlocked(
            NixBlock::ConfigurationOwnerUnknown,
        ))
    }
}

impl InstallationOwner {
    pub(super) async fn observe_nix_base(
        &self,
        lease: &super::Effects,
    ) -> Result<ObservedNixBase, TransactionError> {
        use super::{FilesystemJob as Job, FilesystemResult as Result};
        let blocked = TransactionError::NixBlocked;
        if self
            .inspect_path(PathBuf::from(CLIENT))
            .await
            .map_err(|error| error.preserve_stop(blocked(NixBlock::UnprovedClient)))?
            .is_none()
        {
            return Err(blocked(NixBlock::MissingClient));
        }
        let identity = target_identity().map_err(blocked)?;
        let client = self.nix_client().await?;
        let client_digest = self
            .nix_digest(client.clone(), 8 * 1024 * 1024, NixBlock::UnprovedClient)
            .await?;
        if client_digest != identity.client_sha256 {
            return Err(blocked(NixBlock::UnprovedClient));
        }
        let configuration = ConfigurationObservation {
            digest: if self
                .inspect_path(PathBuf::from("/etc/nix/nix.conf"))
                .await
                .map_err(|error| error.preserve_stop(blocked(NixBlock::ConfigurationOwnerUnknown)))?
                .is_some()
            {
                let path = self
                    .nix_protected(
                        PathBuf::from("/etc/nix/nix.conf"),
                        NixBlock::ConfigurationOwnerUnknown,
                    )
                    .await?;
                Some(
                    self.nix_digest(path, 128 * 1024, NixBlock::ConfigurationOwnerUnknown)
                        .await?,
                )
            } else {
                None
            },
        };
        match self.effects.filesystem(Job::NixProcessName).await {
            Ok(Result::Bytes(Some(bytes))) if bytes == b"systemd\n" => {}
            Err(error) => return Err(error.preserve_stop(blocked(NixBlock::InactiveService))),
            _ => return Err(blocked(NixBlock::InactiveService)),
        }
        for (name, digest) in [
            ("nix-daemon.service", identity.service_sha256),
            ("nix-daemon.socket", identity.socket_sha256),
        ] {
            let path = self
                .nix_protected(
                    Path::new("/etc/systemd/system").join(name),
                    NixBlock::UnprovedService,
                )
                .await?;
            if self
                .nix_digest(path, 16 * 1024, NixBlock::UnprovedService)
                .await?
                != digest
            {
                return Err(blocked(NixBlock::UnprovedService));
            }
        }
        let version = self
            .nix_query(
                lease,
                process::WorkerRequest::Version,
                NixBlock::UnprovedClient,
            )
            .await?;
        if version != b"nix (Nix) 2.34.9\n" {
            return Err(blocked(NixBlock::UnprovedClient));
        }
        self.nix_protected(
            Path::new(SOCKET).parent().unwrap().to_owned(),
            NixBlock::DaemonUnavailable,
        )
        .await?;
        if !self
            .inspect_path(PathBuf::from(SOCKET))
            .await
            .map_err(|error| error.preserve_stop(blocked(NixBlock::DaemonUnavailable)))?
            .is_some_and(|m| m.uid() == 0 && m.file_type().is_socket())
        {
            return Err(blocked(NixBlock::DaemonUnavailable));
        }
        let bytes = self
            .nix_query(
                lease,
                process::WorkerRequest::StoreInfo,
                NixBlock::DaemonUnavailable,
            )
            .await?;
        let daemon = daemon_observation(&bytes).map_err(blocked)?;
        let service = service_observation(
            &self
                .nix_query(
                    lease,
                    process::WorkerRequest::SystemdService,
                    NixBlock::InactiveService,
                )
                .await?,
            identity,
        )
        .map_err(blocked)?;
        socket_observation(
            &self
                .nix_query(
                    lease,
                    process::WorkerRequest::SystemdSocket,
                    NixBlock::InactiveService,
                )
                .await?,
            identity,
        )
        .map_err(blocked)?;
        let repeated = service_observation(
            &self
                .nix_query(
                    lease,
                    process::WorkerRequest::SystemdService,
                    NixBlock::InactiveService,
                )
                .await?,
            identity,
        )
        .map_err(blocked)?;
        if service != repeated || self.nix_client().await? != client {
            return Err(blocked(NixBlock::DaemonMismatch));
        }
        Ok(ObservedNixBase {
            client_digest,
            daemon,
            service,
            configuration,
        })
    }

    async fn nix_client(&self) -> Result<PathBuf, TransactionError> {
        match self
            .effects
            .filesystem(super::FilesystemJob::NixClient)
            .await?
        {
            super::FilesystemResult::Executable(path) => Ok(path),
            _ => Err(TransactionError::InvalidState("Nix client completion type")),
        }
    }

    async fn nix_protected(
        &self,
        path: PathBuf,
        failure: NixBlock,
    ) -> Result<PathBuf, TransactionError> {
        match self
            .effects
            .filesystem(super::FilesystemJob::ProtectedPath(path))
            .await
            .map_err(|error| error.preserve_stop(TransactionError::NixBlocked(failure)))?
        {
            super::FilesystemResult::Executable(path) => Ok(path),
            _ => Err(TransactionError::InvalidState("Nix path completion type")),
        }
    }

    async fn nix_digest(
        &self,
        path: PathBuf,
        maximum: u64,
        failure: NixBlock,
    ) -> Result<String, TransactionError> {
        match self
            .effects
            .filesystem(super::FilesystemJob::BoundedDigest { path, maximum })
            .await
            .map_err(|error| error.preserve_stop(TransactionError::NixBlocked(failure)))?
        {
            super::FilesystemResult::Digest(digest) => Ok(digest),
            _ => Err(TransactionError::InvalidState("Nix digest completion type")),
        }
    }

    async fn nix_query(
        &self,
        lease: &super::Effects,
        operation: process::WorkerRequest,
        failure: NixBlock,
    ) -> Result<Vec<u8>, TransactionError> {
        let store_info = matches!(operation, process::WorkerRequest::StoreInfo);
        let result = self
            .observe(lease, operation)
            .await
            .map_err(|error| error.preserve_stop(TransactionError::NixBlocked(failure)))?;
        let restricted_store_warning = b"warning: ignoring the client-specified setting 'store', because it is a restricted setting and you are not a trusted user\n";
        if result.exit_code != Some(0)
            || (!result.stderr.is_empty()
                && !(store_info && result.stderr == restricted_store_warning))
        {
            return Err(TransactionError::NixBlocked(failure));
        }
        Ok(result.stdout)
    }
}

pub(super) fn trusted_client() -> Result<PathBuf, NixBlock> {
    let identity = target_identity()?;
    let client = protected_path(Path::new(CLIENT)).map_err(|_| NixBlock::UnprovedClient)?;
    if client != Path::new(identity.package_root).join("bin/nix") {
        return Err(NixBlock::UnprovedClient);
    }
    Ok(client)
}

pub(super) fn protected_path(path: &Path) -> io::Result<PathBuf> {
    for ancestor in path.ancestors() {
        let m = fs::symlink_metadata(ancestor)?;
        if m.uid() != 0
            || (!m.file_type().is_symlink()
                && m.mode() & 0o022 != 0
                && !(m.is_dir() && m.mode() & 0o1000 != 0))
        {
            return Err(io::Error::other("unproved root ownership"));
        }
    }
    let canonical = path.canonicalize()?;
    for ancestor in canonical.ancestors() {
        let m = fs::symlink_metadata(ancestor)?;
        if m.uid() != 0 || (m.mode() & 0o022 != 0 && !(m.is_dir() && m.mode() & 0o1000 != 0)) {
            return Err(io::Error::other("unproved root ownership"));
        }
    }
    Ok(canonical)
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StoreInfo {
    url: String,
    version: String,
    trusted: bool,
}

fn daemon_observation(bytes: &[u8]) -> Result<DaemonObservation, NixBlock> {
    let info: StoreInfo = serde_json::from_slice(bytes).map_err(|_| NixBlock::DaemonMismatch)?;
    if info.url != "daemon" || info.version != "2.34.9" {
        return Err(NixBlock::DaemonMismatch);
    }
    Ok(DaemonObservation {
        trusted: info.trusted,
    })
}

fn properties(bytes: &[u8]) -> Result<BTreeMap<&str, &str>, NixBlock> {
    let text = std::str::from_utf8(bytes).map_err(|_| NixBlock::UnprovedService)?;
    let mut values = BTreeMap::new();
    for line in text.lines() {
        let (key, value) = line.split_once('=').ok_or(NixBlock::UnprovedService)?;
        if values.insert(key, value).is_some() {
            return Err(NixBlock::UnprovedService);
        }
    }
    Ok(values)
}

fn active_unit<'a>(
    bytes: &'a [u8],
    unit: &str,
    substate: &str,
    identity: &NixTargetIdentity,
) -> Result<BTreeMap<&'a str, &'a str>, NixBlock> {
    let values = properties(bytes)?;
    for (key, expected) in [
        ("Id", unit),
        ("LoadState", "loaded"),
        ("ActiveState", "active"),
        ("SubState", substate),
    ] {
        if values.get(key) != Some(&expected) {
            return Err(NixBlock::InactiveService);
        }
    }
    if values.get("NeedDaemonReload") != Some(&"no") || values.get("DropInPaths") != Some(&"") {
        return Err(NixBlock::UnprovedService);
    }
    let fragment = format!("/etc/systemd/system/{unit}");
    let packaged = format!("{}/lib/systemd/system/{unit}", identity.package_root);
    if values.get("FragmentPath") != Some(&fragment.as_str())
        && values.get("FragmentPath") != Some(&packaged.as_str())
    {
        return Err(NixBlock::UnprovedService);
    }
    Ok(values)
}

fn service_observation(
    bytes: &[u8],
    identity: &NixTargetIdentity,
) -> Result<ServiceObservation, NixBlock> {
    let values = active_unit(bytes, "nix-daemon.service", "running", identity)?;
    let main_pid = values
        .get("MainPID")
        .and_then(|v| v.parse::<u32>().ok())
        .filter(|v| *v > 1)
        .ok_or(NixBlock::InactiveService)?;
    let started = values
        .get("ExecMainStartTimestampMonotonic")
        .and_then(|v| v.parse::<u64>().ok())
        .filter(|v| *v > 0)
        .ok_or(NixBlock::UnprovedService)?;
    let prefix = format!(
        "{{ path={}/bin/nix-daemon ; argv[]=nix-daemon --daemon ; ignore_errors=no ; ",
        identity.package_root
    );
    if !values
        .get("ExecStart")
        .is_some_and(|v| v.starts_with(&prefix))
        || values.get("User") != Some(&"")
        || values.get("RootDirectory") != Some(&"")
    {
        return Err(NixBlock::UnprovedService);
    }
    Ok(ServiceObservation { main_pid, started })
}

fn socket_observation(bytes: &[u8], identity: &NixTargetIdentity) -> Result<(), NixBlock> {
    let values = active_unit(bytes, "nix-daemon.socket", "running", identity)?;
    if values.get("Listen") != Some(&"/nix/var/nix/daemon-socket/socket (Stream)")
        || values.get("Triggers") != Some(&"nix-daemon.service")
    {
        return Err(NixBlock::UnprovedService);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::Layout;
    use super::super::RootLease;
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    #[test]
    fn daemon_response_requires_exact_standard_store_and_version() {
        assert_eq!(
            daemon_observation(br#"{"url":"daemon","version":"2.34.9","trusted":false}"#),
            Ok(DaemonObservation { trusted: false })
        );
        for bytes in [
            br#"{"url":"local","version":"2.34.9","trusted":false}"#.as_slice(),
            br#"{"url":"unix:///nix/var/nix/daemon-socket/socket","version":"2.34.9","trusted":false}"#,
            br#"{"url":"daemon","version":"2.35.0","trusted":false}"#,
            br#"{"url":"daemon","trusted":false}"#,
            br#"{"url":"daemon","version":"2.34.9","trusted":"false"}"#,
            br#"{"url":"daemon","version":"2.34.9","trusted":false,"extra":1}"#,
            br#"{"url":"daemon"}"#,
        ] {
            assert_eq!(daemon_observation(bytes), Err(NixBlock::DaemonMismatch));
        }
    }

    fn service() -> String {
        format!(
            "Id=nix-daemon.service\nLoadState=loaded\nActiveState=active\nSubState=running\nNeedDaemonReload=no\nFragmentPath=/etc/systemd/system/nix-daemon.service\nDropInPaths=\nMainPID=91\nExecMainStartTimestampMonotonic=1234\nExecStart={{ path={}/bin/nix-daemon ; argv[]=nix-daemon --daemon ; ignore_errors=no ; start_time=[Tue] ; stop_time=[n/a] ; pid=91 ; code=(null) ; status=0/0 }}\nUser=\nRootDirectory=\n",
            ARM64_IDENTITY.package_root
        )
    }

    #[test]
    fn loaded_service_must_be_active_exact_and_without_overrides() {
        assert_eq!(
            service_observation(service().as_bytes(), &ARM64_IDENTITY),
            Ok(ServiceObservation {
                main_pid: 91,
                started: 1234
            })
        );
        for (before, after) in [
            ("ActiveState=active", "ActiveState=inactive"),
            ("MainPID=91", "MainPID=0"),
            ("NeedDaemonReload=no", "NeedDaemonReload=yes"),
            ("DropInPaths=", "DropInPaths=/private/override.conf"),
            ("argv[]=nix-daemon --daemon", "argv[]=imposter --daemon"),
            ("User=", "User=jess"),
        ] {
            assert!(
                service_observation(service().replace(before, after).as_bytes(), &ARM64_IDENTITY)
                    .is_err()
            );
        }
        assert!(
            service_observation(
                format!("{}MainPID=92\n", service()).as_bytes(),
                &ARM64_IDENTITY
            )
            .is_err()
        );
    }

    const CAPTURED_X64_SERVICE: &[u8] = br#"MainPID=2656
ExecMainStartTimestampMonotonic=131824418
ExecStart={ path=/nix/store/ssswpqihnyi8231b9r515k2hmpnk0h1n-nix-2.34.9/bin/nix-daemon ; argv[]=nix-daemon --daemon ; ignore_errors=no ; start_time=[Wed 2026-11-18 08:58:58 UTC] ; stop_time=[n/a] ; pid=2656 ; code=(null) ; status=0/0 }
RootDirectory=
User=
Id=nix-daemon.service
LoadState=loaded
ActiveState=active
SubState=running
FragmentPath=/etc/systemd/system/nix-daemon.service
DropInPaths=
NeedDaemonReload=no
"#;

    const CAPTURED_X64_SOCKET: &[u8] = br#"Listen=/nix/var/nix/daemon-socket/socket (Stream)
Id=nix-daemon.socket
Triggers=nix-daemon.service
LoadState=loaded
ActiveState=active
SubState=running
FragmentPath=/etc/systemd/system/nix-daemon.socket
DropInPaths=
NeedDaemonReload=no
"#;

    #[test]
    fn captured_x64_service_observation_uses_selected_package() {
        assert_eq!(
            service_observation(CAPTURED_X64_SERVICE, &X86_64_GNU_IDENTITY),
            Ok(ServiceObservation {
                main_pid: 2656,
                started: 131824418,
            }),
            "captured x64 systemd service uses its selected Nix package"
        );
    }

    #[test]
    fn captured_x64_socket_observation_matches_the_running_unit() {
        assert_eq!(
            socket_observation(CAPTURED_X64_SOCKET, &X86_64_GNU_IDENTITY),
            Ok(())
        );
    }

    #[test]
    fn captured_x64_service_is_rejected_with_the_arm_package() {
        assert_eq!(
            service_observation(CAPTURED_X64_SERVICE, &ARM64_IDENTITY),
            Err(NixBlock::UnprovedService)
        );
    }

    #[test]
    fn a_configuration_digest_never_grants_authority() {
        for digest in [None, Some("a".repeat(64))] {
            let base = ObservedNixBase {
                client_digest: ARM64_IDENTITY.client_sha256.into(),
                daemon: DaemonObservation { trusted: false },
                service: ServiceObservation {
                    main_pid: 91,
                    started: 1234,
                },
                configuration: ConfigurationObservation { digest },
            };
            assert_eq!(
                base.require_configuration_authority(),
                Err(TransactionError::NixBlocked(
                    NixBlock::ConfigurationOwnerUnknown
                ))
            );
        }
    }

    #[test]
    fn file_observation_refuses_oversize_and_nonregular_inputs() {
        let temporary = tempfile::tempdir().unwrap();
        let file = temporary.path().join("observation");
        fs::write(&file, b"abcdef").unwrap();
        assert!(super::super::filesystem::test_digest(&file, 5).is_err());
        assert!(super::super::filesystem::test_digest(temporary.path(), 32).is_err());
        assert_eq!(
            super::super::filesystem::test_digest(&file, 6).unwrap(),
            "bef57ec7f53a6d40beb640a780a639c83bc29ac8a9816f1fc6c5c6dcd93c4721"
        );
    }

    #[test]
    #[ignore]
    fn owned_container_observer() {
        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().canonicalize().unwrap();
        let original_lease = RootLease::acquire(&directory.join("lock")).unwrap();
        println!(
            "ORIGINAL_ADMISSION mode={:o} result={:?}",
            fs::metadata(&directory).unwrap().mode() & 0o777,
            process::OperationOwner::start(&original_lease).map(|_| ())
        );
        drop(original_lease);
        fs::set_permissions(&directory, fs::Permissions::from_mode(0o700)).unwrap();
        let owner = InstallationOwner::new(Layout::new(directory.clone(), directory.join("bin")));
        let lease = RootLease::acquire(&directory.join("lock")).unwrap();
        println!(
            "VERSION_BOUNDARY={:?}",
            super::super::coordinator::test_with(
                &owner.effects,
                &lease,
                owner.observe(&owner.effects, process::WorkerRequest::Version)
            )
        );
        let observation = match super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.observe_nix_base(&owner.effects),
        ) {
            Ok(base) => {
                serde_json::json!({"status":"compatible_base", "evidence":base, "configuration_authority":"unknown", "acquisition_blocked":base.require_configuration_authority().is_err()})
            }
            Err(error) => serde_json::json!({"status":"blocked", "reason":format!("{error:?}")}),
        };
        println!("OBSERVATION={observation}");
    }
}
