use super::{CLIENT, NixBlock, protected_path};
use sha2::{Digest, Sha256};
use std::ffi::{CStr, c_char, c_int, c_void};
use std::fs::{self, Metadata, OpenOptions};
use std::io::{self, Read, Write};
use std::mem::{MaybeUninit, size_of, size_of_val};
use std::os::fd::AsRawFd;
use std::os::unix::fs::{FileTypeExt, MetadataExt, OpenOptionsExt};
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::ptr::{NonNull, null};
use std::time::{Duration, Instant};

mod administrator;
mod configuration;
pub(in crate::installation) mod discovery_effect;
mod publication;
mod restart;
mod startup;

use self::configuration::{DiskObservation, DiskPolicy, SourceRoute};
use self::startup::StartupContext;
pub(crate) use administrator::dispatch as dispatch_administrator;
pub(in crate::installation) use publication::PublicationOutcome;

pub(crate) const OBSERVE_ARGUMENT: &str = "--internal-observe-darwin-nix";
pub(in crate::installation) const OBSERVATION_BYTES: usize = 8;
pub(in crate::installation) const INSPECTION_BYTES: usize = OBSERVATION_BYTES + 1 + 32;

const CLIENT_PATH: &str =
    "/nix/store/6rn1wxxxikr07crcaqinpf7yqgm9pyqx-determinate-nix-3.23.1/bin/nix";
const CLIENT_SIZE: u64 = 4_989_216;
const CLIENT_SHA256: &str = "540097f96e9037cfe2b23ab8e92dddcca14486dadc0afb45cbe9dc12e23d2866";
const NIX_REQUIREMENT: &CStr = c"cdhash H\"8fcb8f55e0b8e2b03ec47131e617fe0792a62f8d\"";
const SUPERVISOR_PATH: &str = "/usr/local/bin/determinate-nixd";
const SUPERVISOR_REQUIREMENT: &CStr = c"anchor apple generic and identifier \"systems.determinate.determinate-nixd\" and certificate leaf[subject.OU] = \"X3JQ4VPJZ6\" and cdhash H\"1092289bf51a5f1bf9d040f3b238402078ef8747\"";

const JOB: &str = "system/systems.determinate.nix-daemon";
const PLIST: &str = "/Library/LaunchDaemons/systems.determinate.nix-daemon.plist";
const SERVICE: &str = "/var/run/determinate-nixd.socket";
const LISTENER: &str = "/var/run/nix-daemon.socket";
const LAUNCHCTL_LIMIT: u64 = 64 * 1024;
const LAUNCHCTL_TIMEOUT: Duration = Duration::from_secs(2);
const SOCKET_TIMEOUT: Duration = Duration::from_secs(2);

const PROTOCOL: u64 = 0x123;
const CLIENT_MAGIC: u64 = 0x6e69_7863;
const SERVER_MAGIC: u64 = 0x6478_696f;
const STDERR_LAST: u64 = 0x616c_7473;
const VERSION: &[u8; 8] = b"2.35.2\0\0";

#[derive(Debug, PartialEq, Eq)]
pub(in crate::installation) struct ObservedDarwinNix {
    disk: DiskObservation,
    startup: StartupContext,
}

impl ObservedDarwinNix {
    pub(in crate::installation) fn supports_inspection(&self) -> bool {
        self.disk.source != SourceRoute::Unknown
            && self.disk.policy != DiskPolicy::Unknown
            && self.startup == StartupContext::RequiresPrivilege
    }

    fn confirms(&self, ordinary: &Self) -> bool {
        ordinary.supports_inspection()
            && self.disk == ordinary.disk
            && self.startup == StartupContext::StandardSources
    }

    fn permits_vendor_plan(&self) -> bool {
        self.startup == StartupContext::StandardSources
            && self.disk.source == SourceRoute::VendorCustom
            && self.disk.policy == DiskPolicy::RequiredCacheMissing
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct VendorPlanCommitment([u8; 32]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::installation) struct VendorPublicationRequest {
    attempt: [u8; 32],
    reviewed: VendorPlanCommitment,
}

impl VendorPublicationRequest {
    fn decode(bytes: &[u8; 64]) -> Option<Self> {
        let (attempt, reviewed) = bytes.split_at(32);
        let attempt: [u8; 32] = attempt.try_into().expect("fixed frame");
        let reviewed: [u8; 32] = reviewed.try_into().expect("fixed frame");
        (attempt != [0; 32] && reviewed != [0; 32]).then_some(Self {
            attempt,
            reviewed: VendorPlanCommitment(reviewed),
        })
    }

    fn encode(self) -> [u8; 64] {
        let mut bytes = [0; 64];
        bytes[..32].copy_from_slice(&self.attempt);
        bytes[32..].copy_from_slice(&self.reviewed.0);
        bytes
    }

    pub(in crate::installation) fn from_pending(
        pending: &super::super::PendingNixPolicy,
    ) -> Option<Self> {
        if !pending.operation.is_append()
            || pending.state_lease.is_none()
            || pending.attempt == [0; 32]
            || pending.reviewed == [0; 32]
        {
            return None;
        }
        Some(Self {
            attempt: pending.attempt,
            reviewed: VendorPlanCommitment(pending.reviewed),
        })
    }

    pub(in crate::installation) fn pending(
        self,
        helper_path: String,
        helper: crate::candidate::FileIdentity,
        state_lease: super::super::PolicyStateLease,
    ) -> super::super::PendingNixPolicy {
        super::super::PendingNixPolicy {
            attempt: self.attempt,
            reviewed: self.reviewed.0,
            helper_path,
            helper,
            operation: super::policy_observation::PolicyOperation::ConventionalAppend,
            state_lease: Some(state_lease),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(in crate::installation) struct DarwinRestartPlan([u8; 32]);

#[cfg(test)]
impl DarwinRestartPlan {
    pub(in crate::installation) fn for_test() -> Self {
        Self([9; 32])
    }
}

impl std::fmt::Debug for DarwinRestartPlan {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("DarwinRestartPlan")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::installation) enum DarwinPolicyRecovery {
    Prepared,
    LeafPublished,
    Ready(DarwinRestartPlan),
    Active,
    Unresolved,
}

#[derive(Debug, PartialEq, Eq)]
pub(in crate::installation) struct InspectedDarwinNix {
    observation: ObservedDarwinNix,
    vendor_plan: Option<VendorPlanCommitment>,
}

impl std::fmt::Debug for VendorPlanCommitment {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("VendorPlanCommitment")
    }
}

impl InspectedDarwinNix {
    pub(in crate::installation) fn confirms(&self, ordinary: &ObservedDarwinNix) -> bool {
        self.observation.confirms(ordinary)
    }

    #[cfg(test)]
    fn has_vendor_plan(&self) -> bool {
        self.vendor_plan.is_some()
    }

    pub(in crate::installation) fn publication_request(
        &self,
    ) -> Result<Option<VendorPublicationRequest>, super::TransactionError> {
        use ring::rand::{SecureRandom, SystemRandom};

        self.vendor_plan
            .map(|reviewed| {
                let mut attempt = [0; 32];
                SystemRandom::new()
                    .fill(&mut attempt)
                    .map_err(|_| super::TransactionError::Io)?;
                if attempt == [0; 32] {
                    return Err(super::TransactionError::Io);
                }
                Ok(VendorPublicationRequest { attempt, reviewed })
            })
            .transpose()
    }
}

pub(in crate::installation) fn decode_observation(
    bytes: &[u8],
) -> Result<ObservedDarwinNix, NixBlock> {
    let [b'N', b'D', b'V', 2, status, source, policy, startup] = bytes else {
        return Err(NixBlock::UnprovedService);
    };
    match (status, source, policy, startup) {
        (0, source, policy, startup) => {
            let source = match source {
                0 => SourceRoute::Unknown,
                1 => SourceRoute::VendorCustom,
                2 => SourceRoute::DarwinGenerated,
                _ => return Err(NixBlock::UnprovedService),
            };
            let policy = match policy {
                0 => DiskPolicy::Unknown,
                1 => DiskPolicy::RequiredCacheMissing,
                2 => DiskPolicy::Satisfies,
                _ => return Err(NixBlock::UnprovedService),
            };
            if source == SourceRoute::Unknown && policy != DiskPolicy::Unknown {
                return Err(NixBlock::UnprovedService);
            }
            Ok(ObservedDarwinNix {
                disk: DiskObservation { source, policy },
                startup: StartupContext::decode(*startup).ok_or(NixBlock::UnprovedService)?,
            })
        }
        (1, 0, 0, 0) => Err(NixBlock::MissingClient),
        (2, 0, 0, 0) => Err(NixBlock::UnprovedClient),
        (3, 0, 0, 0) => Err(NixBlock::InactiveService),
        (5, 0, 0, 0) => Err(NixBlock::DaemonUnavailable),
        (6, 0, 0, 0) => Err(NixBlock::DaemonMismatch),
        _ => Err(NixBlock::UnprovedService),
    }
}

pub(in crate::installation) fn decode_inspection(
    bytes: &[u8],
) -> Result<InspectedDarwinNix, NixBlock> {
    let frame: &[u8; INSPECTION_BYTES] = bytes.try_into().map_err(|_| NixBlock::UnprovedService)?;
    let (observation, tail) = frame.split_at(OBSERVATION_BYTES);
    let digest: [u8; 32] = tail[1..].try_into().expect("fixed frame");
    let vendor_plan = match tail[0] {
        0 if digest == [0; 32] => None,
        1 if digest != [0; 32] => Some(VendorPlanCommitment(digest)),
        _ => return Err(NixBlock::UnprovedService),
    };
    let observation = decode_observation(observation)?;
    if vendor_plan.is_some() && !observation.permits_vendor_plan() {
        return Err(NixBlock::UnprovedService);
    }
    Ok(InspectedDarwinNix {
        observation,
        vendor_plan,
    })
}

pub(crate) fn run_observer() -> i32 {
    let Some(frame) = encode_observation(observe(false).map(|value| value.inspection.observation))
    else {
        return 1;
    };
    i32::from(
        io::stdout()
            .write_all(&frame)
            .and_then(|()| io::stdout().flush())
            .is_err(),
    )
}

fn inspect_frame() -> Option<[u8; INSPECTION_BYTES]> {
    let inspection = observe(true);
    let plan = inspection
        .as_ref()
        .ok()
        .and_then(|value| value.inspection.vendor_plan.as_ref());
    let mut frame = [0; INSPECTION_BYTES];
    if let Some(plan) = plan {
        frame[OBSERVATION_BYTES] = 1;
        frame[OBSERVATION_BYTES + 1..].copy_from_slice(&plan.0);
    }
    let observation = encode_observation(inspection.map(|value| value.inspection.observation))?;
    frame[..OBSERVATION_BYTES].copy_from_slice(&observation);
    Some(frame)
}

fn encode_observation(
    observation: Result<ObservedDarwinNix, NixBlock>,
) -> Option<[u8; OBSERVATION_BYTES]> {
    let fields = match observation {
        Ok(value) => [
            0,
            match value.disk.source {
                SourceRoute::Unknown => 0,
                SourceRoute::VendorCustom => 1,
                SourceRoute::DarwinGenerated => 2,
            },
            match value.disk.policy {
                DiskPolicy::Unknown => 0,
                DiskPolicy::RequiredCacheMissing => 1,
                DiskPolicy::Satisfies => 2,
            },
            value.startup as u8,
        ],
        Err(NixBlock::MissingClient) => [1, 0, 0, 0],
        Err(NixBlock::UnprovedClient) => [2, 0, 0, 0],
        Err(NixBlock::InactiveService) => [3, 0, 0, 0],
        Err(NixBlock::UnprovedService) => [4, 0, 0, 0],
        Err(NixBlock::DaemonUnavailable) => [5, 0, 0, 0],
        Err(NixBlock::DaemonMismatch) => [6, 0, 0, 0],
        Err(_) => return None,
    };
    let mut frame = [b'N', b'D', b'V', 2, 0, 0, 0, 0];
    frame[4..].copy_from_slice(&fields);
    Some(frame)
}

#[derive(Eq, PartialEq)]
struct FileIdentity {
    device: u64,
    inode: u64,
    mode: u32,
    owner: u32,
    group: u32,
    length: u64,
    changed: (i64, i64),
    modified: (i64, i64),
}

impl FileIdentity {
    fn bind(&self, hash: &mut Sha256) {
        for value in [self.device, self.inode, self.length] {
            hash.update(value.to_le_bytes());
        }
        for value in [self.mode, self.owner, self.group] {
            hash.update(value.to_le_bytes());
        }
        for value in [
            self.changed.0,
            self.changed.1,
            self.modified.0,
            self.modified.1,
        ] {
            hash.update(value.to_le_bytes());
        }
    }
}

impl From<Metadata> for FileIdentity {
    fn from(metadata: Metadata) -> Self {
        Self {
            device: metadata.dev(),
            inode: metadata.ino(),
            mode: metadata.mode(),
            owner: metadata.uid(),
            group: metadata.gid(),
            length: metadata.len(),
            changed: (metadata.ctime(), metadata.ctime_nsec()),
            modified: (metadata.mtime(), metadata.mtime_nsec()),
        }
    }
}

fn client_identity() -> Result<FileIdentity, NixBlock> {
    let path = protected_path(Path::new(CLIENT)).map_err(|error| match error.kind() {
        io::ErrorKind::NotFound => NixBlock::MissingClient,
        _ => NixBlock::UnprovedClient,
    })?;
    if path != Path::new(CLIENT_PATH) {
        return Err(NixBlock::UnprovedClient);
    }
    let metadata = fs::symlink_metadata(path).map_err(|_| NixBlock::UnprovedClient)?;
    if !metadata.is_file() || metadata.uid() != 0 || metadata.len() != CLIENT_SIZE {
        return Err(NixBlock::UnprovedClient);
    }
    Ok(metadata.into())
}

fn verify_client() -> Result<FileIdentity, NixBlock> {
    let identity = client_identity()?;
    let file = OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
        .open(CLIENT_PATH)
        .map_err(|_| NixBlock::UnprovedClient)?;
    if FileIdentity::from(file.metadata().map_err(|_| NixBlock::UnprovedClient)?) != identity {
        return Err(NixBlock::UnprovedClient);
    }
    let mut measured = Sha256::new();
    let mut reader = (&file).take(CLIENT_SIZE + 1);
    let count = io::copy(&mut reader, &mut measured).map_err(|_| NixBlock::UnprovedClient)?;
    if count != CLIENT_SIZE
        || format!("{:x}", measured.finalize()) != CLIENT_SHA256
        || FileIdentity::from(file.metadata().map_err(|_| NixBlock::UnprovedClient)?) != identity
    {
        return Err(NixBlock::UnprovedClient);
    }
    Ok(identity)
}

pub(in crate::installation) fn policy_helper_client() -> Result<PathBuf, NixBlock> {
    let client = Path::new(CLIENT);
    let expected = Path::new(CLIENT_PATH);
    let protect = |path| {
        policy_helper_protected_path(path).map_err(|error| match error.kind() {
            io::ErrorKind::NotFound => NixBlock::MissingClient,
            _ => NixBlock::UnprovedClient,
        })
    };
    if protect(client)? != expected {
        return Err(NixBlock::UnprovedClient);
    }
    let identity = verify_client()?;
    if identity.mode & 0o7777 != 0o555
        || protect(client)? != expected
        || client_identity()? != identity
    {
        return Err(NixBlock::UnprovedClient);
    }
    Ok(expected.to_owned())
}

pub(in crate::installation) fn policy_helper_protected_path(path: &Path) -> io::Result<PathBuf> {
    configuration::protect(path, true)
}

#[derive(Eq, PartialEq)]
struct SocketIdentities {
    alias: FileIdentity,
    listener: FileIdentity,
    supervisor: FileIdentity,
}

impl SocketIdentities {
    fn observe() -> Result<Self, NixBlock> {
        let alias = fs::symlink_metadata(super::SOCKET).map_err(|_| NixBlock::DaemonUnavailable)?;
        if !alias.is_symlink()
            || alias.uid() != 0
            || fs::read_link(super::SOCKET).as_deref().ok() != Some(Path::new(LISTENER))
            || fs::canonicalize(super::SOCKET).as_deref().ok()
                != Some(Path::new("/private/var/run/nix-daemon.socket"))
        {
            return Err(NixBlock::DaemonMismatch);
        }
        Ok(Self {
            alias: alias.into(),
            listener: root_socket(LISTENER, NixBlock::DaemonUnavailable)?,
            supervisor: root_socket(SERVICE, NixBlock::InactiveService)?,
        })
    }
}

fn root_socket(path: &str, missing: NixBlock) -> Result<FileIdentity, NixBlock> {
    let metadata = fs::symlink_metadata(path).map_err(|_| missing)?;
    if !metadata.file_type().is_socket() || metadata.uid() != 0 {
        return Err(NixBlock::DaemonMismatch);
    }
    Ok(metadata.into())
}

fn launchd_pid() -> Result<u32, NixBlock> {
    let mut child = Command::new("/bin/launchctl")
        .args(["print", JOB])
        .env_clear()
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|_| NixBlock::UnprovedService)?;
    let observed = read_launchctl(&mut child);
    if observed.is_err() {
        let _ = child.kill();
        let _ = child.wait();
    }
    observed
}

fn read_launchctl(child: &mut std::process::Child) -> Result<u32, NixBlock> {
    parse_job(&read_bounded_child(child, LAUNCHCTL_TIMEOUT)?)
}

fn read_bounded_child(
    child: &mut std::process::Child,
    timeout: Duration,
) -> Result<Vec<u8>, NixBlock> {
    let pipe = child.stdout.take().ok_or(NixBlock::UnprovedService)?;
    read_bounded_output(child, pipe, timeout)
}

fn read_bounded_output(
    child: &mut std::process::Child,
    mut pipe: impl Read + AsRawFd,
    timeout: Duration,
) -> Result<Vec<u8>, NixBlock> {
    let deadline = Instant::now() + timeout;
    let fd = pipe.as_raw_fd();
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0 || unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) } < 0 {
        return Err(NixBlock::UnprovedService);
    }
    let mut output = Vec::new();
    let mut buffer = [0; 4096];
    let mut eof = false;
    while !administrator::cancelled() && Instant::now() < deadline {
        if !eof {
            match pipe.read(&mut buffer) {
                Ok(0) => eof = true,
                Ok(count) if output.len() + count <= LAUNCHCTL_LIMIT as usize => {
                    output.extend_from_slice(&buffer[..count]);
                    continue;
                }
                Err(error)
                    if matches!(
                        error.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                _ => return Err(NixBlock::UnprovedService),
            }
        }
        if eof && let Some(status) = child.try_wait().map_err(|_| NixBlock::UnprovedService)? {
            return if status.success() {
                Ok(output)
            } else {
                Err(NixBlock::InactiveService)
            };
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    Err(NixBlock::UnprovedService)
}

fn parse_job(output: &[u8]) -> Result<u32, NixBlock> {
    let output = std::str::from_utf8(output).map_err(|_| NixBlock::UnprovedService)?;
    let mut lines = output.lines();
    let header = format!("{JOB} = {{");
    if lines.next() != Some(header.as_str()) || lines.next_back() != Some("}") {
        return Err(NixBlock::UnprovedService);
    }
    let mut program = None;
    let mut path = None;
    let mut state = None;
    let mut pid = None;
    for (key, value) in lines
        .filter_map(|line| line.strip_prefix('\t'))
        .filter(|line| !line.starts_with([' ', '\t']))
        .filter_map(|line| line.split_once(" = "))
    {
        let field = match key {
            "program" => &mut program,
            "path" => &mut path,
            "state" => &mut state,
            "pid" => &mut pid,
            _ => continue,
        };
        if field.replace(value).is_some() {
            return Err(NixBlock::UnprovedService);
        }
    }
    if program != Some(SUPERVISOR_PATH) || path != Some(PLIST) {
        return Err(NixBlock::UnprovedService);
    }
    if state != Some("running") {
        return Err(NixBlock::InactiveService);
    }
    let pid = pid.ok_or(NixBlock::UnprovedService)?;
    pid.parse::<u32>()
        .ok()
        .filter(|number| *number > 1 && i32::try_from(*number).is_ok() && number.to_string() == pid)
        .ok_or(NixBlock::UnprovedService)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RootPeer([u32; 8]);

impl RootPeer {
    fn connect(path: &str, unavailable: NixBlock) -> Result<(UnixStream, Self), NixBlock> {
        let stream = UnixStream::connect(path).map_err(|_| unavailable)?;
        let peer = Self::read(&stream).ok_or(NixBlock::DaemonMismatch)?;
        Ok((stream, peer))
    }

    fn read(stream: &UnixStream) -> Option<Self> {
        let mut audit = [0_u32; 8];
        let mut len = size_of_val(&audit) as libc::socklen_t;
        let mut uid = libc::uid_t::MAX;
        let mut gid = libc::gid_t::MAX;
        let fd = stream.as_raw_fd();
        let credentials = unsafe { libc::getpeereid(fd, &mut uid, &mut gid) };
        let token = unsafe {
            libc::getsockopt(
                fd,
                libc::SOL_LOCAL,
                libc::LOCAL_PEERTOKEN,
                audit.as_mut_ptr().cast(),
                &mut len,
            )
        };
        let peer = Self(audit);
        (credentials == 0
            && token == 0
            && len as usize == size_of_val(&audit)
            && uid == 0
            && audit[1] == 0
            && peer.pid() > 1
            && i32::try_from(peer.pid()).is_ok()
            && audit[7] != 0)
            .then_some(peer)
    }

    fn pid(self) -> u32 {
        self.0[5]
    }

    fn parent(self) -> Option<u32> {
        let mut info = MaybeUninit::<libc::proc_bsdshortinfo>::uninit();
        let length = size_of::<libc::proc_bsdshortinfo>() as c_int;
        // libproc initializes the full record when the returned length matches.
        let found = unsafe {
            libc::proc_pidinfo(
                self.pid() as c_int,
                libc::PROC_PIDT_SHORTBSDINFO,
                0,
                info.as_mut_ptr().cast(),
                length,
            )
        };
        if found != length {
            return None;
        }
        let info = unsafe { info.assume_init() };
        (info.pbsi_pid == self.pid() && info.pbsi_uid == 0).then_some(info.pbsi_ppid)
    }

    fn verified(self, requirement: &CodeRequirement) -> bool {
        requirement.matches(&self.0)
    }
}

struct VerifiedObservation {
    inspection: InspectedDarwinNix,
    application: Option<configuration::VendorApplication>,
    daemon: DaemonIncarnation,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct DaemonIncarnation {
    supervisor: RootPeer,
    fingerprint: [u8; 32],
}

fn observe(prepare_application: bool) -> Result<VerifiedObservation, NixBlock> {
    let client = verify_client()?;
    let sockets = SocketIdentities::observe()?;
    let job = launchd_pid()?;
    let vendor = CodeRequirement::new(SUPERVISOR_REQUIREMENT).ok_or(NixBlock::UnprovedService)?;
    let nix = CodeRequirement::new(NIX_REQUIREMENT).ok_or(NixBlock::UnprovedClient)?;
    let (control, supervisor) = RootPeer::connect(SERVICE, NixBlock::InactiveService)?;
    if supervisor.pid() != job || !supervisor.verified(&vendor) {
        return Err(NixBlock::UnprovedService);
    }
    let (mut connection, listener) = RootPeer::connect(super::SOCKET, NixBlock::DaemonUnavailable)?;
    if RootPeer::connect(LISTENER, NixBlock::DaemonUnavailable)?.1 != listener
        || !listener.verified(&nix)
        || listener.parent() != Some(supervisor.pid())
        || RootPeer::read(&connection) != Some(listener)
    {
        return Err(NixBlock::DaemonMismatch);
    }
    let startup = StartupContext::observe(listener);
    if RootPeer::read(&connection) != Some(listener) || !listener.verified(&nix) {
        return Err(NixBlock::DaemonMismatch);
    }

    connection
        .set_read_timeout(Some(SOCKET_TIMEOUT))
        .and_then(|()| connection.set_write_timeout(Some(SOCKET_TIMEOUT)))
        .and_then(|()| handshake(&mut connection))
        .map_err(|error| match error.kind() {
            io::ErrorKind::InvalidData => NixBlock::DaemonMismatch,
            _ => NixBlock::DaemonUnavailable,
        })?;

    // A connection worker can take over the socket and change its audit token.
    let replying = RootPeer::read(&connection).ok_or(NixBlock::DaemonMismatch)?;
    let (disk, application) =
        configuration::observe(prepare_application && startup == StartupContext::StandardSources);
    let observation = ObservedDarwinNix { disk, startup };
    let application = application.filter(|_| {
        observation.permits_vendor_plan() && StartupContext::observe(listener) == startup
    });
    if !replying.verified(&nix)
        || replying != listener && replying.parent() != Some(listener.pid())
        || !listener.verified(&nix)
        || RootPeer::connect(LISTENER, NixBlock::DaemonUnavailable)?.1 != listener
        || RootPeer::connect(super::SOCKET, NixBlock::DaemonUnavailable)?.1 != listener
        || RootPeer::read(&connection) != Some(replying)
        || SocketIdentities::observe()? != sockets
    {
        return Err(NixBlock::DaemonMismatch);
    }
    if RootPeer::read(&control) != Some(supervisor)
        || RootPeer::connect(SERVICE, NixBlock::InactiveService)?.1 != supervisor
        || !supervisor.verified(&vendor)
        || launchd_pid()? != job
    {
        return Err(NixBlock::UnprovedService);
    }
    if client_identity()? != client {
        return Err(NixBlock::UnprovedClient);
    }
    let daemon = DaemonIncarnation {
        supervisor,
        fingerprint: daemon_incarnation(&client, &sockets, supervisor, listener),
    };
    let vendor_plan = application.as_ref().map(|inputs| {
        let mut hash = Sha256::new();
        hash.update(b"neo:determinate-darwin:application:v2\0");
        hash.update(inputs.commitment());
        hash.update(daemon.fingerprint);
        VendorPlanCommitment(hash.finalize().into())
    });
    let inspection = InspectedDarwinNix {
        observation,
        vendor_plan,
    };
    Ok(VerifiedObservation {
        inspection,
        application,
        daemon,
    })
}

fn publish_vendor_policy(
    request: &VendorPublicationRequest,
    authorize: impl FnOnce() -> bool,
) -> publication::PublicationOutcome {
    use publication::{LockedVendorDirectory, PublicationOutcome};

    let Ok(directory) = LockedVendorDirectory::lock() else {
        return PublicationOutcome::Refused;
    };
    let Ok(VerifiedObservation {
        inspection,
        application: Some(application),
        daemon,
    }) = observe(true)
    else {
        return PublicationOutcome::Refused;
    };
    if inspection.vendor_plan != Some(request.reviewed) {
        return PublicationOutcome::Refused;
    }
    directory.publish(&application, request, &daemon.fingerprint, authorize)
}

fn daemon_incarnation(
    client: &FileIdentity,
    sockets: &SocketIdentities,
    supervisor: RootPeer,
    listener: RootPeer,
) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(b"neo:determinate-darwin:daemon:v1\0");
    hash.update(CLIENT_SHA256);
    for identity in [
        client,
        &sockets.alias,
        &sockets.listener,
        &sockets.supervisor,
    ] {
        identity.bind(&mut hash);
    }
    for process in [supervisor, listener] {
        for field in process.0 {
            hash.update(field.to_le_bytes());
        }
    }
    hash.finalize().into()
}

fn handshake(stream: &mut (impl Read + Write)) -> io::Result<()> {
    let invalid = || io::Error::new(io::ErrorKind::InvalidData, "Nix daemon handshake");
    stream.write_all(&CLIENT_MAGIC.to_le_bytes())?;
    stream.write_all(&PROTOCOL.to_le_bytes())?;
    if read_word(stream)? != SERVER_MAGIC || !(PROTOCOL..=0x1ff).contains(&read_word(stream)?) {
        return Err(invalid());
    }
    stream.write_all(&[0; 16])?;
    if read_word(stream)? != 6 {
        return Err(invalid());
    }
    let mut version = [0; VERSION.len()];
    stream.read_exact(&mut version)?;
    if &version != VERSION
        || !matches!(read_word(stream)?, 1 | 2)
        || read_word(stream)? != STDERR_LAST
    {
        return Err(invalid());
    }
    Ok(())
}

fn read_word(stream: &mut impl Read) -> io::Result<u64> {
    let mut word = [0; size_of::<u64>()];
    stream.read_exact(&mut word)?;
    Ok(u64::from_le_bytes(word))
}

type CfRef = *const c_void;

#[link(name = "CoreFoundation", kind = "framework")]
unsafe extern "C" {
    static kCFTypeDictionaryKeyCallBacks: c_void;
    static kCFTypeDictionaryValueCallBacks: c_void;
    fn CFStringCreateWithCString(allocator: CfRef, bytes: *const c_char, encoding: u32) -> CfRef;
    fn CFDataCreate(allocator: CfRef, bytes: *const u8, len: isize) -> CfRef;
    fn CFDictionaryCreate(
        allocator: CfRef,
        keys: *const CfRef,
        values: *const CfRef,
        count: isize,
        key_callbacks: CfRef,
        value_callbacks: CfRef,
    ) -> CfRef;
    fn CFRelease(value: CfRef);
}

#[link(name = "Security", kind = "framework")]
unsafe extern "C" {
    static kSecGuestAttributeAudit: CfRef;
    fn SecRequirementCreateWithString(requirement: CfRef, flags: u32, result: *mut CfRef) -> i32;
    fn SecCodeCopyGuestWithAttributes(
        host: CfRef,
        attributes: CfRef,
        flags: u32,
        guest: *mut CfRef,
    ) -> i32;
    fn SecCodeCheckValidity(guest: CfRef, flags: u32, requirement: CfRef) -> i32;
}

struct OwnedCf(NonNull<c_void>);

impl OwnedCf {
    fn created(result: CfRef) -> Option<Self> {
        NonNull::new(result.cast_mut()).map(Self)
    }

    fn as_ptr(&self) -> CfRef {
        self.0.as_ptr()
    }
}

impl Drop for OwnedCf {
    fn drop(&mut self) {
        // The create and copy calls each transfer one Core Foundation reference.
        unsafe { CFRelease(self.as_ptr()) };
    }
}

struct CodeRequirement(OwnedCf);

impl CodeRequirement {
    fn new(requirement: &CStr) -> Option<Self> {
        const UTF8: u32 = 0x0800_0100;
        let text = OwnedCf::created(unsafe {
            CFStringCreateWithCString(null(), requirement.as_ptr(), UTF8)
        })?;
        let mut result = null();
        let status = unsafe { SecRequirementCreateWithString(text.as_ptr(), 0, &mut result) };
        let requirement = OwnedCf::created(result)?;
        (status == 0).then_some(Self(requirement))
    }

    fn matches(&self, audit: &[u32; 8]) -> bool {
        // Security.framework consumes the native bytes of the full kernel audit token.
        let data = OwnedCf::created(unsafe {
            CFDataCreate(null(), audit.as_ptr().cast(), size_of_val(audit) as isize)
        });
        let Some(data) = data else {
            return false;
        };
        let attributes = OwnedCf::created(unsafe {
            CFDictionaryCreate(
                null(),
                &kSecGuestAttributeAudit,
                &data.as_ptr(),
                1,
                &raw const kCFTypeDictionaryKeyCallBacks,
                &raw const kCFTypeDictionaryValueCallBacks,
            )
        });
        let Some(attributes) = attributes else {
            return false;
        };
        let mut guest = null();
        let status =
            unsafe { SecCodeCopyGuestWithAttributes(null(), attributes.as_ptr(), 0, &mut guest) };
        let Some(guest) = OwnedCf::created(guest) else {
            return false;
        };
        status == 0 && unsafe { SecCodeCheckValidity(guest.as_ptr(), 0, self.0.as_ptr()) } == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn observation_frame_requires_exact_known_success() {
        assert_eq!(
            decode_observation(b"NDV\x02\0\x01\x02\0"),
            Ok(ObservedDarwinNix {
                disk: DiskObservation {
                    source: SourceRoute::VendorCustom,
                    policy: DiskPolicy::Satisfies,
                },
                startup: StartupContext::RequiresPrivilege,
            })
        );
        assert_eq!(
            decode_observation(b"NDV\x02\x05\0\0\0"),
            Err(NixBlock::DaemonUnavailable)
        );
        for frame in [
            b"NDV\x01\0".as_slice(),
            b"NDV\x02\0\0\0",
            b"NDV\x02\0\0\0\0\0",
            b"NDV\x02\0\0\x02\0",
            b"NDV\x02\x05\x01\0\0",
            b"NDV\x02\0\x01\x02\xff",
        ] {
            assert!(matches!(
                decode_observation(frame),
                Err(NixBlock::UnprovedService)
            ));
        }
    }

    #[test]
    fn inspection_plan_requires_confirmed_standard_vendor_sources() {
        let mut frame = [0; INSPECTION_BYTES];
        frame[..OBSERVATION_BYTES].copy_from_slice(b"NDV\x02\0\x01\x01\x01");
        let ordinary = decode_observation(b"NDV\x02\0\x01\x01\0").expect("ordinary observation");

        let inspection = decode_inspection(&frame).expect("inspection without plan");
        assert!(inspection.confirms(&ordinary));
        assert!(!inspection.has_vendor_plan());

        frame[OBSERVATION_BYTES] = 1;
        assert_eq!(decode_inspection(&frame), Err(NixBlock::UnprovedService));
        frame[OBSERVATION_BYTES + 1..].fill(19);
        assert!(
            decode_inspection(&frame)
                .expect("vendor plan")
                .has_vendor_plan()
        );
        assert_eq!(decode_observation(&frame), Err(NixBlock::UnprovedService));
        for (index, changed) in [
            (5, 2),
            (6, 2),
            (7, 0),
            (OBSERVATION_BYTES, 0),
            (OBSERVATION_BYTES, 2),
        ] {
            let mut invalid = frame;
            invalid[index] = changed;
            assert_eq!(decode_inspection(&invalid), Err(NixBlock::UnprovedService));
        }
        assert_eq!(
            decode_inspection(&frame[..INSPECTION_BYTES - 1]),
            Err(NixBlock::UnprovedService)
        );
    }

    #[test]
    fn launchd_job_requires_unique_top_level_identity() {
        let job = format!(
            "{JOB} = {{\n\tpath = {PLIST}\n\tprogram = {SUPERVISOR_PATH}\n\tstate = running\n\tpid = 25\n\tproperties = {{\n\t\tpid = 99\n\t}}\n}}\n"
        );
        assert!(matches!(parse_job(job.as_bytes()), Ok(25)));
        for invalid in [
            job.replace("\tpid = 25\n", ""),
            job.replace("\tpid = 25\n", "\tpid = 25\n\tpid = 25\n"),
            job.replace("\tpid = 25\n", "\tpid = +25\n"),
            job.replace(SUPERVISOR_PATH, "/tmp/determinate-nixd"),
        ] {
            assert!(matches!(
                parse_job(invalid.as_bytes()),
                Err(NixBlock::UnprovedService)
            ));
        }
        let inactive = job.replace("state = running", "state = not running");
        assert!(matches!(
            parse_job(inactive.as_bytes()),
            Err(NixBlock::InactiveService)
        ));
    }

    struct Exchange {
        reply: Cursor<Vec<u8>>,
        sent: Vec<u8>,
    }

    impl Exchange {
        fn new(server_protocol: u64, length: u64, version: [u8; 8], trust: u64, last: u64) -> Self {
            let mut reply = Vec::new();
            for word in [SERVER_MAGIC, server_protocol, length] {
                reply.extend(word.to_le_bytes());
            }
            reply.extend(version);
            for word in [trust, last] {
                reply.extend(word.to_le_bytes());
            }
            Self {
                reply: Cursor::new(reply),
                sent: Vec::new(),
            }
        }
    }

    impl Read for Exchange {
        fn read(&mut self, intended: &mut [u8]) -> io::Result<usize> {
            self.reply.read(intended)
        }
    }

    impl Write for Exchange {
        fn write(&mut self, sent: &[u8]) -> io::Result<usize> {
            self.sent.write(sent)
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn handshake_stops_before_sending_store_operations() {
        for trust in [1, 2] {
            let mut exchange = Exchange::new(0x126, 6, *VERSION, trust, STDERR_LAST);
            assert!(handshake(&mut exchange).is_ok());
            assert_eq!(
                exchange.sent,
                [CLIENT_MAGIC, PROTOCOL, 0, 0]
                    .map(u64::to_le_bytes)
                    .concat()
            );
        }
        for mut invalid in [
            Exchange::new(0x122, 6, *VERSION, 1, STDERR_LAST),
            Exchange::new(0x200, 6, *VERSION, 1, STDERR_LAST),
            Exchange::new(0x126, u64::MAX, *VERSION, 1, STDERR_LAST),
            Exchange::new(0x126, 6, *b"2.35.2\0x", 1, STDERR_LAST),
            Exchange::new(0x126, 6, *VERSION, 0, STDERR_LAST),
            Exchange::new(0x126, 6, *VERSION, 1, 0),
        ] {
            assert_eq!(
                handshake(&mut invalid).unwrap_err().kind(),
                io::ErrorKind::InvalidData
            );
        }
    }
}
