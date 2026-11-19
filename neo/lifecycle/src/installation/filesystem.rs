use super::coordinator::{Admission, Cancellation, SettlementKind, WaitInterest};
use super::dashboard::terminal::ParkedTerminal;
#[cfg(target_os = "linux")]
use super::nix_owner::peer_handoff::{
    ExactExecutable, HandoffError, Rendezvous,
    launcher::{VerifiedCaller, VerifiedExistingPolicy},
};
use super::resource_io::ResourceDirectory;
use super::resources::{Inventory, LinkObservation};
use super::review::{CommandOnPath, inspect_commands_on_path};
use super::{
    Layout, ManagedCommand, RootLease, TransactionError, open_owned_file, path_kind,
    read_owned_link, remove_owned_file_if_present, sync_directory,
};
use crate::candidate::FileIdentity;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::os::fd::AsRawFd;
use std::os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt, symlink};
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

pub(super) enum FilesystemJob {
    Startup(Option<Layout>),
    UninstallInspect {
        layout: Layout,
        record: super::InstallationRecord,
    },
    UninstallBind {
        layout: super::Layout,
        intent: super::uninstall::UninstallIntent,
    },
    UninstallRemove(super::uninstall::Removal),
    UninstallConfirm(super::uninstall::UninstallIntent),
    ShellRemovalPlan {
        lease: std::sync::Arc<super::shell::HomeLease>,
        receipt: super::shell::Receipt,
    },
    ShellRemove {
        lease: std::sync::Arc<super::shell::HomeLease>,
        plan: super::shell::RemovalPlan,
    },
    ShellInspect {
        home: PathBuf,
        name: String,
    },
    ShellAcquire {
        home: PathBuf,
        layout: Layout,
    },
    ShellPlan {
        lease: std::sync::Arc<super::shell::HomeLease>,
        name: String,
        block: String,
    },
    ShellVerify {
        lease: std::sync::Arc<super::shell::HomeLease>,
        receipt: super::shell::Receipt,
    },
    ShellApply {
        lease: std::sync::Arc<super::shell::HomeLease>,
        receipt: super::shell::Receipt,
    },
    EnsureCommandDirectory(PathBuf),
    ObservePath(PathBuf),
    ObserveCommandPath(Layout),
    InspectResourceDirectory {
        path: PathBuf,
        limit: usize,
    },
    #[cfg(test)]
    GatedRead {
        path: PathBuf,
        entered: std::sync::mpsc::SyncSender<()>,
        release: std::sync::mpsc::Receiver<()>,
    },
    #[cfg(test)]
    GatedWrite {
        path: PathBuf,
        entered: std::sync::mpsc::SyncSender<()>,
        release: std::sync::mpsc::Receiver<()>,
    },
    ReadRecord(Layout),
    PublishRecord {
        path: PathBuf,
        bytes: Vec<u8>,
        committing: bool,
    },
    #[cfg(all(test, target_os = "linux"))]
    PublishPolicyIntent {
        layout: Layout,
        expected: Option<Vec<u8>>,
        bytes: Vec<u8>,
    },
    #[cfg(target_os = "linux")]
    ClearPolicyIntent {
        layout: Layout,
        expected: Vec<u8>,
    },
    PublishReviewedRecord {
        layout: Layout,
        expected: Option<Vec<u8>>,
        active: Option<PathBuf>,
        resources: Option<(
            usize,
            Option<(super::resources::ParentObservation, Inventory)>,
        )>,
        bytes: Vec<u8>,
        committing: bool,
    },
    InspectLink(PathBuf),
    InspectPath(PathBuf),
    EnsureCommandLink {
        layout: Layout,
        command: ManagedCommand,
    },
    RemoveCommandLink {
        path: PathBuf,
        target: PathBuf,
    },
    PrepareActiveLink {
        layout: Layout,
        target: PathBuf,
    },
    RenameActiveLink {
        layout: Layout,
        expected: Option<PathBuf>,
        target: PathBuf,
    },
    RemoveNextLink {
        layout: Layout,
        expected: PathBuf,
    },
    EnsureSnapshotDirectory(PathBuf),
    ReadSnapshot {
        path: PathBuf,
        maximum: usize,
    },
    PublishSnapshot {
        path: PathBuf,
        bytes: Vec<u8>,
    },
    PruneSnapshot {
        directory: PathBuf,
        files: Vec<FileIdentity>,
    },
    CanonicalExecutable,
    HashExecutor {
        path: PathBuf,
        expected: FileIdentity,
    },
    HashPolicyHelper {
        path: PathBuf,
        expected: FileIdentity,
        location: PolicyHelperLocation,
    },
    #[cfg(target_os = "linux")]
    ObserveStockPolicy {
        executable: ExactExecutable,
        rendezvous: Rendezvous,
        caller: VerifiedCaller,
        common_inputs: [u8; 32],
        operation: super::nix_owner::policy_observation::PolicyOperation,
    },
    #[cfg(target_os = "linux")]
    VerifyExistingNixPolicy {
        inspector: ExistingNixInspector,
        lease: super::PolicyStateLease,
        stamp: [u8; 32],
    },
    #[cfg(target_os = "linux")]
    PublishStockPolicy {
        executable: ExactExecutable,
        rendezvous: Rendezvous,
        caller: VerifiedCaller,
        intent: super::coordinator::PreparedStockPolicyIntent,
    },
    #[cfg(target_os = "linux")]
    InspectStockPolicy(super::coordinator::StockPolicyIntent),
    #[cfg(target_os = "linux")]
    ResumeStockPolicy(super::coordinator::StockPolicyIntent),
    ResolvePairMember {
        member: PathBuf,
        expected: PathBuf,
    },
    NixClient,
    NixPolicyHelperClient,
    ProtectedPath(PathBuf),
    BoundedDigest {
        path: PathBuf,
        maximum: u64,
    },
    NixDaemonProcess {
        pid: u32,
        executable: PathBuf,
    },
    NixProcessName,
    NixConfigurationInput,
    OpenResourceDirectory(PathBuf),
    ResourceInventory {
        directory: ResourceDirectory,
        limit: usize,
    },
    RemoveResource {
        directory: ResourceDirectory,
        name: String,
        expected: LinkObservation,
        limit: usize,
    },
}

#[cfg(target_os = "linux")]
pub(super) enum ExistingNixInspector {
    Installed(crate::candidate::OutputPath),
    Retained {
        executable: ExactExecutable,
        caller: VerifiedCaller,
    },
}

pub(super) enum FilesystemResult {
    #[cfg(target_os = "linux")]
    ExistingNixPolicyVerified(VerifiedExistingPolicy),
    #[cfg(target_os = "linux")]
    StockPolicyObserved {
        executable: ExactExecutable,
        caller: VerifiedCaller,
        discovery: super::nix_owner::peer_handoff::launcher::StockDiscovery,
    },
    #[cfg(target_os = "linux")]
    StockPolicyPublished(super::nix_owner::peer_handoff::launcher::VerifiedPolicyInspection),
    #[cfg(target_os = "linux")]
    StockPolicyInspected(super::nix_owner::peer_handoff::launcher::StockPolicyInspection),
    #[cfg(target_os = "linux")]
    StockPolicyResumed(super::nix_owner::peer_handoff::launcher::VerifiedPolicyInspection),
    NixDaemonProcess(super::nix_owner::KernelDaemonObservation),
    ConfigurationInput(super::nix_owner::ConfigurationObservation),
    UninstallIntent(super::uninstall::UninstallIntent),
    ShellRemovalPlan(super::shell::RemovalPlan),
    ShellPreimage(Option<super::shell::ProfilePreimage>),
    ShellLease(std::sync::Arc<super::shell::HomeLease>),
    ShellPlan(super::shell::Receipt),
    ObservedPath(PathPreimage),
    ObservedCommandPath([CommandOnPath; 2]),
    ObservedResources(Option<(super::resources::ParentObservation, Inventory)>),
    JournalPublication {
        kind: SettlementKind,
        path: PathBuf,
        before: Option<Vec<u8>>,
        bytes: Vec<u8>,
        result: std::result::Result<(), TransactionError>,
    },

    Startup {
        layout: Layout,
        lease: RootLease,
    },
    Bytes(Option<Vec<u8>>),
    Link(Option<PathBuf>),
    Path(Option<fs::Metadata>),
    Executable(PathBuf),
    Digest(String),
    ResourceDirectory(ResourceDirectory),
    Inventory {
        directory: ResourceDirectory,
        result: Result<Inventory, TransactionError>,
    },
    ResourceRemoved {
        directory: ResourceDirectory,
        result: Result<(), TransactionError>,
    },
    Applied,
}

impl FilesystemJob {
    pub(super) fn journal_kind(&self) -> Option<SettlementKind> {
        let Self::PublishRecord { bytes, .. } = self else {
            return None;
        };
        let record: super::InstallationRecord = serde_json::from_slice(bytes).ok()?;
        match (
            record.activation.is_some(),
            record.resources.pending.is_some(),
            record
                .shell
                .as_ref()
                .is_some_and(super::shell::Ledger::pending),
        ) {
            (false, false, true) => Some(SettlementKind::Shell),
            (true, false, false) => Some(SettlementKind::Activation),
            (false, true, false) => Some(SettlementKind::Resource),
            _ => None,
        }
    }

    pub(super) fn mutates(&self) -> bool {
        match self {
            #[cfg(all(test, target_os = "linux"))]
            Self::PublishPolicyIntent { .. } => true,
            #[cfg(target_os = "linux")]
            Self::ObserveStockPolicy { .. } | Self::VerifyExistingNixPolicy { .. } => false,
            #[cfg(target_os = "linux")]
            Self::PublishStockPolicy { .. }
            | Self::ClearPolicyIntent { .. }
            | Self::InspectStockPolicy(_)
            | Self::ResumeStockPolicy(_) => true,
            Self::UninstallConfirm(_)
            | Self::UninstallInspect { .. }
            | Self::ShellInspect { .. }
            | Self::ShellVerify { .. }
            | Self::Startup(_)
            | Self::ReadRecord(_)
            | Self::InspectLink(_)
            | Self::InspectPath(_)
            | Self::ReadSnapshot { .. }
            | Self::CanonicalExecutable
            | Self::HashExecutor { .. }
            | Self::HashPolicyHelper { .. }
            | Self::ResolvePairMember { .. }
            | Self::NixClient
            | Self::NixPolicyHelperClient
            | Self::ProtectedPath(_)
            | Self::BoundedDigest { .. }
            | Self::NixConfigurationInput
            | Self::NixDaemonProcess { .. }
            | Self::NixProcessName
            | Self::ResourceInventory { .. }
            | Self::ObservePath(_)
            | Self::ObserveCommandPath(_)
            | Self::InspectResourceDirectory { .. } => false,
            Self::UninstallBind { .. }
            | Self::UninstallRemove(_)
            | Self::ShellRemovalPlan { .. }
            | Self::ShellRemove { .. }
            | Self::ShellAcquire { .. }
            | Self::ShellPlan { .. }
            | Self::ShellApply { .. }
            | Self::EnsureCommandDirectory(_)
            | Self::PublishRecord { .. }
            | Self::PublishReviewedRecord { .. }
            | Self::EnsureCommandLink { .. }
            | Self::RemoveCommandLink { .. }
            | Self::PrepareActiveLink { .. }
            | Self::RenameActiveLink { .. }
            | Self::RemoveNextLink { .. }
            | Self::EnsureSnapshotDirectory(_)
            | Self::PublishSnapshot { .. }
            | Self::PruneSnapshot { .. }
            | Self::OpenResourceDirectory(_)
            | Self::RemoveResource { .. } => true,
            #[cfg(test)]
            Self::GatedWrite { .. } => true,
            #[cfg(test)]
            Self::GatedRead { .. } => false,
        }
    }
    pub(super) fn permits_settlement(&self, reason: SettlementKind) -> bool {
        match self {
            Self::ShellAcquire { .. } | Self::ShellApply { .. } => {
                matches!(reason, SettlementKind::Shell | SettlementKind::Recovery)
            }
            Self::EnsureCommandDirectory(_) => reason == SettlementKind::Recovery,
            Self::UninstallConfirm(_)
            | Self::UninstallInspect { .. }
            | Self::ShellInspect { .. }
            | Self::ShellVerify { .. }
            | Self::ReadRecord(_)
            | Self::InspectLink(_)
            | Self::InspectPath(_)
            | Self::ObservePath(_)
            | Self::InspectResourceDirectory { .. } => true,
            Self::PublishRecord { .. } => reason != SettlementKind::Observation,
            Self::PublishReviewedRecord { .. } => matches!(
                reason,
                SettlementKind::Recovery | SettlementKind::Activation
            ),
            Self::EnsureCommandLink { .. }
            | Self::RemoveCommandLink { .. }
            | Self::PrepareActiveLink { .. }
            | Self::RenameActiveLink { .. }
            | Self::RemoveNextLink { .. } => matches!(
                reason,
                SettlementKind::Activation | SettlementKind::Recovery
            ),
            Self::OpenResourceDirectory(_) | Self::ResourceInventory { .. } => {
                matches!(reason, SettlementKind::Resource | SettlementKind::Recovery)
            }
            _ => false,
        }
    }
}

pub(super) struct FilesystemTask {
    handle: Option<JoinHandle<Result<FilesystemResult, TransactionError>>>,
    completion: UnixStream,
    notified: bool,
    attended: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct PathPreimage {
    pub(super) entries: Vec<(PathBuf, Option<(u64, u64, u32, u32, Option<PathBuf>)>)>,
}

fn observe_path(path: &Path) -> Result<PathPreimage, TransactionError> {
    let mut entries = Vec::new();
    let mut current = path;
    loop {
        match fs::symlink_metadata(current) {
            Ok(metadata) => {
                let target = if metadata.file_type().is_symlink() {
                    Some(read_owned_link(current)?.ok_or(TransactionError::UnsafePath)?)
                } else {
                    None
                };
                entries.push((
                    current.to_owned(),
                    Some((
                        metadata.dev(),
                        metadata.ino(),
                        metadata.uid(),
                        metadata.mode(),
                        target,
                    )),
                ));
                break;
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                entries.push((current.to_owned(), None));
                current = current.parent().ok_or(TransactionError::UnsafePath)?;
            }
            Err(_) => return Err(TransactionError::UnsafePath),
        }
    }
    Ok(PathPreimage { entries })
}

impl FilesystemTask {
    pub(super) fn start(
        job: FilesystemJob,
        lease: Option<File>,
        cancellation: Cancellation,
        admission: Admission,
        permission: Option<super::coordinator::MutationPermission>,
        terminal: Option<ParkedTerminal>,
    ) -> Result<Self, TransactionError> {
        if matches!(job, FilesystemJob::ShellApply { .. })
            && !matches!(admission, Admission::Settlement(_))
        {
            return Err(TransactionError::InvalidState(
                "shell edit requires journal settlement",
            ));
        }
        if job.mutates() && permission.is_none() {
            return Err(TransactionError::InvalidState(
                "filesystem requires accepted review",
            ));
        }
        if lease.is_none() != matches!(job, FilesystemJob::Startup(_)) {
            return Err(TransactionError::InvalidState(
                "filesystem effect before lease",
            ));
        }
        if let Admission::Settlement(permit) = &admission {
            if !job.permits_settlement(permit.kind()) {
                return Err(TransactionError::InvalidState(
                    "filesystem settlement authority",
                ));
            }
        }
        if terminal
            .as_ref()
            .is_some_and(|terminal| !terminal.foreground())
        {
            return Err(TransactionError::AdministratorUnavailable);
        }
        let (completion, mut signal) = UnixStream::pair().map_err(|_| TransactionError::Io)?;
        completion
            .set_nonblocking(true)
            .map_err(|_| TransactionError::Io)?;
        let attended = terminal.is_some();
        let handle = std::thread::Builder::new()
            .name("neo-filesystem".into())
            .spawn(move || {
                let result = execute(
                    job,
                    &ReadPolicy {
                        cancellation,
                        admission,
                    },
                    terminal.as_ref(),
                );
                drop(lease);
                let _ = signal.write_all(&[1]);
                result
            })
            .map_err(|_| TransactionError::Io)?;
        Ok(Self {
            handle: Some(handle),
            completion,
            notified: false,
            attended,
        })
    }

    pub(super) fn uses_terminal(&self) -> bool {
        self.attended
    }

    pub(super) fn poll(&mut self) -> Option<Result<FilesystemResult, TransactionError>> {
        let mut byte = [0];
        if self.completion.read(&mut byte).is_ok() {
            self.notified = true;
        }
        if !self.handle.as_ref()?.is_finished() {
            return None;
        }
        Some(
            self.handle
                .take()
                .unwrap()
                .join()
                .unwrap_or(if self.attended {
                    Err(TransactionError::CleanupUnknown)
                } else {
                    Err(TransactionError::Io)
                }),
        )
    }

    pub(super) fn interest(&self) -> WaitInterest {
        WaitInterest {
            descriptor: (!self.notified).then(|| (self.completion.as_raw_fd(), libc::POLLIN)),
            deadline: Instant::now() + Duration::from_millis(if self.notified { 1 } else { 10 }),
        }
    }
}

impl Drop for FilesystemTask {
    fn drop(&mut self) {
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

pub(super) struct ReadPolicy {
    cancellation: Cancellation,
    admission: Admission,
}
impl ReadPolicy {
    #[cfg(all(test, target_os = "linux"))]
    pub(super) fn requested_for_test(cancellation: Cancellation) -> Self {
        Self {
            cancellation,
            admission: Admission::Requested,
        }
    }

    fn check(&self) -> Result<(), TransactionError> {
        if matches!(self.admission, Admission::Requested) && self.cancellation.requested() {
            Err(TransactionError::StopRequested)
        } else {
            Ok(())
        }
    }
}

fn read_bounded(
    mut file: impl Read,
    maximum: usize,
    size_limit_error: TransactionError,
    policy: &ReadPolicy,
) -> Result<Vec<u8>, TransactionError> {
    let mut bytes = Vec::new();
    let mut buffer = [0; 32 * 1024];
    loop {
        policy.check()?;
        let count = file.read(&mut buffer).map_err(|_| TransactionError::Io)?;
        if count == 0 {
            return Ok(bytes);
        }
        if bytes.len().saturating_add(count) > maximum {
            return Err(size_limit_error);
        }
        bytes.extend_from_slice(&buffer[..count]);
    }
}

fn nar_string(digest: &mut Sha256, bytes: &[u8]) {
    digest.update((bytes.len() as u64).to_le_bytes());
    digest.update(bytes);
    digest.update(&[0; 7][..(8 - bytes.len() % 8) % 8]);
}

pub(super) fn helper_snapshot(
    metadata: &fs::Metadata,
) -> (u64, u64, u64, u32, u32, u32, i64, i64, i64, i64) {
    (
        metadata.dev(),
        metadata.ino(),
        metadata.len(),
        metadata.mode(),
        metadata.uid(),
        metadata.gid(),
        metadata.mtime(),
        metadata.mtime_nsec(),
        metadata.ctime(),
        metadata.ctime_nsec(),
    )
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum PolicyHelperLocation {
    Source,
    Store,
    #[cfg(target_os = "linux")]
    Installed,
}

pub(super) fn protected_helper_path(path: &Path, location: PolicyHelperLocation) -> bool {
    let checked = match location {
        PolicyHelperLocation::Store => path,
        #[cfg(target_os = "linux")]
        PolicyHelperLocation::Installed => match path.parent().and_then(Path::parent) {
            Some(output) if path == output.join("bin/neo") => output,
            _ => return false,
        },
        PolicyHelperLocation::Source => return false,
    };
    checked
        .to_str()
        .is_some_and(|text| crate::candidate::OutputPath::parse(text).is_ok())
        && super::nix_owner::policy_helper_protected_path(path)
            .ok()
            .as_deref()
            == Some(path)
}

fn verify_executor_file(
    path: &Path,
    expected: &FileIdentity,
    helper: Option<PolicyHelperLocation>,
    policy: &ReadPolicy,
    mut visit: impl FnMut(&[u8]),
) -> Result<(fs::File, fs::Metadata), TransactionError> {
    let stored = helper.is_some_and(|location| location != PolicyHelperLocation::Source);
    policy.check()?;
    if !path.is_absolute() {
        return Err(TransactionError::Verification("executor absolute source"));
    }
    if let Some(location) = helper {
        if stored && !protected_helper_path(path, location) {
            return Err(TransactionError::UnsafePath);
        }
    }
    let mut file = OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
        .open(path)
        .map_err(|_| TransactionError::Verification("native executor file"))?;
    let before = file.metadata().map_err(|_| TransactionError::Io)?;
    if before.len() != expected.size as u64 {
        return Err(TransactionError::Verification("native executor size"));
    }
    if !before.is_file()
        || (helper.is_some() && before.mode() & 0o100 == 0)
        || (stored && (before.uid() != 0 || before.mode() & 0o7777 != 0o555))
    {
        return Err(TransactionError::Verification("executor file identity"));
    }
    let mut raw = Sha256::new();
    let mut count = 0usize;
    let mut buffer = [0; 32 * 1024];
    loop {
        policy.check()?;
        let size = file.read(&mut buffer).map_err(|_| TransactionError::Io)?;
        if size == 0 {
            break;
        }
        count = count
            .checked_add(size)
            .ok_or(TransactionError::Verification("native executor size"))?;
        if count > expected.size {
            return Err(TransactionError::Verification("native executor size"));
        }
        raw.update(&buffer[..size]);
        visit(&buffer[..size]);
    }
    let after = file.metadata().map_err(|_| TransactionError::Io)?;
    let named = fs::symlink_metadata(path).map_err(|_| TransactionError::Io)?;
    if helper_snapshot(&before) != helper_snapshot(&after)
        || helper_snapshot(&after) != helper_snapshot(&named)
        || count != expected.size
        || format!("{:x}", raw.finalize()) != expected.sha
    {
        return Err(TransactionError::Verification("native executor hash"));
    }
    policy.check()?;
    Ok((file, after))
}

pub(super) fn open_verified_helper(
    path: &Path,
    expected: &FileIdentity,
    location: PolicyHelperLocation,
) -> Result<(fs::File, fs::Metadata), TransactionError> {
    verify_executor_file(
        path,
        expected,
        Some(location),
        &ReadPolicy {
            cancellation: Cancellation::default(),
            admission: Admission::Requested,
        },
        |_| {},
    )
}

fn hash_helper(
    path: &Path,
    expected: &FileIdentity,
    location: PolicyHelperLocation,
    policy: &ReadPolicy,
) -> Result<String, TransactionError> {
    let mut nar = Sha256::new();
    for tag in [
        b"nix-archive-1".as_slice(),
        b"(",
        b"type",
        b"regular",
        b"executable",
        b"",
        b"contents",
    ] {
        nar_string(&mut nar, tag);
    }
    nar.update((expected.size as u64).to_le_bytes());
    verify_executor_file(path, expected, Some(location), policy, |chunk| {
        nar.update(chunk)
    })?;
    nar.update(&[0; 7][..(8 - expected.size % 8) % 8]);
    nar_string(&mut nar, b")");
    Ok(format!("{:x}", nar.finalize()))
}

fn execute(
    job: FilesystemJob,
    policy: &ReadPolicy,
    terminal: Option<&ParkedTerminal>,
) -> Result<FilesystemResult, TransactionError> {
    #[cfg(not(target_os = "linux"))]
    let _ = terminal;
    use FilesystemJob as Job;
    use FilesystemResult as Result;
    let journal = job.journal_kind();
    match job {
        Job::UninstallInspect { layout, record } => {
            policy.check()?;
            Ok(Result::UninstallIntent(super::uninstall::prepare(
                &layout, &record,
            )?))
        }
        Job::UninstallConfirm(intent) => {
            policy.check()?;
            super::uninstall::confirm(&intent)?;
            Ok(Result::Applied)
        }
        Job::UninstallBind { layout, intent } => {
            policy.check()?;
            Ok(Result::UninstallIntent(super::uninstall::bind(
                &layout, intent,
            )?))
        }
        Job::UninstallRemove(entry) => {
            policy.check()?;
            super::uninstall::remove(&entry)?;
            Ok(Result::Applied)
        }
        Job::ShellRemovalPlan { lease, receipt } => {
            policy.check()?;
            Ok(Result::ShellRemovalPlan(lease.plan_removal(&receipt)?))
        }
        Job::ShellRemove { lease, plan } => {
            policy.check()?;
            lease.remove(&plan)?;
            Ok(Result::Applied)
        }
        Job::ShellInspect { home, name } => Ok(Result::ShellPreimage(
            super::shell::inspect_profile(&home, &name)?,
        )),
        Job::ShellAcquire { home, layout } => {
            policy.check()?;
            Ok(Result::ShellLease(super::shell::HomeLease::acquire(
                home, &layout,
            )?))
        }
        Job::ShellPlan { lease, name, block } => {
            policy.check()?;
            Ok(Result::ShellPlan(lease.plan(name, block)?))
        }
        Job::ShellVerify { lease, receipt } => {
            policy.check()?;
            lease.verify(&receipt)?;
            Ok(Result::Applied)
        }
        Job::ShellApply { lease, receipt } => {
            policy.check()?;
            lease.apply(&receipt)?;
            Ok(Result::Applied)
        }
        #[cfg(test)]
        Job::GatedRead {
            path,
            entered,
            release,
        } => {
            entered.send(()).map_err(|_| TransactionError::Io)?;
            release.recv().map_err(|_| TransactionError::Io)?;
            policy.check()?;
            Ok(Result::Bytes(Some(
                fs::read(path).map_err(|_| TransactionError::Io)?,
            )))
        }
        Job::EnsureCommandDirectory(path) => {
            super::runtime::create_directory(&path)?;
            #[cfg(test)]
            policy.cancellation.hit("after_reviewed_command_directory");
            Ok(Result::Applied)
        }
        Job::ObservePath(path) => Ok(Result::ObservedPath(observe_path(&path)?)),
        Job::ObserveCommandPath(layout) => Ok(Result::ObservedCommandPath(
            inspect_commands_on_path(&layout, layout.command_path().as_deref(), || policy.check())?,
        )),
        Job::InspectResourceDirectory { path, limit } => {
            let observed = ResourceDirectory::open_existing(path)?
                .map(|directory| Ok((directory.identity.clone(), directory.inventory(limit)?)))
                .transpose()?;
            Ok(Result::ObservedResources(observed))
        }
        #[cfg(test)]
        Job::GatedWrite {
            path,
            entered,
            release,
        } => {
            entered.send(()).map_err(|_| TransactionError::Io)?;
            release.recv().map_err(|_| TransactionError::Io)?;
            fs::write(path, b"admitted write completed").map_err(|_| TransactionError::Io)?;
            Ok(Result::Applied)
        }
        Job::Startup(layout) => {
            let layout = match layout {
                Some(layout) => layout,
                None => Layout::for_user()?,
            };
            layout.validate()?;
            let lease = RootLease::acquire(&layout.lock())?;
            Ok(Result::Startup { layout, lease })
        }
        Job::ReadRecord(layout) => {
            for legacy in ["receipt.json", "intent.json"] {
                if path_kind(&layout.state_directory.join(legacy))?.is_some() {
                    return Err(TransactionError::InvalidState(
                        "legacy installation state requires an explicit migration",
                    ));
                }
            }
            let bytes = read_record_bytes(&layout.record(), policy)?;
            Ok(Result::Bytes(bytes))
        }
        Job::ReadSnapshot { path, maximum } => Ok(Result::Bytes(
            open_owned_file(&path, true, false)?
                .map(|file| {
                    read_bounded(
                        file,
                        maximum,
                        TransactionError::Verification("snapshot file size"),
                        policy,
                    )
                })
                .transpose()?,
        )),
        Job::InspectLink(path) => {
            policy.check()?;
            Ok(Result::Link(read_owned_link(&path)?))
        }
        Job::InspectPath(path) => {
            policy.check()?;
            Ok(Result::Path(path_kind(&path)?))
        }
        Job::PublishRecord {
            path,
            bytes,
            committing,
        } => {
            if let Some(kind) = journal.filter(|_| matches!(policy.admission, Admission::Requested))
            {
                let before = read_record_bytes(&path, policy)?;
                let result = publish_record(&path, &bytes, committing, policy);
                #[cfg(test)]
                let result = if result.is_ok()
                    && policy
                        .cancellation
                        .is_cut("after_journal_publication_error")
                {
                    Err(TransactionError::Io)
                } else {
                    result
                };
                return Ok(Result::JournalPublication {
                    kind,
                    path,
                    before,
                    bytes,
                    result,
                });
            }
            publish_record(&path, &bytes, committing, policy)?;
            Ok(Result::Applied)
        }
        #[cfg(all(test, target_os = "linux"))]
        Job::PublishPolicyIntent {
            layout,
            expected,
            bytes,
        } => {
            publish_policy_intent(&layout, expected.as_deref(), &bytes, policy)?;
            Ok(Result::Applied)
        }
        #[cfg(target_os = "linux")]
        Job::ClearPolicyIntent { layout, expected } => {
            let path = layout.record();
            if read_record_bytes(&path, policy)?.as_deref() != Some(expected.as_slice()) {
                return Err(TransactionError::InvalidState(
                    "Nix policy intent changed before settlement",
                ));
            }
            let mut record: super::InstallationRecord = serde_json::from_slice(&expected)
                .map_err(|_| TransactionError::InvalidState("Nix policy record format"))?;
            let lease = record
                .pending_nix_policy
                .take()
                .and_then(|pending| pending.state_lease)
                .ok_or(TransactionError::InvalidState(
                    "Nix policy state lease missing",
                ))?;
            let named = fs::symlink_metadata(layout.lock()).map_err(|_| TransactionError::Io)?;
            super::validate_private_file(&named)?;
            if (named.dev(), named.ino(), named.nlink()) != (lease.device, lease.inode, 1) {
                return Err(TransactionError::InvalidState(
                    "Nix policy state lease changed",
                ));
            }
            let bytes = serde_json::to_vec(&record)
                .map_err(|_| TransactionError::InvalidState("Nix policy record format"))?;
            policy.check()?;
            publish_record(&path, &bytes, false, policy)?;
            Ok(Result::Applied)
        }
        Job::PublishReviewedRecord {
            layout,
            expected,
            active,
            resources,
            bytes,
            committing,
        } => {
            let current = read_record_bytes(&layout.record(), policy)?;
            if current != expected || read_owned_link(&layout.active_link())? != active {
                return Err(TransactionError::InvalidState(
                    "reviewed recovery changed before publication",
                ));
            }
            if let Some((limit, expected)) = resources {
                let observed =
                    ResourceDirectory::open_existing(layout.state_directory.join("resources"))?
                        .map(|directory| {
                            Ok((directory.identity.clone(), directory.inventory(limit)?))
                        })
                        .transpose()?;
                if observed != expected {
                    return Err(TransactionError::InvalidState(
                        "reviewed resources changed before publication",
                    ));
                }
            }
            publish_record(&layout.record(), &bytes, committing, policy)?;
            Ok(Result::Applied)
        }
        Job::EnsureCommandLink { layout, command } => {
            let path = layout.command_directory.join(command.name());
            let target = layout.expected_command_target(command.name());
            match read_owned_link(&path).map_err(|_| TransactionError::CommandLinkConflict)? {
                Some(actual) if actual == target => {}
                Some(_) => return Err(TransactionError::CommandLinkConflict),
                None => {
                    inspect_commands_on_path(&layout, layout.command_path().as_deref(), || {
                        policy.check()
                    })?;
                    symlink(target, &path).map_err(|_| TransactionError::CommandLinkConflict)?;
                    sync_directory(path.parent().ok_or(TransactionError::UnsafePath)?)?;
                }
            }
            Ok(Result::Applied)
        }
        Job::RemoveCommandLink { path, target } => {
            match read_owned_link(&path).map_err(|_| TransactionError::CommandLinkConflict)? {
                Some(actual) if actual == target => {
                    fs::remove_file(&path).map_err(|_| TransactionError::Io)?;
                    sync_directory(path.parent().ok_or(TransactionError::UnsafePath)?)?;
                }
                Some(_) => return Err(TransactionError::CommandLinkConflict),
                None => {}
            }
            Ok(Result::Applied)
        }
        Job::PrepareActiveLink { layout, target } => {
            if path_kind(&layout.active_next_link())?.is_some() {
                return Err(TransactionError::InvalidState(
                    "activation temporary already exists",
                ));
            }
            symlink(target, layout.active_next_link()).map_err(|_| TransactionError::Io)?;
            sync_directory(&layout.state_directory)?;
            #[cfg(test)]
            {
                policy.cancellation.hit("after_active_next_creation");
                super::pause_after("after_active_next_creation");
            }
            Ok(Result::Applied)
        }
        Job::RenameActiveLink {
            layout,
            expected,
            target,
        } => {
            if read_owned_link(&layout.active_link())? != expected
                || read_owned_link(&layout.active_next_link())?.as_ref() != Some(&target)
            {
                return Err(TransactionError::ForeignActiveTarget);
            }
            inspect_commands_on_path(&layout, layout.command_path().as_deref(), || policy.check())?;
            fs::rename(layout.active_next_link(), layout.active_link())
                .map_err(|_| TransactionError::Io)?;
            sync_directory(&layout.state_directory)?;
            #[cfg(test)]
            {
                policy.cancellation.hit("after_activation_rename");
                super::pause_after("after_activation_rename");
            }
            Ok(Result::Applied)
        }
        Job::RemoveNextLink { layout, expected } => {
            match read_owned_link(&layout.active_next_link())? {
                Some(actual) if actual == expected => {
                    fs::remove_file(layout.active_next_link()).map_err(|_| TransactionError::Io)?;
                    sync_directory(&layout.state_directory)?;
                }
                Some(_) => return Err(TransactionError::ForeignActiveTarget),
                None => {}
            }
            Ok(Result::Applied)
        }
        Job::EnsureSnapshotDirectory(path) => {
            super::snapshot::private_directory(&path)?;
            Ok(Result::Applied)
        }
        Job::PublishSnapshot { path, bytes } => {
            super::snapshot::write_bytes(&path, &bytes)?;
            Ok(Result::Applied)
        }
        Job::PruneSnapshot { directory, files } => {
            prune_snapshot(&directory, &files)?;
            Ok(Result::Applied)
        }
        Job::CanonicalExecutable => Ok(Result::Executable(
            std::env::current_exe()
                .and_then(|path| path.canonicalize())
                .map_err(|_| TransactionError::Verification("executor path"))?,
        )),
        Job::HashExecutor { path, expected } => {
            verify_executor_file(&path, &expected, None, policy, |_| {})?;
            Ok(Result::Applied)
        }
        Job::HashPolicyHelper {
            path,
            expected,
            location,
        } => Ok(Result::Digest(hash_helper(
            &path, &expected, location, policy,
        )?)),
        #[cfg(target_os = "linux")]
        Job::ObserveStockPolicy {
            executable,
            rendezvous,
            caller,
            common_inputs,
            operation,
        } => {
            let discovery = executable
                .discover_stock_policy(rendezvous, &caller, terminal, || policy.check().is_err())
                .map_err(|error| match error {
                    HandoffError::Cancelled => TransactionError::StopRequested,
                    HandoffError::CleanupUnknown => TransactionError::CleanupUnknown,
                    HandoffError::AdministratorUnavailable | HandoffError::ForegroundLost => {
                        TransactionError::AdministratorUnavailable
                    }
                    _ => TransactionError::Verification("helper policy discovery"),
                })?
                .map_err(|_| {
                    TransactionError::NixBlocked(
                        super::nix_owner::NixBlock::ConfigurationOwnerUnknown,
                    )
                })?;
            if discovery.common_inputs() != &common_inputs || discovery.operation() != operation {
                return Err(TransactionError::Verification(
                    "reviewed policy inputs changed",
                ));
            }
            policy.check()?;
            Ok(Result::StockPolicyObserved {
                executable,
                caller,
                discovery,
            })
        }
        #[cfg(target_os = "linux")]
        Job::VerifyExistingNixPolicy {
            inspector,
            lease,
            stamp,
        } => {
            let blocked = || {
                TransactionError::NixBlocked(super::nix_owner::NixBlock::ConfigurationOwnerUnknown)
            };
            let (executable, caller) = match inspector {
                ExistingNixInspector::Installed(output) => (
                    ExactExecutable::open_installed(&output).map_err(|_| blocked())?,
                    None,
                ),
                ExistingNixInspector::Retained { executable, caller } => (executable, Some(caller)),
            };
            let proof = executable
                .verify_existing_policy(caller.as_ref(), lease, stamp, terminal, || {
                    policy.check().is_err()
                })
                .map_err(|error| match error {
                    HandoffError::Cancelled => TransactionError::StopRequested,
                    HandoffError::CleanupUnknown => TransactionError::CleanupUnknown,
                    HandoffError::AdministratorUnavailable | HandoffError::ForegroundLost => {
                        TransactionError::AdministratorUnavailable
                    }
                    _ => blocked(),
                })?
                .ok_or_else(blocked)?;
            policy.check()?;
            Ok(Result::ExistingNixPolicyVerified(proof))
        }
        #[cfg(target_os = "linux")]
        Job::PublishStockPolicy {
            executable,
            rendezvous,
            caller,
            intent,
        } => {
            let pending = intent.pending().clone();
            let verified = executable
                .publish_stock_policy(
                    rendezvous,
                    &caller,
                    &pending,
                    terminal,
                    || policy.check().is_err(),
                    || {
                        intent.persist(policy).map_err(|error| match error {
                            TransactionError::StopRequested => HandoffError::Cancelled,
                            _ => HandoffError::Frame,
                        })
                    },
                )
                .map_err(|error| match error {
                    HandoffError::Cancelled => TransactionError::StopRequested,
                    HandoffError::CleanupUnknown => TransactionError::CleanupUnknown,
                    HandoffError::AdministratorUnavailable | HandoffError::ForegroundLost => {
                        TransactionError::AdministratorUnavailable
                    }
                    _ => TransactionError::Verification("Nix policy application is pending"),
                })?;
            Ok(Result::StockPolicyPublished(verified))
        }
        #[cfg(target_os = "linux")]
        job @ (Job::InspectStockPolicy(_) | Job::ResumeStockPolicy(_)) => {
            let (intent, resume) = match job {
                Job::InspectStockPolicy(intent) => (intent, false),
                Job::ResumeStockPolicy(intent) => (intent, true),
                _ => unreachable!(),
            };
            let unresolved = TransactionError::NixPolicyRecoveryPending;
            let pending = intent.pending();
            let caller =
                VerifiedCaller::open_current(&pending.helper).map_err(|_| unresolved.clone())?;
            let executable =
                ExactExecutable::open(Path::new(&pending.helper_path), &pending.helper)
                    .map_err(|_| unresolved.clone())?;
            let rendezvous = Rendezvous::fresh().map_err(|_| unresolved.clone())?;
            let map_error = |error| match error {
                HandoffError::Cancelled => TransactionError::StopRequested,
                HandoffError::CleanupUnknown => TransactionError::CleanupUnknown,
                HandoffError::AdministratorUnavailable | HandoffError::ForegroundLost if resume => {
                    TransactionError::AdministratorUnavailable
                }
                HandoffError::AdministratorUnavailable | HandoffError::ForegroundLost => {
                    TransactionError::NixPolicyRecoveryInspectionUnavailable
                }
                _ => unresolved,
            };
            let cancelled = || policy.check().is_err();
            let result = if resume {
                executable
                    .resume_stock_policy(rendezvous, &caller, intent, terminal, cancelled)
                    .map(Result::StockPolicyResumed)
            } else {
                executable
                    .inspect_stock_policy(rendezvous, &caller, &intent, terminal, cancelled)
                    .map(Result::StockPolicyInspected)
            }
            .map_err(map_error)?;
            policy.check()?;
            Ok(result)
        }
        Job::ResolvePairMember { member, expected } => {
            if member
                .canonicalize()
                .map_err(|_| TransactionError::Verification("pair executable"))?
                != expected
                    .canonicalize()
                    .map_err(|_| TransactionError::Verification("member executable"))?
            {
                return Err(TransactionError::Verification("pair member resolution"));
            }
            Ok(Result::Applied)
        }
        Job::NixClient => Ok(Result::Executable(
            super::nix_owner::trusted_client().map_err(TransactionError::NixBlocked)?,
        )),
        Job::NixPolicyHelperClient => Ok(Result::Executable(
            super::nix_owner::policy_helper_client().map_err(TransactionError::NixBlocked)?,
        )),
        Job::ProtectedPath(path) => Ok(Result::Executable(
            super::nix_owner::protected_path(&path).map_err(|_| TransactionError::Io)?,
        )),
        Job::BoundedDigest { path, maximum } => {
            let file = OpenOptions::new()
                .read(true)
                .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
                .open(path)
                .map_err(|_| TransactionError::Io)?;
            let metadata = file.metadata().map_err(|_| TransactionError::Io)?;
            if !metadata.is_file() || metadata.len() > maximum {
                return Err(TransactionError::Io);
            }
            Ok(Result::Digest(format!(
                "{:x}",
                Sha256::digest(read_bounded(
                    file,
                    maximum as usize,
                    TransactionError::Verification("file size limit"),
                    policy,
                )?)
            )))
        }
        Job::NixConfigurationInput => {
            policy.check()?;
            let configuration =
                super::nix_owner::configuration_input(Path::new("/etc/nix/nix.conf"), |file| {
                    read_bounded(
                        file,
                        128 * 1024,
                        TransactionError::Verification("file size limit"),
                        policy,
                    )
                })?;
            #[cfg(target_os = "linux")]
            let configuration = {
                let mut observed = configuration;
                observed.observe_caller_access();
                observed
            };
            policy.check()?;
            Ok(Result::ConfigurationInput(configuration))
        }
        Job::NixDaemonProcess { pid, executable } => {
            policy.check()?;
            Ok(Result::NixDaemonProcess(
                super::nix_owner::daemon_process::observe(pid, &executable, |file| {
                    read_bounded(
                        file,
                        64 * 1024,
                        TransactionError::Verification("process size limit"),
                        policy,
                    )
                })?,
            ))
        }
        Job::NixProcessName => Ok(Result::Bytes(Some(read_bounded(
            File::open("/proc/1/comm").map_err(|_| TransactionError::Io)?,
            4096,
            TransactionError::Verification("file size limit"),
            policy,
        )?))),
        Job::OpenResourceDirectory(path) => {
            Ok(Result::ResourceDirectory(ResourceDirectory::open(path)?))
        }
        Job::ResourceInventory { directory, limit } => {
            let result = policy.check().and_then(|_| directory.inventory(limit));
            Ok(Result::Inventory { directory, result })
        }
        Job::RemoveResource {
            directory,
            name,
            expected,
            limit,
        } => {
            let result = directory.remove(&name, &expected, limit);
            #[cfg(test)]
            let result = if result.is_ok() && policy.cancellation.is_cut("resource_remove_failure")
            {
                Err(TransactionError::Io)
            } else {
                result
            };
            Ok(Result::ResourceRemoved { directory, result })
        }
    }
}

fn read_record_bytes(
    path: &Path,
    policy: &ReadPolicy,
) -> Result<Option<Vec<u8>>, TransactionError> {
    open_owned_file(path, true, false)?
        .map(|file| {
            read_bounded(
                file,
                super::MAX_STATE_BYTES as usize,
                TransactionError::InvalidState("state record exceeds the size limit"),
                policy,
            )
        })
        .transpose()
}

#[cfg(target_os = "linux")]
pub(super) fn publish_policy_intent(
    layout: &Layout,
    expected: Option<&[u8]>,
    bytes: &[u8],
    policy: &ReadPolicy,
) -> Result<(), TransactionError> {
    let path = layout.record();
    if read_record_bytes(&path, policy)?.as_deref() != expected {
        return Err(TransactionError::InvalidState(
            "reviewed record changed before Nix policy intent publication",
        ));
    }
    #[cfg(test)]
    policy.cancellation.hit("before_policy_intent_publication");
    policy.check()?;
    publish_record(&path, bytes, false, policy)?;
    #[cfg(test)]
    if policy
        .cancellation
        .is_cut("after_policy_intent_publication_error")
    {
        return Err(TransactionError::Io);
    }
    Ok(())
}

fn publish_record(
    path: &Path,
    bytes: &[u8],
    committing: bool,
    policy: &ReadPolicy,
) -> Result<(), TransactionError> {
    let parent = path.parent().ok_or(TransactionError::UnsafePath)?;
    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or(TransactionError::UnsafePath)?;
    let temporary = parent.join(format!(".{name}.tmp"));
    remove_owned_file_if_present(&temporary)?;
    let _existing_target = open_owned_file(path, false, false)?;
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(&temporary)
        .map_err(|_| TransactionError::Io)?;
    file.set_permissions(fs::Permissions::from_mode(0o600))
        .map_err(|_| TransactionError::Io)?;
    file.write_all(bytes).map_err(|_| TransactionError::Io)?;
    file.sync_all().map_err(|_| TransactionError::Io)?;
    drop(file);
    #[cfg(test)]
    if committing {
        policy.cancellation.hit("before_receipt_rename");
        super::pause_after("before_receipt_rename");
    }
    #[cfg(not(test))]
    let _ = (committing, policy);
    fs::rename(&temporary, path).map_err(|_| TransactionError::Io)?;
    sync_directory(parent)?;
    #[cfg(test)]
    if committing {
        policy.cancellation.hit("after_receipt_rename");
        super::pause_after("after_receipt_rename");
    }
    Ok(())
}

fn prune_snapshot(directory: &Path, files: &[FileIdentity]) -> Result<(), TransactionError> {
    match fs::symlink_metadata(&directory) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Ok(());
        }
        Err(_) => return Err(TransactionError::Io),
        Ok(_) => {}
    }
    super::validate_directory(&directory, true)?;
    let parent = fs::symlink_metadata(&directory).map_err(|_| TransactionError::Io)?;
    let mut allowed = BTreeMap::new();
    {
        for file in files {
            allowed.insert(file.name.clone(), file.size);
            allowed.insert(format!(".{}.tmp", file.name), file.size);
        }
    }
    let mut observed = Vec::new();
    for entry in fs::read_dir(&directory).map_err(|_| TransactionError::Io)? {
        if observed.len() >= allowed.len() {
            return Err(TransactionError::InvalidState("foreign snapshot file"));
        }
        let entry = entry.map_err(|_| TransactionError::Io)?;
        let name = entry
            .file_name()
            .into_string()
            .map_err(|_| TransactionError::UnsafePath)?;
        let maximum = allowed
            .get(&name)
            .ok_or(TransactionError::InvalidState("foreign snapshot file"))?;
        let file = open_owned_file(&entry.path(), true, false)?
            .ok_or(TransactionError::InvalidState("snapshot file disappeared"))?;
        let metadata = file.metadata().map_err(|_| TransactionError::Io)?;
        if metadata.len() > *maximum as u64 {
            return Err(TransactionError::InvalidState(
                "snapshot file exceeds recorded bounds",
            ));
        }
        observed.push((entry.path(), metadata.dev(), metadata.ino()));
    }
    for (path, device, inode) in observed {
        let metadata = fs::symlink_metadata(&path).map_err(|_| TransactionError::Io)?;
        if metadata.dev() != device || metadata.ino() != inode {
            return Err(TransactionError::InvalidState(
                "snapshot file changed before removal",
            ));
        }
        #[cfg(test)]
        super::pause_after("before_snapshot_unlink");
        fs::remove_file(path).map_err(|_| TransactionError::Io)?;
        sync_directory(&directory)?;
        #[cfg(test)]
        super::pause_after("after_snapshot_unlink");
    }
    let current = fs::symlink_metadata(&directory).map_err(|_| TransactionError::Io)?;
    if current.dev() != parent.dev() || current.ino() != parent.ino() {
        return Err(TransactionError::UnsafePath);
    }
    fs::remove_dir(&directory).map_err(|_| TransactionError::Io)?;
    sync_directory(directory.parent().ok_or(TransactionError::UnsafePath)?)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn helper_nar_matches_independent_nix_vectors() {
        let temporary = tempfile::tempdir().unwrap();
        let path = temporary.path().canonicalize().unwrap().join("helper");
        let policy = ReadPolicy {
            cancellation: Cancellation::default(),
            admission: Admission::Requested,
        };
        for (bytes, expected_nar) in [
            (
                b"#!/bin/sh\nprintf \"%s\\n\" helper-fixture\n".to_vec(),
                "92bf8394ab84a8f865520a06b0ca2138d961bf39f27b84d96b6c2d307910e4c7",
            ),
            (
                vec![0; 8],
                "a0e542dbaff146ec07c4e46eb27841bd9a45a304203eaa34ecccb9b3022b08c3",
            ),
            (
                vec![0; 32769],
                "8e763ed1e1c58166a05d46044cd058f24863b94ffd211f28d1512c33e370f0dc",
            ),
        ] {
            fs::write(&path, &bytes).unwrap();
            fs::set_permissions(&path, fs::Permissions::from_mode(0o700)).unwrap();
            let identity = FileIdentity {
                name: "helper".into(),
                sha: format!("{:x}", Sha256::digest(&bytes)),
                size: bytes.len(),
            };
            assert_eq!(
                hash_helper(&path, &identity, PolicyHelperLocation::Source, &policy).unwrap(),
                expected_nar
            );
            fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
            assert!(hash_helper(&path, &identity, PolicyHelperLocation::Source, &policy).is_err());
            fs::set_permissions(&path, fs::Permissions::from_mode(0o700)).unwrap();
            let mut replaced = bytes.clone();
            replaced[0] ^= 1;
            fs::write(&path, replaced).unwrap();
            assert!(hash_helper(&path, &identity, PolicyHelperLocation::Source, &policy).is_err());
            fs::write(&path, &bytes).unwrap();
            let link = temporary.path().join("link");
            symlink(&path, &link).unwrap();
            assert!(hash_helper(&link, &identity, PolicyHelperLocation::Source, &policy).is_err());
            fs::remove_file(link).unwrap();
        }
    }

    #[test]
    fn helper_rejects_fifo_and_directory_without_blocking() {
        use std::os::unix::ffi::OsStrExt;
        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().canonicalize().unwrap();
        let fifo = directory.join("fifo");
        let name = std::ffi::CString::new(fifo.as_os_str().as_bytes()).unwrap();
        assert_eq!(unsafe { libc::mkfifo(name.as_ptr(), 0o700) }, 0);
        let identity = FileIdentity {
            name: "helper".into(),
            sha: format!("{:x}", Sha256::digest([])),
            size: 0,
        };
        let policy = ReadPolicy {
            cancellation: Cancellation::default(),
            admission: Admission::Requested,
        };
        assert!(hash_helper(&fifo, &identity, PolicyHelperLocation::Source, &policy).is_err());
        assert!(hash_helper(&directory, &identity, PolicyHelperLocation::Source, &policy).is_err());
    }

    #[test]
    fn adding_either_link_rechecks_both_command_names() {
        use std::os::unix::fs::PermissionsExt;
        let temporary = tempfile::tempdir().unwrap();
        let base = temporary.path().canonicalize().unwrap();
        let command_directory = base.join("owned");
        let other_directory = base.join("foreign");
        fs::create_dir(&command_directory).unwrap();
        fs::create_dir(&other_directory).unwrap();
        let foreign = other_directory.join("neo");
        fs::write(&foreign, b"foreign executable").unwrap();
        fs::set_permissions(&foreign, fs::Permissions::from_mode(0o700)).unwrap();
        let mut layout = Layout::new(base.join("state"), command_directory.clone());
        layout.command_path = Some(other_directory.into_os_string());
        let outcome = execute(
            FilesystemJob::EnsureCommandLink {
                layout,
                command: ManagedCommand::Neoctl,
            },
            &ReadPolicy {
                cancellation: Cancellation::default(),
                admission: Admission::Requested,
            },
            None,
        );
        assert!(
            matches!(outcome, Err(TransactionError::CommandPathConflict { command: ManagedCommand::Neo, path }) if path == foreign)
        );
        assert!(fs::symlink_metadata(command_directory.join("neoctl")).is_err());
    }

    #[test]
    fn completion_signal_does_not_join_a_thread_that_has_not_returned() {
        let temporary = tempfile::tempdir().unwrap();
        let lock = temporary.path().canonicalize().unwrap().join("lock");
        let lease = RootLease::acquire(&lock).unwrap();
        let retained = lease.file.try_clone().unwrap();
        let (completion, mut signal) = UnixStream::pair().unwrap();
        completion.set_nonblocking(true).unwrap();
        let (entered, entered_rx) = std::sync::mpsc::sync_channel(1);
        let (release, release_rx) = std::sync::mpsc::channel();
        let handle = std::thread::spawn(move || {
            signal.write_all(&[1]).unwrap();
            entered.send(()).unwrap();
            release_rx.recv().unwrap();
            drop(retained);
            Ok(FilesystemResult::Applied)
        });
        let mut task = FilesystemTask {
            handle: Some(handle),
            completion,
            notified: false,
            attended: false,
        };
        entered_rx.recv_timeout(Duration::from_secs(10)).unwrap();
        drop(lease);
        assert!(task.poll().is_none());
        assert!(task.interest().descriptor.is_none());
        assert!(matches!(
            RootLease::acquire(&lock),
            Err(TransactionError::Busy)
        ));
        release.send(()).unwrap();
        let result = loop {
            if let Some(result) = task.poll() {
                break result;
            }
            task.interest().wait();
        };
        assert!(result.is_ok());
        assert!(RootLease::acquire(&lock).is_ok());
    }
}

#[cfg(test)]
pub(super) fn test_digest(path: &Path, maximum: u64) -> Result<String, TransactionError> {
    match execute(
        FilesystemJob::BoundedDigest {
            path: path.to_owned(),
            maximum,
        },
        &ReadPolicy {
            cancellation: Cancellation::default(),
            admission: Admission::Requested,
        },
        None,
    )? {
        FilesystemResult::Digest(value) => Ok(value),
        _ => Err(TransactionError::InvalidState("digest completion type")),
    }
}
