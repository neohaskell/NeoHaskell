use super::coordinator::{Admission, Cancellation, SettlementKind, WaitInterest};
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
    ResolvePairMember {
        member: PathBuf,
        expected: PathBuf,
    },
    NixClient,
    ProtectedPath(PathBuf),
    BoundedDigest {
        path: PathBuf,
        maximum: u64,
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

pub(super) enum FilesystemResult {
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
            | Self::ResolvePairMember { .. }
            | Self::NixClient
            | Self::ProtectedPath(_)
            | Self::BoundedDigest { .. }
            | Self::NixConfigurationInput
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
        let (completion, mut signal) = UnixStream::pair().map_err(|_| TransactionError::Io)?;
        completion
            .set_nonblocking(true)
            .map_err(|_| TransactionError::Io)?;
        let handle = std::thread::Builder::new()
            .name("neo-filesystem".into())
            .spawn(move || {
                let result = execute(
                    job,
                    &ReadPolicy {
                        cancellation,
                        admission,
                    },
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
        })
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
                .unwrap_or(Err(TransactionError::Io)),
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

struct ReadPolicy {
    cancellation: Cancellation,
    admission: Admission,
}
impl ReadPolicy {
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

fn execute(job: FilesystemJob, policy: &ReadPolicy) -> Result<FilesystemResult, TransactionError> {
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
            let bytes = open_owned_file(&layout.record(), true, false)?
                .map(|file| {
                    read_bounded(
                        file,
                        super::MAX_STATE_BYTES as usize,
                        TransactionError::InvalidState("state record exceeds the size limit"),
                        policy,
                    )
                })
                .transpose()?;
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
                let before = open_owned_file(&path, true, false)?
                    .map(|file| {
                        read_bounded(
                            file,
                            super::MAX_STATE_BYTES as usize,
                            TransactionError::InvalidState("state record exceeds the size limit"),
                            policy,
                        )
                    })
                    .transpose()?;
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
        Job::PublishReviewedRecord {
            layout,
            expected,
            active,
            resources,
            bytes,
            committing,
        } => {
            let current = open_owned_file(&layout.record(), true, false)?
                .map(|file| {
                    read_bounded(
                        file,
                        super::MAX_STATE_BYTES as usize,
                        TransactionError::InvalidState("state record exceeds the size limit"),
                        policy,
                    )
                })
                .transpose()?;
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
            let mut file = File::open(path)
                .map_err(|_| TransactionError::Verification("native executor file"))?;
            if file.metadata().map_err(|_| TransactionError::Io)?.len() != expected.size as u64 {
                return Err(TransactionError::Verification("native executor size"));
            }
            let mut digest = Sha256::new();
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
                digest.update(&buffer[..size]);
            }
            if count != expected.size || format!("{:x}", digest.finalize()) != expected.sha {
                return Err(TransactionError::Verification("native executor hash"));
            }
            Ok(Result::Applied)
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
            Ok(Result::ConfigurationInput(
                super::nix_owner::configuration_input(Path::new("/etc/nix/nix.conf"), |file| {
                    read_bounded(
                        file,
                        128 * 1024,
                        TransactionError::Verification("file size limit"),
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
    )? {
        FilesystemResult::Digest(value) => Ok(value),
        _ => Err(TransactionError::InvalidState("digest completion type")),
    }
}
