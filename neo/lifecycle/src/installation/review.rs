use super::coordinator::{RecoveryPending, ReviewOperation, ReviewSummary, ReviewedChange};
use super::filesystem::PathPreimage;
use super::resources::{Inventory, ParentObservation};
use super::{
    FilesystemJob, FilesystemResult, InstallationOwner, InstallationRecord, TransactionError,
};
use std::env;
use std::ffi::{CString, OsStr};
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ManagedCommand {
    Neo,
    Neoctl,
}

impl ManagedCommand {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Neo => "neo",
            Self::Neoctl => "neoctl",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum CommandOnPath {
    Absent,
    Managed,
}

pub(super) fn inspect_commands_on_path(
    layout: &super::Layout,
    path: Option<&OsStr>,
    mut checkpoint: impl FnMut() -> Result<(), TransactionError>,
) -> Result<[CommandOnPath; 2], TransactionError> {
    Ok([
        inspect_command_on_path(layout, ManagedCommand::Neo, path, &mut checkpoint)?,
        inspect_command_on_path(layout, ManagedCommand::Neoctl, path, checkpoint)?,
    ])
}

fn inspect_command_on_path(
    layout: &super::Layout,
    command: ManagedCommand,
    path: Option<&OsStr>,
    mut checkpoint: impl FnMut() -> Result<(), TransactionError>,
) -> Result<CommandOnPath, TransactionError> {
    checkpoint()?;
    let Some(path) = path else {
        return Ok(CommandOnPath::Absent);
    };
    for directory in env::split_paths(path) {
        checkpoint()?;
        let candidate = directory.join(command.name());
        if !is_executable(&candidate) {
            continue;
        }
        let directory = if directory.as_os_str().is_empty() {
            Path::new(".")
        } else {
            &directory
        };
        if same_directory(directory, &layout.command_directory)
            && super::read_owned_link(&candidate)
                .is_ok_and(|target| target == Some(layout.expected_command_target(command.name())))
        {
            return Ok(CommandOnPath::Managed);
        }
        return Err(TransactionError::CommandPathConflict {
            command,
            path: std::path::absolute(&candidate).unwrap_or(candidate),
        });
    }
    Ok(CommandOnPath::Absent)
}

fn same_directory(left: &Path, right: &Path) -> bool {
    match (fs::metadata(left), fs::metadata(right)) {
        (Ok(left), Ok(right)) => left.dev() == right.dev() && left.ino() == right.ino(),
        _ => false,
    }
}

fn is_executable(path: &Path) -> bool {
    if !fs::metadata(path).is_ok_and(|metadata| metadata.is_file()) {
        return false;
    }
    CString::new(path.as_os_str().as_bytes()).is_ok_and(|path| unsafe {
        libc::faccessat(libc::AT_FDCWD, path.as_ptr(), libc::X_OK, libc::AT_EACCESS) == 0
    })
}

pub(super) struct PreparedRecovery {
    bytes: Option<Vec<u8>>,
    paths: Vec<(PathBuf, PathPreimage)>,
    resources: Option<(ParentObservation, Inventory)>,
    resource_limit: usize,
    pub(super) projected: InstallationRecord,
    repairs: RecoveryRepairs,
    pub(super) pending: RecoveryPending,
    pub(super) command_directory_missing: bool,
}

#[derive(Default)]
struct RecoveryRepairs {
    command_directory: Option<FilesystemJob>,
    next_link: Option<FilesystemJob>,
    neo: Option<FilesystemJob>,
    neoctl: Option<FilesystemJob>,
    committed: Option<FilesystemJob>,
    activation: Option<FilesystemJob>,
    resources: Option<FilesystemJob>,
}

impl RecoveryRepairs {
    fn required(&self) -> bool {
        self.command_directory.is_some()
            || self.next_link.is_some()
            || self.neo.is_some()
            || self.neoctl.is_some()
            || self.committed.is_some()
            || self.activation.is_some()
            || self.resources.is_some()
    }
}

impl InstallationOwner {
    pub(super) async fn record_bytes(&self) -> Result<Option<Vec<u8>>, TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::ReadRecord(self.layout.clone()))
            .await?
        {
            FilesystemResult::Bytes(bytes) => Ok(bytes),
            _ => Err(TransactionError::InvalidState("record completion type")),
        }
    }

    async fn path_preimage(&self, path: &Path) -> Result<PathPreimage, TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::ObservePath(path.to_owned()))
            .await?
        {
            FilesystemResult::ObservedPath(value) => Ok(value),
            _ => Err(TransactionError::InvalidState(
                "path preimage completion type",
            )),
        }
    }

    async fn resource_preimage(
        &self,
        limit: usize,
    ) -> Result<Option<(ParentObservation, Inventory)>, TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::InspectResourceDirectory {
                path: self.layout.state_directory.join("resources"),
                limit,
            })
            .await?
        {
            FilesystemResult::ObservedResources(value) => Ok(value),
            _ => Err(TransactionError::InvalidState(
                "resource preimage completion type",
            )),
        }
    }

    pub(super) async fn observe_command_path(
        &self,
    ) -> Result<[CommandOnPath; 2], TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::ObserveCommandPath(self.layout.clone()))
            .await?
        {
            FilesystemResult::ObservedCommandPath(commands) => Ok(commands),
            _ => Err(TransactionError::InvalidState(
                "command PATH completion type",
            )),
        }
    }

    pub(super) async fn recheck_command_path(
        &self,
        expected: [CommandOnPath; 2],
    ) -> Result<(), TransactionError> {
        if self.observe_command_path().await? != expected {
            return Err(self.effects.review_changed());
        }
        Ok(())
    }

    pub(super) async fn inspect_recovery(&self) -> Result<PreparedRecovery, TransactionError> {
        let bytes = self.record_bytes().await?;
        self.effects.check_recovery_preimage(bytes.as_deref())?;
        let record = self.decode_record(bytes.as_deref())?;
        if record
            .shell
            .as_ref()
            .is_some_and(super::shell::Ledger::pending)
        {
            return Err(TransactionError::InvalidState(
                "shell recovery must settle before activation or resources",
            ));
        }
        let pending = match (
            record.activation.is_some(),
            record.resources.pending.is_some(),
        ) {
            (true, true) => RecoveryPending::ActivationAndResources,
            (true, false) => RecoveryPending::Activation,
            (false, true) => RecoveryPending::Resources,
            (false, false) => RecoveryPending::None,
        };
        self.effects.recovery_pending(pending);
        let mut paths = Vec::new();
        for path in [
            self.layout.active_link(),
            self.layout.active_next_link(),
            self.layout.command_directory.clone(),
            self.layout.owned_links().neo,
            self.layout.owned_links().neoctl,
            self.layout.state_directory.clone(),
            self.layout.state_directory.join("resources"),
        ] {
            let observed = self.path_preimage(&path).await?;
            paths.push((path, observed));
        }
        let active = self.read_active_target().await?;
        let next = self.read_next_target().await?;
        let command_directory_missing = paths[2].1.entries[0].1.is_none();
        let mut projected = record.clone();
        let mut repairs = RecoveryRepairs::default();
        if let Some(intent) = &record.activation {
            if active.as_ref() == Some(&intent.after.active.output) {
                projected.committed = Some(intent.after.clone());
            } else if active.as_ref() != intent.before.as_ref().map(|r| &r.active.output)
                || record.committed != intent.before
            {
                return Err(TransactionError::ForeignActiveTarget);
            }
            if let Some(target) = next {
                if target != intent.after.active.output {
                    return Err(TransactionError::ForeignActiveTarget);
                }
                repairs.next_link = Some(FilesystemJob::RemoveNextLink {
                    layout: self.layout.clone(),
                    expected: target,
                });
            }
            projected.activation = None;
        } else if next.is_some()
            || active.as_ref() != record.committed.as_ref().map(|r| &r.active.output)
        {
            return Err(TransactionError::ForeignActiveTarget);
        }
        for (command, repair) in [
            (ManagedCommand::Neo, &mut repairs.neo),
            (ManagedCommand::Neoctl, &mut repairs.neoctl),
        ] {
            let path = self.layout.command_directory.join(command.name());
            let expected = self.layout.expected_command_target(command.name());
            if let Some(metadata) = self.inspect_path(path.clone()).await? {
                if metadata.is_file()
                    && metadata.uid() == unsafe { libc::geteuid() }
                    && metadata.mode() & 0o022 == 0
                {
                    return Err(TransactionError::CommandPathConflict { command, path });
                }
            }
            let actual = self.inspect_link(path.clone()).await?;
            if actual.as_ref().is_some_and(|target| target != &expected) {
                return Err(TransactionError::CommandLinkConflict);
            }
            if projected.committed.is_some() {
                if actual.is_none() {
                    *repair = Some(FilesystemJob::EnsureCommandLink {
                        layout: self.layout.clone(),
                        command,
                    });
                }
            } else if actual.is_some() {
                if record.activation.is_none() {
                    return Err(TransactionError::CommandLinkConflict);
                }
                *repair = Some(FilesystemJob::RemoveCommandLink {
                    path,
                    target: expected,
                });
            }
        }
        if command_directory_missing && projected.committed.is_some() {
            repairs.command_directory = Some(FilesystemJob::EnsureCommandDirectory(
                self.layout.command_directory.clone(),
            ));
        }
        let resource_limit = super::resource_io::pending_inventory_limit(&record.resources);
        let resources = self.resource_preimage(resource_limit).await?;
        match &resources {
            Some((parent, inventory)) => {
                if record
                    .resources
                    .parent
                    .as_ref()
                    .is_some_and(|expected| expected != parent)
                {
                    return Err(TransactionError::InvalidState(
                        "resource directory identity changed",
                    ));
                }
                if record.resources.parent.is_none() && !inventory.is_empty() {
                    return Err(TransactionError::InvalidState(
                        "unrecorded resource directory is not empty",
                    ));
                }
                projected
                    .resources
                    .reconcile(inventory.clone(), parent.uid)
                    .map_err(|_| {
                        TransactionError::InvalidState(
                            "resource effect does not explain observed links",
                        )
                    })?;
            }
            None if record.resources.parent.is_some()
                || !record.resources.links.is_empty()
                || record.resources.pending.is_some() =>
            {
                return Err(TransactionError::InvalidState(
                    "recorded resource directory is missing",
                ));
            }
            None => {}
        }
        let mut applied = record.clone();
        let mut expected_bytes = bytes.clone();
        if applied.committed != projected.committed {
            applied.committed = projected.committed.clone();
            repairs.committed =
                Some(self.recovery_record_job(&applied, &mut expected_bytes, true)?);
        }
        if applied.activation.is_some() {
            applied.activation = None;
            repairs.activation =
                Some(self.recovery_record_job(&applied, &mut expected_bytes, false)?);
        }
        if applied.resources != projected.resources {
            let mut job = self.recovery_record_job(&projected, &mut expected_bytes, false)?;
            if let FilesystemJob::PublishReviewedRecord {
                resources: expected,
                ..
            } = &mut job
            {
                *expected = Some((resource_limit, resources.clone()));
            }
            repairs.resources = Some(job);
        }
        Ok(PreparedRecovery {
            bytes,
            paths,
            resources,
            resource_limit,
            projected,
            repairs,
            pending,
            command_directory_missing,
        })
    }

    fn recovery_record_job(
        &self,
        record: &InstallationRecord,
        expected: &mut Option<Vec<u8>>,
        committing: bool,
    ) -> Result<FilesystemJob, TransactionError> {
        let bytes = serde_json::to_vec(record).map_err(|_| TransactionError::Io)?;
        if bytes.len() > super::MAX_STATE_BYTES as usize {
            return Err(TransactionError::InvalidState(
                "state record exceeds the size limit",
            ));
        }
        let previous = expected.replace(bytes.clone());
        Ok(FilesystemJob::PublishReviewedRecord {
            layout: self.layout.clone(),
            expected: previous,
            active: record
                .committed
                .as_ref()
                .map(|roles| roles.active.output.clone()),
            resources: None,
            bytes,
            committing,
        })
    }

    pub(super) async fn recheck_recovery(
        &self,
        prepared: &PreparedRecovery,
    ) -> Result<(), TransactionError> {
        if self.record_bytes().await? != prepared.bytes {
            return Err(self.effects.review_changed());
        }
        #[cfg(test)]
        self.effects.hit("during_review_recheck");
        for (path, expected) in &prepared.paths {
            if self.path_preimage(path).await? != *expected {
                return Err(self.effects.review_changed());
            }
        }
        if self.resource_preimage(prepared.resource_limit).await? != prepared.resources {
            return Err(self.effects.review_changed());
        }
        Ok(())
    }

    pub(super) async fn apply_recovery(
        &self,
        prepared: PreparedRecovery,
    ) -> Result<(), TransactionError> {
        let mut settlement = None;
        self.apply_recovery_repairs(prepared.repairs, &mut settlement)
            .await?;
        if let Some(settlement) = settlement {
            settlement.finish_settlement()?;
            self.effects.checkpoint()?;
        }

        Ok(())
    }
    pub(super) async fn recover_failed_journal(&self) -> Result<(), TransactionError> {
        let prepared = self.inspect_recovery().await?;
        let mut settlement = Some(self.effects.clone());
        self.apply_recovery_repairs(prepared.repairs, &mut settlement)
            .await
    }

    async fn apply_recovery_repairs(
        &self,
        repairs: RecoveryRepairs,
        settlement: &mut Option<super::Effects>,
    ) -> Result<(), TransactionError> {
        self.apply_recovery_repair(settlement, repairs.command_directory)
            .await?;
        self.apply_recovery_repair(settlement, repairs.next_link)
            .await?;
        self.apply_recovery_repair(settlement, repairs.neo).await?;
        self.apply_recovery_repair(settlement, repairs.neoctl)
            .await?;
        self.apply_recovery_repair(settlement, repairs.committed)
            .await?;
        self.apply_recovery_repair(settlement, repairs.activation)
            .await?;
        self.apply_recovery_repair(settlement, repairs.resources)
            .await?;
        Ok(())
    }

    async fn apply_recovery_repair(
        &self,
        settlement: &mut Option<super::Effects>,
        repair: Option<FilesystemJob>,
    ) -> Result<(), TransactionError> {
        if let Some(job) = repair {
            match settlement {
                Some(effects) => {
                    effects.filesystem(job).await?;
                }
                None => {
                    #[cfg(test)]
                    self.effects.hit("before_reviewed_recovery");
                    *settlement = Some(self.effects.begin_reviewed_recovery(job).await?);
                }
            }
        }
        Ok(())
    }
}

impl PreparedRecovery {
    pub(super) fn introduces_command_links(&self) -> bool {
        [&self.repairs.neo, &self.repairs.neoctl]
            .into_iter()
            .any(|repair| matches!(repair, Some(FilesystemJob::EnsureCommandLink { .. })))
    }

    pub(super) fn summary(
        &self,
        request: &super::snapshot::Request,
        version: Option<String>,
    ) -> ReviewSummary {
        let operation = match request {
            super::snapshot::Request::Install(_) => ReviewOperation::Install,
            super::snapshot::Request::Update(_) => ReviewOperation::Update,
            super::snapshot::Request::Rollback => ReviewOperation::Rollback,
            super::snapshot::Request::RecoverOnly => ReviewOperation::Recover,
            super::snapshot::Request::Uninstall => ReviewOperation::Uninstall,
        };
        let mut changes = Vec::new();
        if self.repairs.required() {
            changes.push(ReviewedChange::Recovery);
        }
        if self.command_directory_missing
            && (self.projected.committed.is_some()
                || !matches!(request, super::snapshot::Request::RecoverOnly))
        {
            changes.push(ReviewedChange::CommandDirectory);
        }
        if !matches!(request, super::snapshot::Request::RecoverOnly) {
            changes.extend([
                ReviewedChange::CommandLinks,
                ReviewedChange::Selection,
                ReviewedChange::Retention,
                ReviewedChange::Activation,
                ReviewedChange::Cleanup,
            ]);
        }
        ReviewSummary {
            operation,
            version,
            recovery: self.pending,
            changes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::{PermissionsExt, symlink};

    #[test]
    fn first_visible_executable_determines_command_ownership() {
        let root = tempfile::tempdir().unwrap();
        let root = root.path().canonicalize().unwrap();
        let managed = root.join("managed");
        let foreign = root.join("foreign");
        let alias = root.join("alias");
        let inactive = root.join("inactive");
        let layout = super::super::Layout::new(root.clone(), managed.clone());
        for directory in [&managed, &foreign, &inactive] {
            fs::create_dir(directory).unwrap();
        }
        symlink(&managed, &alias).unwrap();
        fs::create_dir(inactive.join("neo")).unwrap();
        fs::write(inactive.join("neoctl"), "").unwrap();
        for command in [ManagedCommand::Neo, ManagedCommand::Neoctl] {
            let name = command.name();
            fs::write(foreign.join(name), "").unwrap();
            fs::set_permissions(foreign.join(name), fs::Permissions::from_mode(0o700)).unwrap();
            symlink(layout.expected_command_target(name), managed.join(name)).unwrap();
        }
        let check = |directories: &[&Path]| {
            let path = env::join_paths(directories).unwrap();
            inspect_commands_on_path(&layout, Some(&path), || Ok(()))
        };
        assert_eq!(check(&[&inactive, &alias]), Ok([CommandOnPath::Absent; 2]));
        let first = foreign.join("neo");
        assert_eq!(
            check(&[&alias, &foreign]),
            Err(TransactionError::CommandPathConflict {
                command: ManagedCommand::Neo,
                path: first.clone(),
            })
        );
        fs::create_dir(root.join("bin")).unwrap();
        for name in ["neo", "neoctl"] {
            fs::hard_link(foreign.join(name), root.join("bin").join(name)).unwrap();
        }
        symlink(&root, layout.active_link()).unwrap();
        assert_eq!(
            check(&[&inactive, &alias, &foreign]),
            Ok([CommandOnPath::Managed; 2])
        );
        assert_eq!(
            check(&[&foreign, &managed]),
            Err(TransactionError::CommandPathConflict {
                command: ManagedCommand::Neo,
                path: first,
            })
        );
        fs::remove_file(managed.join("neoctl")).unwrap();
        assert_eq!(
            check(&[&managed, &foreign]),
            Err(TransactionError::CommandPathConflict {
                command: ManagedCommand::Neoctl,
                path: foreign.join("neoctl"),
            })
        );
    }
}
