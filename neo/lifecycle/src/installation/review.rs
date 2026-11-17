use super::coordinator::{RecoveryPending, ReviewOperation, ReviewSummary, ReviewedChange};
use super::filesystem::PathPreimage;
use super::resources::{Inventory, ParentObservation};
use super::{
    FilesystemJob, FilesystemResult, InstallationOwner, InstallationRecord, TransactionError,
};
use std::path::{Path, PathBuf};

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
    async fn record_bytes(&self) -> Result<Option<Vec<u8>>, TransactionError> {
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

    pub(super) async fn inspect_recovery(&self) -> Result<PreparedRecovery, TransactionError> {
        let bytes = self.record_bytes().await?;
        self.effects.check_recovery_preimage(bytes.as_deref())?;
        let record = self.decode_record(bytes.as_deref())?;
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
        for (command, repair) in [("neo", &mut repairs.neo), ("neoctl", &mut repairs.neoctl)] {
            let path = self.layout.command_directory.join(command);
            let expected = self.layout.expected_command_target(command);
            let actual = self.inspect_link(path.clone()).await?;
            if actual.as_ref().is_some_and(|target| target != &expected) {
                return Err(TransactionError::CommandLinkConflict);
            }
            if projected.committed.is_some() {
                if actual.is_none() {
                    *repair = Some(FilesystemJob::EnsureCommandLink {
                        path,
                        target: expected,
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
