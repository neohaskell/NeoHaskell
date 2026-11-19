use super::resources::{Inventory, LinkObservation, ParentObservation, ResourceEffect};
use super::{InstallationOwner, InstallationRecord, TransactionError, process, snapshot};
use crate::candidate::OutputPath;
use std::collections::BTreeSet;
use std::ffi::CString;
use std::fs::{self, File, OpenOptions};
use std::os::fd::AsRawFd;
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::path::PathBuf;

pub(super) struct HelperRetention {
    target: OutputPath,
}

impl HelperRetention {
    pub(super) fn target(&self) -> &OutputPath {
        &self.target
    }
}

pub(super) struct RetentionObservation {
    pair: OutputPath,
    root: PathBuf,
}

impl RetentionObservation {
    pub(super) fn pair(&self) -> &OutputPath {
        &self.pair
    }
    pub(super) fn root(&self) -> &std::path::Path {
        &self.root
    }
}

pub(super) struct ResourceDirectory {
    path: PathBuf,
    file: File,
    pub(super) identity: ParentObservation,
}

impl ResourceDirectory {
    pub(super) fn open_existing(path: PathBuf) -> Result<Option<Self>, TransactionError> {
        match fs::symlink_metadata(&path) {
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(_) => return Err(TransactionError::Io),
            Ok(_) => super::validate_directory(&path, true)?,
        }
        Self::open_checked(path).map(Some)
    }

    pub(super) fn open(path: PathBuf) -> Result<Self, TransactionError> {
        snapshot::private_directory(&path)?;
        Self::open_checked(path)
    }

    fn open_checked(path: PathBuf) -> Result<Self, TransactionError> {
        let file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_DIRECTORY)
            .open(&path)
            .map_err(|_| TransactionError::UnsafePath)?;
        let metadata = file.metadata().map_err(|_| TransactionError::Io)?;
        let identity = ParentObservation {
            device: metadata.dev(),
            inode: metadata.ino(),
            uid: metadata.uid(),
        };
        Ok(Self {
            path,
            file,
            identity,
        })
    }

    fn capability(&self) -> Result<Self, TransactionError> {
        Ok(Self {
            path: self.path.clone(),
            file: self.file.try_clone().map_err(|_| TransactionError::Io)?,
            identity: self.identity.clone(),
        })
    }

    async fn observe(
        &self,
        effects: &super::Effects,
        limit: usize,
    ) -> Result<Inventory, TransactionError> {
        match effects
            .filesystem(super::FilesystemJob::ResourceInventory {
                directory: self.capability()?,
                limit,
            })
            .await?
        {
            super::FilesystemResult::Inventory { directory, result } => {
                drop(directory);
                result
            }
            _ => Err(TransactionError::InvalidState(
                "resource inventory completion type",
            )),
        }
    }

    async fn unlink(
        &self,
        effects: &super::Effects,
        name: &str,
        expected: &LinkObservation,
        limit: usize,
    ) -> Result<(), TransactionError> {
        match effects
            .filesystem(super::FilesystemJob::RemoveResource {
                directory: self.capability()?,
                name: name.into(),
                expected: expected.clone(),
                limit,
            })
            .await?
        {
            super::FilesystemResult::ResourceRemoved { directory, result } => {
                drop(directory);
                result
            }
            _ => Err(TransactionError::InvalidState(
                "resource removal completion type",
            )),
        }
    }

    pub(super) fn inventory(&self, limit: usize) -> Result<Inventory, TransactionError> {
        let current = fs::symlink_metadata(&self.path).map_err(|_| TransactionError::UnsafePath)?;
        if current.dev() != self.identity.device
            || current.ino() != self.identity.inode
            || current.uid() != self.identity.uid
            || !current.is_dir()
        {
            return Err(TransactionError::UnsafePath);
        }
        let mut result = Inventory::new();
        for entry in fs::read_dir(&self.path).map_err(|_| TransactionError::Io)? {
            if result.len() >= limit {
                return Err(TransactionError::InvalidState(
                    "unexpected resource inventory growth",
                ));
            }
            let entry = entry.map_err(|_| TransactionError::Io)?;
            let name = entry
                .file_name()
                .into_string()
                .map_err(|_| TransactionError::UnsafePath)?;
            let c_name = CString::new(name.as_bytes()).map_err(|_| TransactionError::UnsafePath)?;
            let mut metadata = std::mem::MaybeUninit::<libc::stat>::uninit();
            if unsafe {
                libc::fstatat(
                    self.file.as_raw_fd(),
                    c_name.as_ptr(),
                    metadata.as_mut_ptr(),
                    libc::AT_SYMLINK_NOFOLLOW,
                )
            } != 0
            {
                return Err(TransactionError::Io);
            }
            let metadata = unsafe { metadata.assume_init() };
            if metadata.st_mode & libc::S_IFMT != libc::S_IFLNK
                || metadata.st_uid != self.identity.uid
            {
                return Err(TransactionError::UnsafePath);
            }
            let mut target = vec![0u8; 4096];
            let length = unsafe {
                libc::readlinkat(
                    self.file.as_raw_fd(),
                    c_name.as_ptr(),
                    target.as_mut_ptr().cast(),
                    target.len(),
                )
            };
            if length < 0 || length as usize == target.len() {
                return Err(TransactionError::UnsafePath);
            }
            target.truncate(length as usize);
            let target = String::from_utf8(target).map_err(|_| TransactionError::UnsafePath)?;
            result.insert(
                name,
                LinkObservation {
                    device: metadata.st_dev as u64,
                    inode: metadata.st_ino as u64,
                    uid: metadata.st_uid,
                    target,
                },
            );
        }
        let current = fs::symlink_metadata(&self.path).map_err(|_| TransactionError::UnsafePath)?;
        if current.dev() != self.identity.device || current.ino() != self.identity.inode {
            return Err(TransactionError::UnsafePath);
        }
        Ok(result)
    }

    pub(super) fn remove(
        &self,
        name: &str,
        expected: &LinkObservation,
        inventory_limit: usize,
    ) -> Result<(), TransactionError> {
        if self.inventory(inventory_limit)?.get(name) != Some(expected) {
            return Err(TransactionError::InvalidState(
                "resource changed before removal",
            ));
        }
        let name = CString::new(name).map_err(|_| TransactionError::UnsafePath)?;
        if unsafe { libc::unlinkat(self.file.as_raw_fd(), name.as_ptr(), 0) } != 0 {
            return Err(TransactionError::Io);
        }
        self.file.sync_all().map_err(|_| TransactionError::Io)
    }
}

pub(super) fn pending_inventory_limit(ledger: &super::resources::ResourceLedger) -> usize {
    let additions = match &ledger.pending {
        Some(ResourceEffect::Acquire { .. }) => {
            if ledger.links.contains_key("prepared") {
                5
            } else {
                6
            }
        }
        Some(ResourceEffect::Register { parent, .. })
            if parent == super::resources::HELPER_ROOT =>
        {
            super::resources::HELPER_REGISTRATION_ADDITIONS
                - usize::from(ledger.links.contains_key(parent))
        }
        Some(ResourceEffect::Register { parent, .. }) => {
            if ledger.links.contains_key(parent) {
                1
            } else {
                2
            }
        }
        Some(ResourceEffect::Remove { .. }) | None => 0,
    };
    ledger.links.len() + additions + 1
}

impl InstallationOwner {
    async fn resource_directory(
        &self,
        record: &mut InstallationRecord,
    ) -> Result<ResourceDirectory, TransactionError> {
        let super::FilesystemResult::ResourceDirectory(directory) = self
            .effects
            .filesystem(super::FilesystemJob::OpenResourceDirectory(
                self.layout.state_directory.join("resources"),
            ))
            .await?
        else {
            return Err(TransactionError::InvalidState(
                "resource directory completion type",
            ));
        };
        if let Some(expected) = &record.resources.parent {
            if expected != &directory.identity {
                return Err(TransactionError::InvalidState(
                    "resource directory identity changed",
                ));
            }
        } else {
            if !directory.observe(&self.effects, 1).await?.is_empty() {
                return Err(TransactionError::InvalidState(
                    "unrecorded resource directory is not empty",
                ));
            }
            record.resources.parent = Some(directory.identity.clone());
            self.write_record(record, false).await?;
        }
        Ok(directory)
    }

    async fn reconcile_resources(
        &self,
        record: &mut InstallationRecord,
        directory: &ResourceDirectory,
    ) -> Result<(), TransactionError> {
        let inventory = directory
            .observe(&self.effects, pending_inventory_limit(&record.resources))
            .await?;
        record
            .resources
            .reconcile(inventory, directory.identity.uid)
            .map_err(|_| {
                TransactionError::InvalidState("resource effect does not explain observed links")
            })?;
        self.write_record(record, false).await
    }

    async fn prune_resources(
        &self,
        record: &mut InstallationRecord,
        directory: &ResourceDirectory,
        roots: &BTreeSet<String>,
    ) -> Result<(), TransactionError> {
        self.reconcile_resources(record, directory).await?;
        for (name, observed) in record
            .resources
            .obsolete(roots)
            .map_err(|_| TransactionError::InvalidState("profile cleanup demand"))?
        {
            let current = directory
                .observe(&self.effects, record.resources.links.len() + 1)
                .await?;
            record
                .resources
                .begin(
                    ResourceEffect::Remove {
                        name: name.clone(),
                        observed: observed.clone(),
                    },
                    &current,
                )
                .map_err(|_| TransactionError::InvalidState("cleanup precondition"))?;
            self.write_record(record, false).await?;
            #[cfg(test)]
            super::pause_after("before_resource_unlink");
            let settlement = self.effects.settlement(super::SettlementKind::Resource)?;
            let result = directory
                .unlink(
                    &self.effects,
                    &name,
                    &observed,
                    record.resources.links.len() + 1,
                )
                .await;
            #[cfg(test)]
            super::pause_after("after_resource_unlink");
            if matches!(&result, Err(error) if *error != TransactionError::StopRequested) {
                return result;
            }
            self.with_effects(settlement.clone())
                .reconcile_resources(record, directory)
                .await?;
            settlement.finish_settlement()?;
            result?;
            self.effects.checkpoint()?;
        }
        Ok(())
    }

    async fn resource_operation(
        &self,
        lease: &super::Effects,
        record: &mut InstallationRecord,
        directory: &ResourceDirectory,
        effect: ResourceEffect,
        operation: process::WorkerRequest,
    ) -> Result<(), TransactionError> {
        let observed = directory
            .observe(&self.effects, record.resources.links.len() + 1)
            .await?;
        record
            .resources
            .begin(effect, &observed)
            .map_err(|_| TransactionError::InvalidState("resource operation precondition"))?;
        self.write_record(record, false).await?;
        let settlement = self.effects.settlement(super::SettlementKind::Resource)?;
        #[cfg(test)]
        {
            self.effects.hit("before_resource_command");
            super::pause_after("before_resource_command");
        }
        let result = self.observe(lease, operation).await;
        #[cfg(test)]
        super::pause_after("after_resource_command");
        if result
            .as_ref()
            .is_err_and(|error| *error == TransactionError::CleanupUnknown)
        {
            return Err(TransactionError::CleanupUnknown);
        }
        let owner = self.with_effects(settlement.clone());
        owner.reconcile_resources(record, directory).await?;
        settlement.finish_settlement()?;
        let result = result?;
        if result.exit_code != Some(0) {
            return Err(TransactionError::Verification("retention operation failed"));
        }
        self.effects.checkpoint()
    }

    fn helper_admission(record: &InstallationRecord) -> Result<(), TransactionError> {
        if record.activation.is_some()
            || record.uninstall.is_some()
            || record
                .shell
                .as_ref()
                .is_some_and(super::shell::Ledger::pending)
            || record
                .resources
                .pending
                .as_ref()
                .is_some_and(|effect| match effect {
                    ResourceEffect::Register { parent, .. } => {
                        parent != super::resources::HELPER_ROOT
                    }
                    ResourceEffect::Remove { name, .. } => !super::resources::is_helper_link(name),
                    ResourceEffect::Acquire { .. } => true,
                })
        {
            return Err(TransactionError::InvalidState(
                "helper retention requires quiescent installation",
            ));
        }
        Ok(())
    }

    fn helper_observation(
        record: &InstallationRecord,
        directory: &ResourceDirectory,
        target: &OutputPath,
    ) -> Result<HelperRetention, TransactionError> {
        if record.resources.pending.is_some()
            || record.resources.parent.as_ref() != Some(&directory.identity)
            || record
                .resources
                .links
                .get(super::resources::HELPER_ROOT)
                .is_none_or(|link| {
                    link.target != target.as_str() || link.uid != directory.identity.uid
                })
        {
            return Err(TransactionError::Verification(
                "helper root does not retain exact output",
            ));
        }
        Ok(HelperRetention {
            target: target.clone(),
        })
    }

    pub(super) async fn retain_nix_policy_helper(
        &self,
        lease: &super::Effects,
        target: &OutputPath,
    ) -> Result<HelperRetention, TransactionError> {
        if target.as_str().ends_with(".drv") {
            return Err(TransactionError::Verification(
                "helper must be an imported output",
            ));
        }
        let mut record = self.read_record().await?;
        Self::helper_admission(&record)?;
        let directory = self.resource_directory(&mut record).await?;
        self.reconcile_resources(&mut record, &directory).await?;
        if record
            .resources
            .links
            .get(super::resources::HELPER_ROOT)
            .is_some_and(|link| link.target != target.as_str())
        {
            return Err(TransactionError::Verification(
                "helper root retains another output",
            ));
        }
        let mut roots: BTreeSet<String> = record
            .resources
            .links
            .keys()
            .filter(|name| !super::resources::is_helper_link(name))
            .cloned()
            .collect();
        roots.insert(super::resources::HELPER_ROOT.into());
        self.prune_resources(&mut record, &directory, &roots)
            .await?;
        self.resource_operation(
            lease,
            &mut record,
            &directory,
            ResourceEffect::Register {
                pair: target.as_str().into(),
                parent: super::resources::HELPER_ROOT.into(),
            },
            process::WorkerRequest::RegisterExact {
                pair: target.as_str().into(),
                root: process::RootName::NixPolicyHelper,
            },
        )
        .await?;
        self.prune_resources(&mut record, &directory, &roots)
            .await?;
        Self::helper_observation(&record, &directory, target)
    }

    pub(super) async fn release_nix_policy_helper(
        &self,
        _lease: &super::Effects,
    ) -> Result<(), TransactionError> {
        let mut record = self.read_record().await?;
        Self::helper_admission(&record)?;
        let directory = self.resource_directory(&mut record).await?;
        self.reconcile_resources(&mut record, &directory).await?;
        let roots = record
            .resources
            .links
            .keys()
            .filter(|name| !super::resources::is_helper_link(name))
            .cloned()
            .collect();
        self.prune_resources(&mut record, &directory, &roots).await
    }

    pub(super) async fn finish_resources(
        &self,
        _lease: &super::Effects,
    ) -> Result<(), TransactionError> {
        let mut record = self.read_record().await?;
        let directory = self.resource_directory(&mut record).await?;
        let mut roots = BTreeSet::new();
        if let Some(roles) = &record.committed {
            for identity in std::iter::once(&roles.active).chain(roles.previous.iter()) {
                let selected =
                    identity
                        .selection
                        .as_ref()
                        .ok_or(TransactionError::InvalidState(
                            "retained role has no candidate identity",
                        ))?;
                roots.insert(format!("candidate-{}", selected.candidate_id));
            }
        }
        self.prune_resources(&mut record, &directory, &roots).await
    }

    pub(super) async fn reconcile_pending_resources(
        &self,
        _lease: &super::Effects,
    ) -> Result<(), TransactionError> {
        let mut record = self.read_record().await?;
        let directory = self.resource_directory(&mut record).await?;
        self.reconcile_resources(&mut record, &directory).await
    }

    pub(super) async fn retain_candidate(
        &self,
        lease: &super::Effects,
        selected: &super::release::PinnedSelection,
        pair: &OutputPath,
        offline: bool,
    ) -> Result<RetentionObservation, TransactionError> {
        let mut record = self.read_record().await?;
        if record
            .resources
            .links
            .keys()
            .any(|name| super::resources::is_helper_link(name))
            || matches!(&record.resources.pending, Some(ResourceEffect::Register { parent, .. }) if parent == super::resources::HELPER_ROOT)
        {
            return Err(TransactionError::InvalidState(
                "helper retention must be released before pair acquisition",
            ));
        }
        if record.pinned.as_ref() != Some(selected) {
            return Err(TransactionError::InvalidState(
                "retention selection is not pinned",
            ));
        }
        let directory = self.resource_directory(&mut record).await?;
        let root = format!("candidate-{}", selected.candidate_id);
        let mut roots = BTreeSet::from([root.clone()]);
        if let Some(roles) = &record.committed {
            for identity in std::iter::once(&roles.active).chain(roles.previous.iter()) {
                let selected =
                    identity
                        .selection
                        .as_ref()
                        .ok_or(TransactionError::InvalidState(
                            "retained role has no candidate identity",
                        ))?;
                roots.insert(format!("candidate-{}", selected.candidate_id));
            }
        }
        self.prune_resources(&mut record, &directory, &roots)
            .await?;
        let profile_pair = record
            .resources
            .links
            .get("prepared")
            .and_then(|profile| record.resources.links.get(&profile.target))
            .map(|generation| generation.target.as_str());
        if profile_pair != Some(pair.as_str())
            || record
                .resources
                .links
                .get(&root)
                .is_none_or(|link| link.target != pair.as_str())
        {
            self.resource_operation(
                lease,
                &mut record,
                &directory,
                ResourceEffect::Acquire {
                    pair: pair.as_str().into(),
                    root: root.clone(),
                },
                process::WorkerRequest::AcquireRetained {
                    pair: pair.as_str().into(),
                    candidate: selected.candidate_id.clone(),
                    offline,
                },
            )
            .await?;
        }
        self.prune_resources(&mut record, &directory, &roots)
            .await?;
        self.resource_operation(
            lease,
            &mut record,
            &directory,
            ResourceEffect::Register {
                pair: pair.as_str().into(),
                parent: root.clone(),
            },
            process::WorkerRequest::RegisterExact {
                pair: pair.as_str().into(),
                root: process::RootName::Candidate(selected.candidate_id.clone()),
            },
        )
        .await?;
        self.prune_resources(&mut record, &directory, &roots)
            .await?;
        let generation = record
            .resources
            .links
            .get("prepared")
            .ok_or(TransactionError::InvalidState(
                "profile missing after acquisition",
            ))?
            .target
            .clone();
        if record
            .resources
            .links
            .get(&generation)
            .is_none_or(|link| link.target != pair.as_str())
        {
            return Err(TransactionError::InvalidState(
                "profile selected another pair",
            ));
        }
        self.resource_operation(
            lease,
            &mut record,
            &directory,
            ResourceEffect::Register {
                pair: pair.as_str().into(),
                parent: generation.clone(),
            },
            process::WorkerRequest::RegisterExact {
                pair: pair.as_str().into(),
                root: process::RootName::Generation(generation),
            },
        )
        .await?;
        self.prune_resources(&mut record, &directory, &roots)
            .await?;
        Ok(RetentionObservation {
            pair: pair.clone(),
            root: directory.path.join(root),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::Layout;
    use super::super::RootLease;
    use super::*;
    use std::io;
    use std::os::unix::fs::symlink;
    use std::path::Path;
    use std::time::{Duration, Instant};

    const PAIR: &str = "/nix/store/00000000000000000000000000000000-resource-fixture";
    const ROOT: &str = "candidate-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

    fn wait_until(mut condition: impl FnMut() -> bool) -> bool {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut delay = Duration::from_millis(1);
        while !condition() {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            std::thread::sleep(delay.min(remaining));
            delay *= 2;
        }
        true
    }

    fn absent(pid: i32) -> bool {
        (unsafe { libc::kill(pid, 0) == -1 })
            && io::Error::last_os_error().raw_os_error() == Some(libc::ESRCH)
    }

    struct OwnedClient {
        pid: i32,
        session: i32,
        worker: i32,
        lock: PathBuf,
    }

    impl OwnedClient {
        fn cleanup(&self) -> bool {
            if !absent(self.pid) {
                if unsafe { libc::getpgid(self.pid) } != self.pid
                    || unsafe { libc::getsid(self.pid) } != self.session
                {
                    eprintln!("resource fixture identity changed before cleanup");
                    return false;
                }
                unsafe { libc::kill(-self.pid, libc::SIGKILL) };
            }
            let gone = wait_until(|| absent(self.pid) && absent(-self.pid));
            let worker_gone = absent(self.worker);
            let lease_free = RootLease::acquire(&self.lock).is_ok();
            eprintln!(
                "resource cleanup client={} pgid={} sid={} worker={} client_and_group_absent={} worker_absent={} lease_free={}",
                self.pid, self.pid, self.session, self.worker, gone, worker_gone, lease_free
            );
            gone && worker_gone && lease_free
        }
    }

    impl Drop for OwnedClient {
        fn drop(&mut self) {
            if !self.cleanup() {
                eprintln!("resource fixture cleanup did not establish absence and lease release");
            }
        }
    }

    fn setup() -> (tempfile::TempDir, InstallationOwner, RootLease) {
        let temporary = tempfile::tempdir().unwrap();
        let base = temporary.path().canonicalize().unwrap();
        let owner = InstallationOwner::new(Layout::new(base.join("state"), base.join("bin")));
        owner.effects.accept_fixture_review();
        snapshot::private_directory(&owner.layout.state_directory).unwrap();
        snapshot::private_directory(&owner.layout.command_directory).unwrap();
        owner.layout.validate().unwrap();
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        (temporary, owner, lease)
    }

    fn register(
        owner: &InstallationOwner,
        lease: &RootLease,
        fixture: &str,
    ) -> Result<(), TransactionError> {
        super::super::coordinator::test_with(&owner.effects, lease, async {
            let mut record = owner.read_record().await?;
            let directory = owner.resource_directory(&mut record).await?;
            owner
                .resource_operation(
                    &owner.effects,
                    &mut record,
                    &directory,
                    ResourceEffect::Register {
                        pair: PAIR.into(),
                        parent: ROOT.into(),
                    },
                    process::WorkerRequest::Fixture {
                        name: fixture.into(),
                    },
                )
                .await
        })
    }

    #[test]
    #[ignore]
    fn fixture_resource_helper() {
        symlink(
            PAIR,
            Path::new("resources").join(super::super::resources::HELPER_ROOT),
        )
        .unwrap();
        std::process::exit(0);
    }

    #[test]
    fn helper_worker_success_and_cancellation_settle_before_observation() {
        for cancel in [false, true] {
            let (_temporary, owner, lease) = setup();
            if cancel {
                owner.effects.cancel_at("before_resource_command");
            }
            let result = super::super::coordinator::test_with(&owner.effects, &lease, async {
                let mut record = owner.read_record().await?;
                let directory = owner.resource_directory(&mut record).await?;
                owner
                    .resource_operation(
                        &owner.effects,
                        &mut record,
                        &directory,
                        ResourceEffect::Register {
                            pair: PAIR.into(),
                            parent: super::super::resources::HELPER_ROOT.into(),
                        },
                        process::WorkerRequest::Fixture {
                            name: "resource-helper".into(),
                        },
                    )
                    .await?;
                InstallationOwner::helper_observation(
                    &record,
                    &directory,
                    &OutputPath::parse(PAIR).unwrap(),
                )
                .map(|observation| observation.target().clone())
            });
            if cancel {
                assert_eq!(result, Err(TransactionError::StopRequested));
            } else {
                assert_eq!(result.unwrap().as_str(), PAIR);
            }
            let record: InstallationRecord =
                serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
            assert!(record.resources.pending.is_none());
            assert_eq!(
                record
                    .resources
                    .links
                    .contains_key(super::super::resources::HELPER_ROOT),
                !cancel
            );
        }
    }

    #[test]
    fn helper_observation_requires_exact_owned_target_and_never_returns_alias() {
        let (_temporary, owner, lease) = setup();
        super::super::coordinator::test_with(&owner.effects, &lease, async {
            let mut record = owner.read_record().await?;
            let directory = owner.resource_directory(&mut record).await?;
            let target = OutputPath::parse(PAIR).unwrap();
            let alias = directory.path.join(super::super::resources::HELPER_ROOT);
            symlink(PAIR, &alias).unwrap();
            record.resources.links = directory.inventory(2)?;
            let retained = InstallationOwner::helper_observation(&record, &directory, &target)?;
            assert_eq!(retained.target(), &target);
            assert_ne!(std::path::Path::new(retained.target().as_str()), alias);
            record
                .resources
                .links
                .get_mut(super::super::resources::HELPER_ROOT)
                .unwrap()
                .target = "wrong".into();
            assert!(InstallationOwner::helper_observation(&record, &directory, &target).is_err());
            record
                .resources
                .links
                .get_mut(super::super::resources::HELPER_ROOT)
                .unwrap()
                .target = PAIR.into();
            record
                .resources
                .links
                .get_mut(super::super::resources::HELPER_ROOT)
                .unwrap()
                .uid += 1;
            assert!(InstallationOwner::helper_observation(&record, &directory, &target).is_err());
            Ok::<(), TransactionError>(())
        })
        .unwrap();
    }

    #[test]
    fn helper_interrupted_registration_reconciles_and_releases_only_helper() {
        let (_temporary, owner, lease) = setup();
        super::super::coordinator::test_with(&owner.effects, &lease, async {
            let mut record = owner.read_record().await?;
            let directory = owner.resource_directory(&mut record).await?;
            symlink(PAIR, directory.path.join(ROOT)).unwrap();
            record.resources.links = directory.inventory(2)?;
            let baseline = record.resources.links.clone();
            record
                .resources
                .begin(
                    ResourceEffect::Register {
                        pair: PAIR.into(),
                        parent: super::super::resources::HELPER_ROOT.into(),
                    },
                    &baseline,
                )
                .unwrap();
            owner.write_record(&record, false).await?;
            symlink(
                PAIR,
                directory.path.join(super::super::resources::HELPER_ROOT),
            )
            .unwrap();
            owner.release_nix_policy_helper(&owner.effects).await?;
            let recovered = owner.read_record().await?;
            assert!(recovered.resources.pending.is_none());
            assert_eq!(recovered.resources.links, baseline);
            assert_eq!(directory.inventory(2)?, baseline);
            Ok::<(), TransactionError>(())
        })
        .unwrap();
    }

    #[test]
    fn helper_inventory_allowance_is_final_plus_one_temporary() {
        let mut ledger = super::super::resources::ResourceLedger::default();
        ledger.pending = Some(ResourceEffect::Register {
            pair: PAIR.into(),
            parent: super::super::resources::HELPER_ROOT.into(),
        });
        assert_eq!(pending_inventory_limit(&ledger), 3);
        ledger.links.insert(
            super::super::resources::HELPER_ROOT.into(),
            LinkObservation {
                device: 1,
                inode: 1,
                uid: 1001,
                target: PAIR.into(),
            },
        );
        assert_eq!(pending_inventory_limit(&ledger), 3);
    }

    #[test]
    #[ignore]
    fn fixture_resource_gated() {
        let identity = unsafe {
            [
                libc::getpid(),
                libc::getpgrp(),
                libc::getsid(0),
                libc::getppid(),
            ]
        };
        fs::write(
            "resource-client-next",
            serde_json::to_vec(&identity).unwrap(),
        )
        .unwrap();
        fs::rename("resource-client-next", "resource-client").unwrap();
        if !wait_until(|| Path::new("resource-release").exists()) {
            std::process::exit(91);
        }
        symlink(PAIR, Path::new("resources").join(ROOT)).unwrap();
        std::process::exit(0);
    }

    #[test]
    #[ignore]
    fn fixture_resource_nonzero() {
        symlink(PAIR, Path::new("resources").join(ROOT)).unwrap();
        std::process::exit(19);
    }

    #[test]
    fn worker_loss_preserves_pending_resource_until_independent_recovery() {
        let (_temporary, owner, lease) = setup();
        let directory = owner.layout.state_directory.clone();
        std::thread::scope(|scope| {
            let layout = owner.layout.clone();
            let operation = scope.spawn(move || {
                let worker_owner = InstallationOwner::new(layout);
                worker_owner.effects.accept_fixture_review();
                let result = register(&worker_owner, &lease, "resource-gated");
                (result, lease)
            });
            assert!(wait_until(|| directory.join("resource-client").exists()));
            let [pid, group, session, worker]: [i32; 4] =
                serde_json::from_slice(&fs::read(directory.join("resource-client")).unwrap())
                    .unwrap();
            let client = OwnedClient {
                pid,
                session,
                worker,
                lock: owner.layout.lock(),
            };
            assert!(pid > 1 && worker > 1 && pid != worker);
            assert_eq!(group, pid);
            assert_eq!(unsafe { libc::getpgid(pid) }, group);
            assert_eq!(unsafe { libc::getsid(pid) }, session);
            assert_eq!(unsafe { libc::getsid(worker) }, session);
            eprintln!(
                "resource admitted client={pid} pgid={group} sid={session} worker={worker} worker_pgid={}",
                unsafe { libc::getpgid(worker) }
            );
            assert_eq!(unsafe { libc::kill(worker, libc::SIGKILL) }, 0);
            let (result, lease) = operation.join().unwrap();
            assert_eq!(result, Err(TransactionError::CleanupUnknown));
            assert!(
                absent(worker),
                "worker must be reaped before the resource call returns"
            );
            let pending = serde_json::from_slice::<InstallationRecord>(
                &fs::read(owner.layout.record()).unwrap(),
            )
            .unwrap()
            .resources
            .pending;
            drop(lease);
            assert_eq!(
                pending,
                Some(ResourceEffect::Register {
                    pair: PAIR.into(),
                    parent: ROOT.into()
                }),
                "unknown containment must preserve the durable pending resource effect"
            );
            assert!(matches!(
                RootLease::acquire(&owner.layout.lock()),
                Err(TransactionError::Busy)
            ));
            assert!(!absent(pid));
            assert!(fs::symlink_metadata(directory.join("resources").join(ROOT)).is_err());
            fs::write(directory.join("resource-release"), b"publish").unwrap();
            assert!(
                wait_until(|| absent(pid) && absent(-group)),
                "released client and group must disappear"
            );
            let independent = RootLease::acquire(&owner.layout.lock()).unwrap();
            super::super::coordinator::test_with(
                &owner.effects,
                &independent,
                owner.reconcile_pending_resources(&owner.effects),
            )
            .unwrap();
            let recovered = serde_json::from_slice::<InstallationRecord>(
                &fs::read(owner.layout.record()).unwrap(),
            )
            .unwrap();
            assert!(recovered.resources.pending.is_none());
            assert_eq!(recovered.resources.links.len(), 1);
            assert_eq!(recovered.resources.links[ROOT].target, PAIR);
            assert_eq!(
                fs::read_link(directory.join("resources").join(ROOT)).unwrap(),
                Path::new(PAIR)
            );
            drop(independent);
            assert!(client.cleanup());
        });
    }

    #[test]
    fn nonzero_resource_client_exit_still_reconciles_published_link() {
        let (_temporary, owner, lease) = setup();
        assert_eq!(
            register(&owner, &lease, "resource-nonzero"),
            Err(TransactionError::Verification("retention operation failed"))
        );
        let record =
            serde_json::from_slice::<InstallationRecord>(&fs::read(owner.layout.record()).unwrap())
                .unwrap();
        assert!(record.resources.pending.is_none());
        assert_eq!(record.resources.links.len(), 1);
        assert_eq!(record.resources.links[ROOT].target, PAIR);
        drop(lease);
        assert!(RootLease::acquire(&owner.layout.lock()).is_ok());
    }
    #[test]
    fn cancellation_after_resource_journal_before_submission_reconciles_absence() {
        let (_temporary, owner, lease) = setup();
        owner.effects.cancel_at("before_resource_command");
        assert_eq!(
            register(&owner, &lease, "resource-nonzero"),
            Err(TransactionError::StopRequested)
        );
        let record: InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(record.resources.pending.is_none());
        assert!(record.resources.links.is_empty());
        assert!(
            !owner
                .layout
                .state_directory
                .join("fixture-operations.jsonl")
                .exists()
        );
        assert!(
            !owner
                .layout
                .state_directory
                .join("resources")
                .join(ROOT)
                .exists()
        );
    }
    #[test]
    fn failed_resource_removal_preserves_journal_until_a_new_recovery() {
        let (_temporary, owner, lease) = setup();
        assert_eq!(
            register(&owner, &lease, "resource-nonzero"),
            Err(TransactionError::Verification("retention operation failed"))
        );
        owner.effects.cancel_at("resource_remove_failure");
        let result = super::super::coordinator::test_with(&owner.effects, &lease, async {
            let mut record = owner.read_record().await?;
            let directory = owner.resource_directory(&mut record).await?;
            owner
                .prune_resources(&mut record, &directory, &BTreeSet::new())
                .await
        });
        assert_eq!(result, Err(TransactionError::Io));
        let record: InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(matches!(
            record.resources.pending,
            Some(ResourceEffect::Remove { .. })
        ));
        assert!(
            !owner
                .layout
                .state_directory
                .join("resources")
                .join(ROOT)
                .exists()
        );
        let recovery = InstallationOwner::new(owner.layout.clone());
        recovery.effects.accept_fixture_review();
        super::super::coordinator::test_with(
            &recovery.effects,
            &lease,
            recovery.reconcile_pending_resources(&recovery.effects),
        )
        .unwrap();
        let record: InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(record.resources.pending.is_none());
        assert!(record.resources.links.is_empty());
    }
}
