use super::{
    FilesystemJob, FilesystemResult, InstallationOwner, InstallationRecord, Layout,
    TransactionError,
};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, OpenOptions};
use std::io::Read;
use std::os::fd::AsRawFd;
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct UninstallIntent {
    files: Vec<Removal>,
    shell: Vec<super::shell::RemovalPlan>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Removal {
    path: PathBuf,
    parent: (u64, u64),
    held: PathBuf,
    held_parent: (u64, u64),
    identity: (u64, u64, u32, u32),
    content: Content,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
enum Content {
    Link(PathBuf),
    File { size: u64, sha: String },
    Directory,
}

impl UninstallIntent {
    pub(super) fn validate(
        &self,
        layout: &Layout,
        record: &InstallationRecord,
    ) -> Result<(), TransactionError> {
        if record.activation.is_some()
            || record.resources.pending.is_some()
            || record
                .shell
                .as_ref()
                .is_some_and(super::shell::Ledger::pending)
            || self.files.len() > 256
            || self.shell.len() > 4
        {
            return Err(TransactionError::InvalidState(
                "uninstall must be exclusive",
            ));
        }
        let allowed = allowed_files(layout, record)?;
        let mut seen = std::collections::BTreeSet::new();
        for file in &self.files {
            if allowed.get(&file.path) != Some(&file.content)
                || !seen.insert(&file.path)
                || (file.held != held_path(layout, &file.path, file.identity)
                    && (!valid_local_slot(layout, file) || file.held_parent.0 != file.parent.0))
                || file.identity.2 != unsafe { libc::geteuid() }
            {
                return Err(TransactionError::InvalidState(
                    "uninstall ownership differs from receipt",
                ));
            }
        }
        for (index, plan) in self.shell.iter().enumerate() {
            let ledger = record.shell.as_ref().ok_or(TransactionError::UnsafePath)?;
            ledger.validate_removal(plan)?;
            if ledger.receipts().get(index).map(|receipt| receipt.name()) != Some(plan.name()) {
                return Err(TransactionError::InvalidState(
                    "shell removal order differs from ledger",
                ));
            }
        }
        Ok(())
    }
}

fn allowed_files(
    layout: &Layout,
    record: &InstallationRecord,
) -> Result<std::collections::BTreeMap<PathBuf, Content>, TransactionError> {
    let mut files = std::collections::BTreeMap::new();
    if let Some(receipt) = &record.committed {
        files.insert(
            layout.active_link(),
            Content::Link(receipt.active.output.clone()),
        );
        for name in ["neo", "neoctl"] {
            files.insert(
                layout.command_directory.join(name),
                Content::Link(layout.expected_command_target(name)),
            );
        }
    }
    for (name, link) in &record.resources.links {
        if name.is_empty() || name.contains('/') || matches!(name.as_str(), "." | "..") {
            return Err(TransactionError::UnsafePath);
        }
        files.insert(
            layout.state_directory.join("resources").join(name),
            Content::Link(link.target.clone().into()),
        );
    }
    for (id, descriptors) in &record.snapshot_files {
        let directory = layout.state_directory.join("snapshots").join(id);
        for file in descriptors {
            files.insert(
                directory.join(&file.name),
                Content::File {
                    size: file.size as u64,
                    sha: file.sha.clone(),
                },
            );
        }
        files.insert(directory, Content::Directory);
    }
    Ok(files)
}

fn inspect(path: &Path, content: Content) -> Result<Option<Removal>, TransactionError> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(value) => value,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(_) => return Err(TransactionError::Io),
    };
    let parent = path.parent().ok_or(TransactionError::UnsafePath)?;
    super::validate_directory(parent, false)?;
    let parent = fs::symlink_metadata(parent).map_err(|_| TransactionError::Io)?;
    if metadata.uid() != unsafe { libc::geteuid() } {
        return Err(TransactionError::UnsafePath);
    }
    match &content {
        Content::Link(target) => {
            if !metadata.file_type().is_symlink()
                || fs::read_link(path).map_err(|_| TransactionError::UnsafePath)? != *target
            {
                return Err(TransactionError::ForeignActiveTarget);
            }
        }
        Content::File { size, sha } => {
            let mut file =
                super::open_owned_file(path, true, false)?.ok_or(TransactionError::UnsafePath)?;
            if metadata.len() != *size {
                return Err(TransactionError::UnsafePath);
            }
            let mut bytes = Vec::new();
            (&mut file)
                .take(size + 1)
                .read_to_end(&mut bytes)
                .map_err(|_| TransactionError::Io)?;
            if bytes.len() as u64 != *size || format!("{:x}", Sha256::digest(&bytes)) != *sha {
                return Err(TransactionError::UnsafePath);
            }
            let opened = file.metadata().map_err(|_| TransactionError::Io)?;
            if opened.dev() != metadata.dev() || opened.ino() != metadata.ino() {
                return Err(TransactionError::UnsafePath);
            }
        }
        Content::Directory => super::validate_directory(path, true)?,
    }
    Ok(Some(Removal {
        path: path.to_owned(),
        parent: (parent.dev(), parent.ino()),
        identity: (
            metadata.dev(),
            metadata.ino(),
            metadata.uid(),
            metadata.mode(),
        ),
        held: PathBuf::new(),
        held_parent: (0, 0),
        content,
    }))
}

pub(super) fn prepare(
    layout: &Layout,
    record: &InstallationRecord,
) -> Result<UninstallIntent, TransactionError> {
    if let Some(intent) = &record.uninstall {
        intent.validate(layout, record)?;
        for entry in &intent.files {
            if inspect(&entry.path, entry.content.clone())?
                .is_some_and(|actual| !same_source(&actual, entry))
            {
                return Err(TransactionError::UnsafePath);
            }
        }
        return Ok(intent.clone());
    }
    if record.activation.is_some()
        || record.resources.pending.is_some()
        || record
            .shell
            .as_ref()
            .is_some_and(super::shell::Ledger::pending)
    {
        return Err(TransactionError::InvalidState(
            "finish pending installation recovery before uninstall",
        ));
    }
    if super::read_owned_link(&layout.active_next_link())?.is_some() {
        return Err(TransactionError::InvalidState(
            "unrecorded prepared active link",
        ));
    }
    if record.committed.is_none() {
        for path in [
            layout.active_link(),
            layout.owned_links().neo,
            layout.owned_links().neoctl,
        ] {
            if fs::symlink_metadata(path).is_ok() {
                return Err(TransactionError::CommandLinkConflict);
            }
        }
    }
    if let Some(directory) = super::resource_io::ResourceDirectory::open_existing(
        layout.state_directory.join("resources"),
    )? {
        let inventory = directory.inventory(record.resources.links.len() + 1)?;
        if inventory != record.resources.links
            || record
                .resources
                .parent
                .as_ref()
                .is_some_and(|expected| expected != &directory.identity)
        {
            return Err(TransactionError::InvalidState("foreign resource inventory"));
        }
    } else if !record.resources.links.is_empty() {
        return Err(TransactionError::UnsafePath);
    }
    let mut files = Vec::new();
    for (path, content) in allowed_files(layout, record)? {
        if let Some(mut entry) = inspect(&path, content)? {
            let state =
                fs::symlink_metadata(&layout.state_directory).map_err(|_| TransactionError::Io)?;
            entry.held = held_path(layout, &entry.path, entry.identity);
            entry.held_parent = (state.dev(), state.ino());
            if fs::symlink_metadata(&entry.held).is_ok() {
                return Err(TransactionError::InvalidState(
                    "unrecorded uninstall quarantine",
                ));
            }
            files.push(entry);
        }
    }
    files.sort_by_key(|entry| matches!(entry.content, Content::Directory));
    let intent = UninstallIntent {
        files,
        shell: Vec::new(),
    };
    intent.validate(layout, record)?;
    Ok(intent)
}

pub(super) fn confirm(intent: &UninstallIntent) -> Result<(), TransactionError> {
    for entry in &intent.files {
        for path in [&entry.path, &entry.held] {
            match fs::symlink_metadata(path) {
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                _ => {
                    return Err(TransactionError::InvalidState(
                        "owned path reappeared during uninstall",
                    ));
                }
            }
        }
    }
    Ok(())
}

fn held_path(layout: &Layout, path: &Path, identity: (u64, u64, u32, u32)) -> PathBuf {
    use std::os::unix::ffi::OsStrExt;
    let mut digest = Sha256::new();
    digest.update(path.as_os_str().as_bytes());
    digest.update(identity.0.to_le_bytes());
    digest.update(identity.1.to_le_bytes());
    layout
        .state_directory
        .join(format!("uninstall-held-{:x}", digest.finalize()))
}

fn local_quarantine_prefix(layout: &Layout, entry: &Removal) -> String {
    let legacy = held_path(layout, &entry.path, entry.identity);
    format!(
        ".neo-{}-",
        legacy
            .file_name()
            .expect("generated hold basename")
            .to_string_lossy()
    )
}

fn valid_local_slot(layout: &Layout, entry: &Removal) -> bool {
    if !private_quarantine(entry) {
        return false;
    }
    let name = entry
        .held
        .parent()
        .and_then(Path::file_name)
        .and_then(|name| name.to_str());
    let prefix = local_quarantine_prefix(layout, entry);
    let nonce = name
        .and_then(|name| name.strip_prefix(&prefix))
        .and_then(|name| name.split_once('-'));
    nonce.is_some_and(|(time, pid)| {
        time.len() == 32
            && !pid.is_empty()
            && pid.len() <= 8
            && time
                .bytes()
                .chain(pid.bytes())
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    })
}

pub(super) fn bind(
    layout: &Layout,
    mut intent: UninstallIntent,
) -> Result<UninstallIntent, TransactionError> {
    for entry in &mut intent.files {
        if entry.held != held_path(layout, &entry.path, entry.identity)
            || entry.parent.0 == entry.held_parent.0
        {
            continue;
        }
        match fs::symlink_metadata(&entry.held) {
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            _ => return Err(TransactionError::UnsafePath),
        }
        let parent_path = entry.path.parent().ok_or(TransactionError::UnsafePath)?;
        let parent = directory(parent_path, entry.parent)?;
        let held = parent_path
            .join(format!(
                "{}{}",
                local_quarantine_prefix(layout, entry),
                super::new_installation_id()
            ))
            .join("held");
        let quarantine = held.parent().ok_or(TransactionError::UnsafePath)?;
        let name = basename(quarantine)?;
        if unsafe { libc::mkdirat(parent.as_raw_fd(), name.as_ptr(), 0o700) } != 0 {
            return Err(TransactionError::Io);
        }
        super::validate_directory(quarantine, true)?;
        let metadata = fs::symlink_metadata(quarantine).map_err(|_| TransactionError::Io)?;
        let identity = (metadata.dev(), metadata.ino());
        let hold = directory(quarantine, identity)?;
        if identity.0 != entry.parent.0
            || fs::read_dir(quarantine)
                .map_err(|_| TransactionError::Io)?
                .next()
                .is_some()
        {
            return Err(TransactionError::UnsafePath);
        }
        directory(parent_path, entry.parent)?;
        hold.sync_all().map_err(|_| TransactionError::Io)?;
        parent.sync_all().map_err(|_| TransactionError::Io)?;
        entry.held = held;
        entry.held_parent = identity;
    }
    Ok(intent)
}

fn private_quarantine(entry: &Removal) -> bool {
    entry.held.parent().and_then(Path::parent) == entry.path.parent()
        && entry.held.file_name().is_some_and(|name| name == "held")
}

fn cleanup_quarantine(entry: &Removal, parent: &std::fs::File) -> Result<(), TransactionError> {
    if private_quarantine(entry) {
        let path = entry.held.parent().ok_or(TransactionError::UnsafePath)?;
        super::validate_directory(path, true)?;
        let hold = directory(path, entry.held_parent)?;
        hold.sync_all().map_err(|_| TransactionError::Io)?;
        let name = basename(path)?;
        if unsafe { libc::unlinkat(parent.as_raw_fd(), name.as_ptr(), libc::AT_REMOVEDIR) } != 0 {
            return Err(TransactionError::Io);
        }
        parent.sync_all().map_err(|_| TransactionError::Io)?;
    }
    Ok(())
}

fn same_source(actual: &Removal, expected: &Removal) -> bool {
    actual.path == expected.path
        && actual.identity == expected.identity
        && actual.parent == expected.parent
        && actual.content == expected.content
}

fn same_held(actual: &Removal, expected: &Removal) -> bool {
    actual.path == expected.held
        && actual.identity == expected.identity
        && actual.parent == expected.held_parent
        && actual.content == expected.content
}

fn directory(path: &Path, identity: (u64, u64)) -> Result<std::fs::File, TransactionError> {
    super::validate_directory(path, false)?;
    let file = OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_DIRECTORY | libc::O_CLOEXEC)
        .open(path)
        .map_err(|_| TransactionError::UnsafePath)?;
    let metadata = file.metadata().map_err(|_| TransactionError::Io)?;
    if (metadata.dev(), metadata.ino()) != identity {
        return Err(TransactionError::UnsafePath);
    }
    Ok(file)
}

fn basename(path: &Path) -> Result<std::ffi::CString, TransactionError> {
    use std::os::unix::ffi::OsStrExt;
    std::ffi::CString::new(
        path.file_name()
            .ok_or(TransactionError::UnsafePath)?
            .as_bytes(),
    )
    .map_err(|_| TransactionError::UnsafePath)
}

fn move_exclusive(
    from: &std::fs::File,
    name: &std::ffi::CStr,
    to: &std::fs::File,
    target: &std::ffi::CStr,
) -> Result<(), TransactionError> {
    #[cfg(target_os = "macos")]
    let status = unsafe {
        libc::renameatx_np(
            from.as_raw_fd(),
            name.as_ptr(),
            to.as_raw_fd(),
            target.as_ptr(),
            libc::RENAME_EXCL,
        )
    };
    #[cfg(target_os = "linux")]
    let status = unsafe {
        libc::renameat2(
            from.as_raw_fd(),
            name.as_ptr(),
            to.as_raw_fd(),
            target.as_ptr(),
            libc::RENAME_NOREPLACE,
        )
    };
    if status != 0 {
        return Err(TransactionError::Io);
    }
    Ok(())
}

pub(super) fn remove(entry: &Removal) -> Result<(), TransactionError> {
    let parent = directory(
        entry.path.parent().ok_or(TransactionError::UnsafePath)?,
        entry.parent,
    )?;
    let hold_path = entry.held.parent().ok_or(TransactionError::UnsafePath)?;
    if private_quarantine(entry) {
        match fs::symlink_metadata(hold_path) {
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                if inspect(&entry.path, entry.content.clone())?.is_some() {
                    return Err(TransactionError::UnsafePath);
                }
                parent.sync_all().map_err(|_| TransactionError::Io)?;
                return Ok(());
            }
            Err(_) => return Err(TransactionError::Io),
            Ok(_) => super::validate_directory(hold_path, true)?,
        }
    }
    let hold = directory(hold_path, entry.held_parent)?;
    let name = basename(&entry.path)?;
    let slot = basename(&entry.held)?;
    let held = inspect(&entry.held, entry.content.clone());
    match held {
        Ok(Some(actual)) if same_held(&actual, entry) => {}
        Ok(None) => {
            match inspect(&entry.path, entry.content.clone())? {
                None => {
                    parent.sync_all().map_err(|_| TransactionError::Io)?;
                    hold.sync_all().map_err(|_| TransactionError::Io)?;
                    cleanup_quarantine(entry, &parent)?;
                    return Ok(());
                }
                Some(actual) if same_source(&actual, entry) => {}
                _ => return Err(TransactionError::UnsafePath),
            }
            if matches!(entry.content, Content::Directory)
                && fs::read_dir(&entry.path)
                    .map_err(|_| TransactionError::Io)?
                    .next()
                    .is_some()
            {
                return Err(TransactionError::InvalidState(
                    "foreign file in snapshot directory",
                ));
            }
            #[cfg(test)]
            super::pause_after("uninstall_before_quarantine");
            move_exclusive(&parent, &name, &hold, &slot)?;
            parent.sync_all().map_err(|_| TransactionError::Io)?;
            hold.sync_all().map_err(|_| TransactionError::Io)?;
            #[cfg(test)]
            super::pause_after("uninstall_after_quarantine");
        }
        _ => {
            return Err(TransactionError::InvalidState(
                "uninstall quarantine contains foreign bytes",
            ));
        }
    }
    if !inspect(&entry.held, entry.content.clone())
        .is_ok_and(|value| value.is_some_and(|actual| same_held(&actual, entry)))
    {
        let _ = move_exclusive(&hold, &slot, &parent, &name);
        parent.sync_all().map_err(|_| TransactionError::Io)?;
        hold.sync_all().map_err(|_| TransactionError::Io)?;
        return Err(TransactionError::InvalidState(
            "uninstall raced with an editor; replacement preserved",
        ));
    }
    let flags = if matches!(entry.content, Content::Directory) {
        libc::AT_REMOVEDIR
    } else {
        0
    };
    if unsafe { libc::unlinkat(hold.as_raw_fd(), slot.as_ptr(), flags) } != 0 {
        return Err(TransactionError::Io);
    }
    parent.sync_all().map_err(|_| TransactionError::Io)?;
    hold.sync_all().map_err(|_| TransactionError::Io)?;
    cleanup_quarantine(entry, &parent)
}

impl InstallationOwner {
    pub(super) async fn uninstall_owned(&self) -> Result<(), TransactionError> {
        let before = self.record_bytes().await?;
        self.effects.check_recovery_preimage(before.as_deref())?;
        let mut record = self.decode_record(before.as_deref())?;
        self.effects
            .recovery_pending(if record.uninstall.is_some() {
                super::RecoveryPending::Uninstall
            } else {
                super::RecoveryPending::None
            });
        let intent = self.inspect_uninstall(&record).await?;
        let profiles = self.uninstall_profiles(&record).await?;
        let review = self
            .effects
            .review(super::ReviewSummary {
                operation: super::ReviewOperation::Uninstall,
                version: record
                    .committed
                    .as_ref()
                    .map(|receipt| receipt.active.version.clone()),
                recovery: if record.uninstall.is_some() {
                    super::RecoveryPending::Uninstall
                } else {
                    super::RecoveryPending::None
                },
                changes: vec![super::ReviewedChange::Uninstall],
            })
            .await?;
        if self.record_bytes().await? != before
            || self.inspect_uninstall(&record).await? != intent
            || self.uninstall_profiles(&record).await? != profiles
        {
            return Err(self.effects.review_changed());
        }
        self.effects.admit_reviewed(review)?;
        let FilesystemResult::UninstallIntent(intent) = self
            .effects
            .filesystem(FilesystemJob::UninstallBind {
                layout: self.layout.clone(),
                intent,
            })
            .await?
        else {
            return Err(TransactionError::UnsafePath);
        };
        record.uninstall = Some(intent);
        self.write_record(&record, false).await?;
        #[cfg(test)]
        self.effects.hit("uninstall_after_intent");
        self.effects.checkpoint()?;
        let home = if let Some(ledger) = record.shell.clone() {
            let FilesystemResult::ShellLease(lease) = self
                .effects
                .filesystem(FilesystemJob::ShellAcquire {
                    home: ledger.home(),
                    layout: self.layout.clone(),
                })
                .await?
            else {
                return Err(TransactionError::UnsafePath);
            };
            if self.uninstall_profiles(&record).await? != profiles {
                return Err(self.effects.review_changed());
            }
            for receipt in ledger
                .receipts()
                .to_vec()
                .iter()
                .skip(record.uninstall.as_ref().unwrap().shell.len())
            {
                let FilesystemResult::ShellRemovalPlan(plan) = self
                    .effects
                    .filesystem(FilesystemJob::ShellRemovalPlan {
                        lease: lease.clone(),
                        receipt: receipt.clone(),
                    })
                    .await?
                else {
                    return Err(TransactionError::UnsafePath);
                };
                record.uninstall.as_mut().unwrap().shell.push(plan);
                self.write_record(&record, false).await?;
            }
            Some(lease)
        } else {
            None
        };
        if let Some(lease) = &home {
            for plan in record.uninstall.as_ref().unwrap().shell.clone() {
                self.effects
                    .filesystem(FilesystemJob::ShellRemove {
                        lease: lease.clone(),
                        plan,
                    })
                    .await?;
            }
        }
        while let Some(entry) = record.uninstall.as_ref().unwrap().files.first().cloned() {
            self.effects
                .filesystem(FilesystemJob::UninstallRemove(entry.clone()))
                .await?;
            self.effects
                .filesystem(FilesystemJob::UninstallConfirm(UninstallIntent {
                    files: vec![entry],
                    shell: Vec::new(),
                }))
                .await?;
            record.uninstall.as_mut().unwrap().files.remove(0);
            self.write_record(&record, false).await?;
        }
        self.write_record(&InstallationRecord::default(), true)
            .await?;
        self.effects.recovery_pending(super::RecoveryPending::None);
        self.effects
            .completed(crate::diagnostics::CompletedStep::FinalSettlement);
        Ok(())
    }

    async fn uninstall_profiles(
        &self,
        record: &InstallationRecord,
    ) -> Result<Vec<Option<super::shell::ProfilePreimage>>, TransactionError> {
        let mut profiles = Vec::new();
        if let Some(ledger) = &record.shell {
            for receipt in ledger.receipts() {
                match self
                    .effects
                    .filesystem(FilesystemJob::ShellInspect {
                        home: ledger.home(),
                        name: receipt.name().into(),
                    })
                    .await?
                {
                    FilesystemResult::ShellPreimage(value) => profiles.push(value),
                    _ => return Err(TransactionError::UnsafePath),
                }
            }
        }
        Ok(profiles)
    }

    async fn inspect_uninstall(
        &self,
        record: &InstallationRecord,
    ) -> Result<UninstallIntent, TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::UninstallInspect {
                layout: self.layout.clone(),
                record: record.clone(),
            })
            .await?
        {
            FilesystemResult::UninstallIntent(intent) => Ok(intent),
            _ => Err(TransactionError::InvalidState(
                "uninstall inspection completion",
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::{PermissionsExt, symlink};

    fn fixture() -> (
        tempfile::TempDir,
        InstallationOwner,
        super::super::RootLease,
    ) {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let state = root.join("state");
        let commands = root.join("bin");
        fs::create_dir(&state).unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        fs::create_dir(&commands).unwrap();
        let owner = InstallationOwner::new(Layout::new(state, commands));
        let receipt = owner.next_receipt(
            None,
            super::super::PairIdentity {
                version: "1.0.0".into(),
                output: root.join("pair"),
                selection: None,
            },
        );
        symlink(&receipt.active.output, owner.layout.active_link()).unwrap();
        for name in ["neo", "neoctl"] {
            symlink(
                owner.layout.expected_command_target(name),
                owner.layout.command_directory.join(name),
            )
            .unwrap();
        }
        let record = InstallationRecord {
            committed: Some(receipt),
            ..Default::default()
        };
        fs::write(owner.layout.record(), serde_json::to_vec(&record).unwrap()).unwrap();
        fs::set_permissions(owner.layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
        let lease = super::super::RootLease::acquire(&owner.layout.lock()).unwrap();
        (temp, owner, lease)
    }

    #[test]
    fn owner_uninstall_removes_only_receipted_links_and_can_repeat() {
        let (_temp, owner, lease) = fixture();
        let unrelated = owner.layout.command_directory.join("other");
        fs::write(&unrelated, "preserve").unwrap();
        super::super::coordinator::test_with_review(
            &owner.effects,
            &lease,
            owner.uninstall_owned(),
        )
        .unwrap();
        assert!(!owner.layout.active_link().exists());
        assert!(fs::symlink_metadata(owner.layout.owned_links().neo).is_err());
        assert_eq!(fs::read(&unrelated).unwrap(), b"preserve");
        assert!(owner.layout.lock().exists());
        let rerun = InstallationOwner::new(owner.layout.clone());
        super::super::coordinator::test_with_review(
            &rerun.effects,
            &lease,
            rerun.uninstall_owned(),
        )
        .unwrap();
    }

    #[test]
    fn owner_uninstall_keeps_intent_after_cancellation_and_resumes() {
        let (_temp, owner, lease) = fixture();
        owner.effects.cancel_at("uninstall_after_intent");
        assert_eq!(
            super::super::coordinator::test_with_review(
                &owner.effects,
                &lease,
                owner.uninstall_owned()
            ),
            Err(TransactionError::StopRequested)
        );
        let saved: InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(saved.uninstall.is_some());
        assert!(saved.committed.is_some());
        let rerun = InstallationOwner::new(owner.layout.clone());
        super::super::coordinator::test_with_review(
            &rerun.effects,
            &lease,
            rerun.uninstall_owned(),
        )
        .unwrap();
        assert!(fs::symlink_metadata(owner.layout.owned_links().neoctl).is_err());
    }

    #[test]
    fn durable_uninstall_rejects_foreign_replacement_and_keeps_it() {
        let (_temp, owner, _lease) = fixture();
        let mut record: InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        record.uninstall = Some(prepare(&owner.layout, &record).unwrap());
        fs::remove_file(owner.layout.owned_links().neo).unwrap();
        fs::write(owner.layout.owned_links().neo, "user replacement").unwrap();
        assert!(prepare(&owner.layout, &record).is_err());
        assert_eq!(
            fs::read(owner.layout.owned_links().neo).unwrap(),
            b"user replacement"
        );
    }
}

#[cfg(test)]
mod quarantine_tests {
    use super::*;
    use std::os::unix::fs::{PermissionsExt, symlink};

    #[test]
    fn missing_source_retries_known_held_link_and_syncs_absence() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
        let source = root.join("prepared");
        symlink("prepared-2-link", &source).unwrap();
        let mut entry = inspect(&source, Content::Link("prepared-2-link".into()))
            .unwrap()
            .unwrap();
        entry.held = root.join("held");
        entry.held_parent = entry.parent;
        fs::rename(&source, &entry.held).unwrap();
        remove(&entry).unwrap();
        remove(&entry).unwrap();
        assert!(fs::symlink_metadata(&source).is_err());
        assert!(fs::symlink_metadata(&entry.held).is_err());
    }

    #[test]
    fn source_volume_quarantine_removes_and_resumes_after_directory_cleanup() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let layout = Layout::new(root.clone(), root.clone());
        let source = root.join("neo");
        symlink("expected", &source).unwrap();
        let mut entry = inspect(&source, Content::Link("expected".into()))
            .unwrap()
            .unwrap();
        entry.held = held_path(&layout, &entry.path, entry.identity);
        entry.held_parent = (entry.parent.0.wrapping_add(1), entry.parent.1);
        let intent = bind(
            &layout,
            UninstallIntent {
                files: vec![entry],
                shell: Vec::new(),
            },
        )
        .unwrap();
        let entry = &intent.files[0];
        assert_eq!(entry.held_parent.0, entry.parent.0);
        assert_eq!(
            fs::symlink_metadata(entry.held.parent().unwrap())
                .unwrap()
                .mode()
                & 0o777,
            0o700
        );
        fs::rename(&source, &entry.held).unwrap();
        remove(entry).unwrap();
        assert!(!entry.held.parent().unwrap().exists());
        remove(entry).unwrap();
        confirm(&intent).unwrap();
    }

    #[test]
    fn legacy_rebinding_preserves_unexpected_held_bytes() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let layout = Layout::new(root.clone(), root.clone());
        let source = root.join("neo");
        symlink("expected", &source).unwrap();
        let mut entry = inspect(&source, Content::Link("expected".into()))
            .unwrap()
            .unwrap();
        entry.held = held_path(&layout, &entry.path, entry.identity);
        entry.held_parent = (entry.parent.0.wrapping_add(1), entry.parent.1);
        fs::write(&entry.held, b"foreign").unwrap();
        assert!(
            bind(
                &layout,
                UninstallIntent {
                    files: vec![entry.clone()],
                    shell: Vec::new()
                }
            )
            .is_err()
        );
        assert_eq!(fs::read(&entry.held).unwrap(), b"foreign");
        assert_eq!(fs::read_link(source).unwrap(), PathBuf::from("expected"));
    }

    #[test]
    fn unrelated_held_inode_is_retained() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
        let source = root.join("neo");
        symlink(root.join("expected"), &source).unwrap();
        let mut entry = inspect(&source, Content::Link(root.join("expected")))
            .unwrap()
            .unwrap();
        entry.held = root.join("held");
        entry.held_parent = entry.parent;
        fs::write(&entry.held, "user bytes").unwrap();
        assert!(remove(&entry).is_err());
        assert_eq!(fs::read(&entry.held).unwrap(), b"user bytes");
        assert_eq!(fs::read_link(source).unwrap(), root.join("expected"));
    }
}
