use super::release::{PinnedSelection, ReleaseSource, Snapshot};
use super::{
    InstallationOwner, TransactionError, open_owned_file, remove_owned_file_if_present,
    sync_directory, validate_directory,
};
use crate::candidate;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::os::unix::fs::{DirBuilderExt, OpenOptionsExt};
use std::path::Path;

#[derive(Debug)]
pub(crate) enum Request {
    Install(Option<String>),
    Update(Option<String>),
    Rollback,
    RecoverOnly,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum Demand {
    Recorded(PinnedSelection),
    SelectExact(String),
    SelectLatest,
    None,
}

impl super::InstallationRecord {
    pub(super) fn ready_demand(&self, request: &Request) -> Result<Demand, TransactionError> {
        let recorded = |pair: &super::PairIdentity| {
            pair.selection
                .clone()
                .map(Demand::Recorded)
                .ok_or(TransactionError::InvalidState(
                    "recorded pair has no candidate metadata identity",
                ))
        };
        match request {
            Request::RecoverOnly => Ok(Demand::None),
            Request::Rollback => {
                let current = self
                    .committed
                    .as_ref()
                    .ok_or(TransactionError::NoInstallation)?;
                recorded(
                    current
                        .previous
                        .as_ref()
                        .ok_or(TransactionError::NoPreviousPair)?,
                )
            }
            Request::Install(None) if self.committed.is_some() => {
                recorded(&self.committed.as_ref().unwrap().active)
            }
            Request::Update(_) if self.committed.is_none() => Err(TransactionError::NoInstallation),
            Request::Install(version) | Request::Update(version) => {
                if let Some(pinned) = &self.pinned {
                    if version
                        .as_ref()
                        .is_none_or(|version| version == &pinned.version)
                    {
                        return Ok(Demand::Recorded(pinned.clone()));
                    }
                }
                match version {
                    Some(version) => Ok(Demand::SelectExact(version.clone())),
                    None => Ok(Demand::SelectLatest),
                }
            }
        }
    }

    pub(super) fn complete_selection(
        &mut self,
        selected: &PinnedSelection,
    ) -> Result<(), TransactionError> {
        if self.pinned.as_ref() != Some(selected)
            || self
                .committed
                .as_ref()
                .and_then(|roles| roles.active.selection.as_ref())
                != Some(selected)
        {
            return Err(TransactionError::InvalidState(
                "completed selection differs from active pair",
            ));
        }
        self.pinned = None;
        Ok(())
    }
}

pub(super) struct PreparedSnapshot {
    pub(super) snapshot: Snapshot,
    pub(super) local: Vec<SourcePreimage>,
}

#[derive(Clone)]
pub(super) struct SourcePreimage {
    path: std::path::PathBuf,
    maximum: usize,
    digest: Option<String>,
}

impl SourcePreimage {
    pub(super) fn new(path: std::path::PathBuf, maximum: usize, bytes: Option<&[u8]>) -> Self {
        use sha2::{Digest, Sha256};
        Self {
            path,
            maximum,
            digest: bytes.map(|bytes| format!("{:x}", Sha256::digest(bytes))),
        }
    }
}

impl InstallationOwner {
    #[cfg(test)]
    pub(super) async fn pin_request(
        &self,
        _lease: &super::Effects,
        request: &Request,
        source: &ReleaseSource,
    ) -> Result<Option<PinnedSelection>, TransactionError> {
        self.prune_snapshots().await?;
        let mut record = self.read_record().await?;
        let selection = match record.ready_demand(request)? {
            Demand::None => return Ok(None),
            Demand::Recorded(selection) => selection,
            Demand::SelectExact(version) => source
                .select_exact(&version)
                .await
                .map_err(|error| error.transaction("exact candidate selection"))?,
            Demand::SelectLatest => source
                .select_latest()
                .await
                .map_err(|error| error.transaction("complete candidate selection"))?,
        };
        if record.pinned.as_ref() != Some(&selection) {
            if record.resources.pending.is_some() {
                return Err(TransactionError::InvalidState(
                    "pending resource effect must be reconciled before selecting",
                ));
            }
            if let Some(previous) = record.pinned.clone() {
                let committed = record.committed.as_ref().is_some_and(|roles| {
                    std::iter::once(&roles.active)
                        .chain(roles.previous.iter())
                        .any(|pair| pair.selection.as_ref() == Some(&previous))
                });
                if !committed {
                    self.discard_snapshot(&mut record, &previous.candidate_id)
                        .await?;
                }
            }
            record.pinned = Some(selection.clone());
            self.write_record(&record, false).await?;
            #[cfg(test)]
            super::pause_after("after_selection_pin");
        }
        Ok(Some(selection))
    }

    async fn record_snapshot_files(
        &self,
        selected: &PinnedSelection,
        manifest: &candidate::MetadataManifest,
        core_size: usize,
    ) -> Result<(), TransactionError> {
        let mut files = manifest.metadata.clone();
        files.push(candidate::FileIdentity {
            name: "candidate.json".into(),
            sha: selected.candidate_id.clone(),
            size: core_size,
        });
        files.sort_by(|left, right| left.name.cmp(&right.name));
        let mut record = self.read_record().await?;
        match record.snapshot_files.get(&selected.candidate_id) {
            Some(saved) if saved != &files => {
                return Err(TransactionError::InvalidState(
                    "snapshot file identities changed",
                ));
            }
            Some(_) => return Ok(()),
            None => {}
        }
        record
            .snapshot_files
            .insert(selected.candidate_id.clone(), files);
        self.write_record(&record, false).await?;
        #[cfg(test)]
        super::pause_after("after_snapshot_manifest");
        Ok(())
    }

    pub(super) async fn prune_snapshots(&self) -> Result<(), TransactionError> {
        let mut record = self.read_record().await?;
        let mut retained = BTreeSet::new();
        if let Some(selected) = &record.pinned {
            retained.insert(selected.candidate_id.clone());
        }
        if let Some(roles) = &record.committed {
            for pair in std::iter::once(&roles.active).chain(roles.previous.iter()) {
                if let Some(selected) = &pair.selection {
                    retained.insert(selected.candidate_id.clone());
                }
            }
        }
        let obsolete: Vec<_> = record
            .snapshot_files
            .keys()
            .filter(|id| !retained.contains(*id))
            .cloned()
            .collect();
        for id in obsolete {
            self.discard_snapshot(&mut record, &id).await?;
        }
        Ok(())
    }

    async fn discard_snapshot(
        &self,
        record: &mut super::InstallationRecord,
        id: &str,
    ) -> Result<(), TransactionError> {
        let directory = self.layout.state_directory.join("snapshots").join(id);
        self.effects
            .filesystem(super::FilesystemJob::PruneSnapshot {
                directory,
                files: record.snapshot_files.get(id).cloned().unwrap_or_default(),
            })
            .await?;
        record.snapshot_files.remove(id);
        self.write_record(record, false).await
    }

    pub(super) async fn prepare_snapshot(
        &self,
        selected: &PinnedSelection,
        source: &ReleaseSource,
        target: &str,
        offline: bool,
    ) -> Result<PreparedSnapshot, TransactionError> {
        super::validate_selection(selected)?;
        let directory = self
            .layout
            .state_directory
            .join("snapshots")
            .join(&selected.candidate_id);
        let mut local = Vec::new();
        let core_path = directory.join("candidate.json");
        let saved = self
            .read_snapshot(core_path.clone(), 2 * 1024 * 1024)
            .await?;
        local.push(SourcePreimage::new(
            core_path,
            2 * 1024 * 1024,
            saved.as_deref(),
        ));
        let core = match saved {
            Some(bytes) => bytes,
            None if offline => {
                return Err(TransactionError::Verification("offline snapshot missing"));
            }
            None => {
                source
                    .core(&selected.version)
                    .await
                    .map_err(|error| error.transaction("pinned candidate download"))?
                    .0
            }
        };
        let manifest = candidate::metadata_manifest(&core, target)
            .map_err(|_| TransactionError::Verification("snapshot core"))?;
        if manifest.candidate_id != selected.candidate_id || manifest.version != selected.version {
            return Err(TransactionError::Verification("snapshot identity"));
        }
        let mut files = BTreeMap::new();
        for file in manifest.metadata {
            let path = directory.join(&file.name);
            let saved = self.read_snapshot(path.clone(), file.size).await?;
            local.push(SourcePreimage::new(path, file.size, saved.as_deref()));
            let bytes = match saved {
                Some(bytes) => bytes,
                None if offline => {
                    return Err(TransactionError::Verification("offline metadata missing"));
                }
                None => source
                    .file(selected, &file)
                    .await
                    .map_err(|error| error.transaction("pinned metadata download"))?,
            };
            file.verify(&bytes)
                .map_err(|_| TransactionError::Verification("snapshot file identity"))?;
            files.insert(file.name, bytes);
        }
        let snapshot = Snapshot { core, files };
        snapshot
            .parse()
            .map_err(|_| TransactionError::Verification("complete snapshot"))?;
        Ok(PreparedSnapshot { snapshot, local })
    }

    pub(super) async fn recheck_snapshot(
        &self,
        prepared: &PreparedSnapshot,
    ) -> Result<(), TransactionError> {
        self.recheck_sources(&prepared.local).await
    }

    pub(super) async fn recheck_sources(
        &self,
        sources: &[SourcePreimage],
    ) -> Result<(), TransactionError> {
        for expected in sources {
            let bytes = self
                .read_snapshot(expected.path.clone(), expected.maximum)
                .await?;
            let current =
                SourcePreimage::new(expected.path.clone(), expected.maximum, bytes.as_deref());
            if current.digest != expected.digest {
                return Err(self.effects.review_changed());
            }
        }
        Ok(())
    }

    pub(super) async fn publish_prepared_snapshot(
        &self,
        selected: &PinnedSelection,
        prepared: PreparedSnapshot,
        target: &str,
    ) -> Result<candidate::ParsedCandidate, TransactionError> {
        self.prune_snapshots().await?;
        let mut record = self.read_record().await?;
        if record.resources.pending.is_some() {
            return Err(TransactionError::InvalidState(
                "pending resource effect before selection",
            ));
        }
        if record.pinned.as_ref() != Some(selected) {
            if let Some(previous) = record.pinned.clone() {
                let committed = record.committed.as_ref().is_some_and(|roles| {
                    std::iter::once(&roles.active)
                        .chain(roles.previous.iter())
                        .any(|pair| pair.selection.as_ref() == Some(&previous))
                });
                if !committed {
                    self.discard_snapshot(&mut record, &previous.candidate_id)
                        .await?;
                }
            }
            record.pinned = Some(selected.clone());
            self.write_record(&record, false).await?;
            #[cfg(test)]
            super::pause_after("after_selection_pin");
        }
        let snapshots = self.layout.state_directory.join("snapshots");
        self.effects
            .filesystem(super::FilesystemJob::EnsureSnapshotDirectory(
                snapshots.clone(),
            ))
            .await?;
        let directory = snapshots.join(&selected.candidate_id);
        self.effects
            .filesystem(super::FilesystemJob::EnsureSnapshotDirectory(
                directory.clone(),
            ))
            .await?;
        let manifest = candidate::metadata_manifest(&prepared.snapshot.core, target)
            .map_err(|_| TransactionError::Verification("snapshot core"))?;
        self.record_snapshot_files(selected, &manifest, prepared.snapshot.core.len())
            .await?;
        for (name, bytes) in std::iter::once(("candidate.json", &prepared.snapshot.core)).chain(
            prepared
                .snapshot
                .files
                .iter()
                .map(|(name, bytes)| (name.as_str(), bytes)),
        ) {
            let path = directory.join(name);
            match self.read_snapshot(path.clone(), bytes.len()).await? {
                Some(current) if current != *bytes => {
                    return Err(TransactionError::Verification(
                        "snapshot changed before publication",
                    ));
                }
                Some(_) => {}
                None => {
                    self.effects
                        .filesystem(super::FilesystemJob::PublishSnapshot {
                            path,
                            bytes: bytes.clone(),
                        })
                        .await?;
                }
            }
            #[cfg(test)]
            super::pause_after(&format!(
                "after_snapshot_{}",
                if name == "candidate.json" {
                    "core"
                } else {
                    name
                }
            ));
        }
        self.parse_prepared_snapshot(&prepared.snapshot)
    }

    pub(super) fn parse_prepared_snapshot(
        &self,
        snapshot: &Snapshot,
    ) -> Result<candidate::ParsedCandidate, TransactionError> {
        let parsed = snapshot
            .parse()
            .map_err(|_| TransactionError::Verification("complete snapshot"))?;
        #[cfg(test)]
        let parsed = {
            let mut parsed = parsed;
            if self.fixture {
                parsed.fixture_authority();
            }
            parsed
        };
        Ok(parsed)
    }

    #[cfg(test)]
    pub(super) async fn capture_snapshot(
        &self,
        _lease: &super::Effects,
        selected: &PinnedSelection,
        source: &ReleaseSource,
        target: &str,
    ) -> Result<candidate::ParsedCandidate, TransactionError> {
        let prepared = self
            .prepare_snapshot(selected, source, target, false)
            .await?;
        self.publish_prepared_snapshot(selected, prepared, target)
            .await
    }
}

pub(super) fn private_directory(path: &Path) -> Result<(), TransactionError> {
    match fs::DirBuilder::new().mode(0o700).create(path) {
        Ok(()) => sync_directory(path.parent().ok_or(TransactionError::UnsafePath)?)?,
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(_) => return Err(TransactionError::Io),
    }
    validate_directory(path, true)
}

pub(super) fn write_bytes(path: &Path, bytes: &[u8]) -> Result<(), TransactionError> {
    let parent = path.parent().ok_or(TransactionError::UnsafePath)?;
    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or(TransactionError::UnsafePath)?;
    let temporary = parent.join(format!(".{name}.tmp"));
    remove_owned_file_if_present(&temporary)?;
    if open_owned_file(path, true, false)?.is_some() {
        return Err(TransactionError::InvalidState("snapshot already exists"));
    }
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(&temporary)
        .map_err(|_| TransactionError::Io)?;
    file.write_all(bytes).map_err(|_| TransactionError::Io)?;
    file.sync_all().map_err(|_| TransactionError::Io)?;
    fs::rename(temporary, path).map_err(|_| TransactionError::Io)?;
    sync_directory(parent)
}

#[cfg(test)]
mod tests {
    use super::super::RootLease;
    use super::super::{InstallationRecord, Layout, PairIdentity, Receipt, STATE_SCHEMA};
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    fn selected(version: &str, digit: char) -> PinnedSelection {
        PinnedSelection {
            version: version.into(),
            candidate_id: digit.to_string().repeat(64),
        }
    }
    fn pair(selection: PinnedSelection) -> PairIdentity {
        PairIdentity {
            version: selection.version.clone(),
            output: format!(
                "/nix/store/00000000000000000000000000000000-{}",
                selection.version
            )
            .into(),
            selection: Some(selection),
        }
    }

    #[test]
    fn demand_preserves_first_install_pin_and_does_not_require_pending_or_current_health() {
        let a = selected("0.17.1", 'a');
        let b = selected("0.17.2", 'b');
        let p = selected("0.17.0", 'c');
        let mut record = InstallationRecord::default();
        record.pinned = Some(b.clone());
        assert_eq!(
            record.ready_demand(&Request::Install(None)).unwrap(),
            Demand::Recorded(b.clone())
        );
        let layout = Layout::new("/tmp/state".into(), "/tmp/bin".into());
        record.committed = Some(Receipt {
            schema: STATE_SCHEMA,
            installation_id: "abc".into(),
            active: pair(a.clone()),
            previous: Some(pair(p.clone())),
            links: layout.owned_links(),
        });
        assert_eq!(
            record.ready_demand(&Request::Install(None)).unwrap(),
            Demand::Recorded(a)
        );
        assert_eq!(
            record.ready_demand(&Request::Rollback).unwrap(),
            Demand::Recorded(p)
        );
        assert_eq!(
            record
                .ready_demand(&Request::Update(Some("0.17.2".into())))
                .unwrap(),
            Demand::Recorded(b)
        );
        assert_eq!(
            record
                .ready_demand(&Request::Update(Some("0.17.3".into())))
                .unwrap(),
            Demand::SelectExact("0.17.3".into())
        );
        assert_eq!(
            record.ready_demand(&Request::RecoverOnly).unwrap(),
            Demand::None
        );
    }

    #[test]
    fn completed_selection_releases_only_its_own_resumable_pin() {
        let a = selected("0.17.1", 'a');
        let b = selected("0.17.2", 'b');
        let links = Layout::new("/tmp/state".into(), "/tmp/bin".into()).owned_links();
        let mut record = InstallationRecord::default();
        record.pinned = Some(a.clone());
        record.committed = Some(Receipt {
            schema: STATE_SCHEMA,
            installation_id: "abc".into(),
            active: pair(a.clone()),
            previous: None,
            links: links.clone(),
        });

        record.complete_selection(&a).unwrap();

        assert_eq!(record.pinned, None);
        assert_eq!(
            record.ready_demand(&Request::Update(None)).unwrap(),
            Demand::SelectLatest
        );
        assert_eq!(
            record.ready_demand(&Request::Rollback),
            Err(TransactionError::NoPreviousPair)
        );

        record.pinned = Some(b.clone());
        assert_eq!(
            record
                .ready_demand(&Request::Update(Some(b.version.clone())))
                .unwrap(),
            Demand::Recorded(b.clone())
        );
        assert!(record.complete_selection(&b).is_err());
        assert_eq!(record.pinned, Some(b.clone()));

        record.committed = Some(Receipt {
            schema: STATE_SCHEMA,
            installation_id: "abc".into(),
            active: pair(b.clone()),
            previous: Some(pair(a.clone())),
            links: links.clone(),
        });
        record.complete_selection(&b).unwrap();
        assert_eq!(record.pinned, None);
        assert_eq!(
            record.ready_demand(&Request::Rollback).unwrap(),
            Demand::Recorded(a.clone())
        );

        record.pinned = Some(a.clone());
        record.committed = Some(Receipt {
            schema: STATE_SCHEMA,
            installation_id: "abc".into(),
            active: pair(a.clone()),
            previous: Some(pair(b.clone())),
            links,
        });
        record.complete_selection(&a).unwrap();
        assert_eq!(record.pinned, None);
        assert_eq!(
            record.ready_demand(&Request::Update(None)).unwrap(),
            Demand::SelectLatest
        );
        assert_eq!(
            record.ready_demand(&Request::Rollback).unwrap(),
            Demand::Recorded(b)
        );
    }

    #[test]
    fn abandoned_snapshot_cleanup_refuses_foreign_files_and_does_not_demand_metadata_health() {
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().canonicalize().unwrap();
        let state = root.join("state");
        let commands = root.join("bin");
        for path in [&state, &commands] {
            fs::create_dir(path).unwrap();
            fs::set_permissions(path, fs::Permissions::from_mode(0o700)).unwrap();
        }
        let owner = InstallationOwner::new(Layout::new(state.clone(), commands));
        owner.effects.accept_fixture_review();
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let pinned = selected("0.17.2", 'b');
        let snapshots = state.join("snapshots");
        private_directory(&snapshots).unwrap();
        let directory = snapshots.join(&pinned.candidate_id);
        private_directory(&directory).unwrap();
        write_bytes(&directory.join("candidate.json"), b"broken").unwrap();
        write_bytes(&directory.join("closures.json"), b"broken").unwrap();
        write_bytes(&directory.join("user-notes.txt"), b"preserve").unwrap();
        let mut record = InstallationRecord::default();
        record.pinned = Some(pinned.clone());
        record.snapshot_files.insert(
            pinned.candidate_id.clone(),
            vec![
                candidate::FileIdentity {
                    name: "candidate.json".into(),
                    sha: pinned.candidate_id.clone(),
                    size: 100,
                },
                candidate::FileIdentity {
                    name: "closures.json".into(),
                    sha: "a".repeat(64),
                    size: 100,
                },
            ],
        );
        super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.write_record(&record, false),
        )
        .unwrap();
        assert!(
            super::super::coordinator::test_with(
                &owner.effects,
                &lease,
                owner.discard_snapshot(&mut record, &pinned.candidate_id)
            )
            .is_err()
        );
        assert_eq!(
            fs::read(directory.join("candidate.json")).unwrap(),
            b"broken"
        );
        assert_eq!(
            fs::read(directory.join("user-notes.txt")).unwrap(),
            b"preserve"
        );
        fs::remove_file(directory.join("user-notes.txt")).unwrap();
        super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.discard_snapshot(&mut record, &pinned.candidate_id),
        )
        .unwrap();
        assert!(!directory.exists());
        assert!(
            super::super::coordinator::test_with(&owner.effects, &lease, owner.read_record())
                .unwrap()
                .snapshot_files
                .is_empty()
        );
    }

    #[test]
    fn partial_snapshot_retry_keeps_the_pin_and_complete_snapshot_needs_no_network() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let state = root.join("state");
        let commands = root.join("bin");
        for path in [&state, &commands] {
            fs::create_dir(path).unwrap();
            fs::set_permissions(path, fs::Permissions::from_mode(0o700)).unwrap();
        }
        let owner = InstallationOwner::new(Layout::new(state.clone(), commands));
        owner.effects.accept_fixture_review();
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("contract/fixtures/v1");
        let index: serde_json::Value =
            serde_json::from_slice(&fs::read(fixtures.join("fixture-set.json")).unwrap()).unwrap();
        let name = index["candidate_ids"]
            .as_object()
            .unwrap()
            .keys()
            .next()
            .unwrap();
        let fixture = fixtures.join(name);
        let core = fs::read(fixture.join("candidate.json")).unwrap();
        let manifest = candidate::metadata_manifest(&core, "aarch64-apple-darwin").unwrap();
        let pinned = PinnedSelection {
            version: manifest.version,
            candidate_id: manifest.candidate_id,
        };
        let mut record = InstallationRecord::default();
        record.pinned = Some(pinned.clone());
        super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.write_record(&record, false),
        )
        .unwrap();
        let source = ReleaseSource::unavailable_fixture(&owner.effects);
        let selected = super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.pin_request(&owner.effects, &Request::Install(None), &source),
        )
        .unwrap()
        .unwrap();
        assert_eq!(selected, pinned);
        let snapshots = state.join("snapshots");
        private_directory(&snapshots).unwrap();
        let directory = snapshots.join(&pinned.candidate_id);
        private_directory(&directory).unwrap();
        write_bytes(&directory.join("candidate.json"), &core).unwrap();
        assert!(
            super::super::coordinator::test_with(
                &owner.effects,
                &lease,
                owner.capture_snapshot(&owner.effects, &pinned, &source, "aarch64-apple-darwin")
            )
            .is_err()
        );
        assert_eq!(
            super::super::coordinator::test_with(
                &owner.effects,
                &lease,
                owner.pin_request(&owner.effects, &Request::Install(None), &source)
            )
            .unwrap(),
            Some(pinned.clone())
        );
        for file in manifest.metadata {
            write_bytes(
                &directory.join(&file.name),
                &fs::read(fixture.join(file.name)).unwrap(),
            )
            .unwrap();
        }
        let snapshot = super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.capture_snapshot(&owner.effects, &pinned, &source, "aarch64-apple-darwin"),
        )
        .unwrap();
        assert_eq!(snapshot.id(), pinned.candidate_id);
    }
    fn review_snapshot_fixture() -> (
        tempfile::TempDir,
        InstallationOwner,
        RootLease,
        PinnedSelection,
    ) {
        let temporary = tempfile::tempdir().unwrap();
        let state = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let owner = InstallationOwner::new(Layout::new(state.clone(), state.join("bin")));
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("contract/fixtures/v1");
        let index: serde_json::Value =
            serde_json::from_slice(&fs::read(fixtures.join("fixture-set.json")).unwrap()).unwrap();
        let name = index["candidate_ids"]
            .as_object()
            .unwrap()
            .keys()
            .next()
            .unwrap();
        let fixture = fixtures.join(name);
        let core = fs::read(fixture.join("candidate.json")).unwrap();
        let manifest = candidate::metadata_manifest(&core, "aarch64-apple-darwin").unwrap();
        let selected = PinnedSelection {
            version: manifest.version,
            candidate_id: manifest.candidate_id,
        };
        let snapshots = state.join("snapshots");
        private_directory(&snapshots).unwrap();
        let directory = snapshots.join(&selected.candidate_id);
        private_directory(&directory).unwrap();
        write_bytes(&directory.join("candidate.json"), &core).unwrap();
        for file in manifest.metadata {
            write_bytes(
                &directory.join(&file.name),
                &fs::read(fixture.join(&file.name)).unwrap(),
            )
            .unwrap();
        }
        (temporary, owner, lease, selected)
    }

    #[test]
    fn review_prepared_snapshot_uses_exact_retained_candidate_without_release_demand() {
        let (_temporary, owner, lease, selected) = review_snapshot_fixture();
        let source = ReleaseSource::unavailable_fixture(&owner.effects);
        let prepared = super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.prepare_snapshot(&selected, &source, "aarch64-apple-darwin", true),
        )
        .unwrap();
        assert!(!owner.layout.record().exists());
        assert_eq!(
            prepared.snapshot.parse().unwrap().id(),
            selected.candidate_id
        );
        super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.recheck_snapshot(&prepared),
        )
        .unwrap();
        owner.effects.accept_fixture_review();
        let parsed = super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.publish_prepared_snapshot(&selected, prepared, "aarch64-apple-darwin"),
        )
        .unwrap();
        assert_eq!(parsed.id(), selected.candidate_id);
        let record =
            super::super::coordinator::test_with(&owner.effects, &lease, owner.read_record())
                .unwrap();
        assert_eq!(record.pinned, Some(selected));
    }

    #[test]
    fn review_changed_local_source_is_rejected_before_publication() {
        let (_temporary, owner, lease, selected) = review_snapshot_fixture();
        let source = ReleaseSource::unavailable_fixture(&owner.effects);
        let prepared = super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.prepare_snapshot(&selected, &source, "aarch64-apple-darwin", true),
        )
        .unwrap();
        let path = owner
            .layout
            .state_directory
            .join("snapshots")
            .join(&selected.candidate_id)
            .join("candidate.json");
        let mut changed = fs::read(&path).unwrap();
        changed.push(b'\n');
        fs::write(path, changed).unwrap();
        assert_eq!(
            super::super::coordinator::test_with(
                &owner.effects,
                &lease,
                owner.recheck_snapshot(&prepared)
            ),
            Err(TransactionError::StopRequested)
        );
        assert!(!owner.layout.record().exists());
        assert!(!owner.layout.command_directory.exists());
    }
}
