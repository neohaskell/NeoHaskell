use super::observation::{Activity, Row};
use super::{
    InstallSelection, InstallationOwner, Layout, Operation, PriorObservation, TransactionError,
    process, release, snapshot, validate_directory,
};
use crate::candidate::{self, OutputPath};
use std::collections::BTreeSet;
use std::fs;
use std::os::unix::fs::DirBuilderExt;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy)]
pub(crate) enum Executor {
    NativeInstaller,
    InstalledNeo,
    Bootstrap(CandidateLocator),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CandidateLocator {
    version: [u64; 3],
    candidate_id: [u8; 64],
}

impl CandidateLocator {
    pub(crate) fn parse(value: &str) -> Result<Self, ()> {
        let (version, candidate_id) = value.split_once('@').ok_or(())?;
        let parsed = semver::Version::parse(version).map_err(|_| ())?;
        if !parsed.pre.is_empty() || !parsed.build.is_empty() || parsed.to_string() != version {
            return Err(());
        }
        if candidate_id.len() != 64
            || !candidate_id
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        {
            return Err(());
        }
        Ok(Self {
            version: [parsed.major, parsed.minor, parsed.patch],
            candidate_id: candidate_id.as_bytes().try_into().map_err(|_| ())?,
        })
    }

    fn selection(self) -> release::PinnedSelection {
        release::PinnedSelection {
            version: format!(
                "{}.{}.{}",
                self.version[0], self.version[1], self.version[2]
            ),
            candidate_id: String::from_utf8(self.candidate_id.to_vec())
                .expect("validated ASCII digest"),
        }
    }

    fn verify(
        self,
        manifest: &candidate::MetadataManifest,
        version: &str,
    ) -> Result<(), TransactionError> {
        let selected = self.selection();
        if selected.version != version
            || manifest.version != selected.version
            || manifest.candidate_id != selected.candidate_id
        {
            return Err(TransactionError::Verification(
                "bootstrap executor identity",
            ));
        }
        Ok(())
    }
}

impl Executor {
    pub(super) fn demand(self, demand: snapshot::Demand) -> snapshot::Demand {
        match (self, demand) {
            (Self::Bootstrap(locator), snapshot::Demand::SelectLatest) => {
                snapshot::Demand::Recorded(locator.selection())
            }
            (_, demand) => demand,
        }
    }
}

pub(crate) struct Outcome {
    pub(crate) version: String,
    pub(crate) previous_missing: bool,
    pub(crate) has_previous: bool,
    pub(crate) open_new_shell: bool,
}

struct ExecutorEvidence {
    executable: PathBuf,
    native: Option<candidate::FileIdentity>,
    installed: Option<OutputPath>,
    sources: Vec<snapshot::SourcePreimage>,
}

impl Layout {
    pub(crate) fn for_user() -> Result<Self, TransactionError> {
        let home = std::env::var_os("HOME")
            .map(PathBuf::from)
            .ok_or(TransactionError::UnsafePath)?;
        validate_directory(&home, false)?;
        let state_base = std::env::var_os("XDG_STATE_HOME")
            .filter(|path| !path.is_empty())
            .map(PathBuf::from)
            .unwrap_or_else(|| home.join(".local/state"));
        if !state_base.is_absolute() {
            return Err(TransactionError::UnsafePath);
        }
        create_directory(&state_base)?;
        let state = state_base.join("neohaskell");
        snapshot::private_directory(&state)?;
        let commands = home.join(".local/bin");
        Ok(Self::new(state, commands))
    }
}

pub(super) fn create_directory(path: &Path) -> Result<(), TransactionError> {
    if fs::symlink_metadata(path).is_ok() {
        return validate_directory(path, false);
    }
    let parent = path.parent().ok_or(TransactionError::UnsafePath)?;
    create_directory(parent)?;
    match fs::DirBuilder::new().mode(0o755).create(path) {
        Ok(()) => super::sync_directory(parent)?,
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(_) => return Err(TransactionError::Io),
    }
    validate_directory(path, false)
}

impl InstallationOwner {
    pub(super) async fn run_owned(
        &self,
        request: snapshot::Request,
        executor: Executor,
    ) -> Result<Option<Outcome>, TransactionError> {
        let record = self.read_record().await?;
        if record.pending_nix_policy.is_some() {
            self.effects
                .recovery_pending(super::coordinator::RecoveryPending::Unknown);
            return Err(TransactionError::NixPolicyRecoveryPending);
        }
        if matches!(request, snapshot::Request::Uninstall) {
            return self.uninstall_owned().await.map(|()| None);
        }
        if record.uninstall.is_some() {
            return Err(TransactionError::InvalidState(
                "uninstall is pending; run uninstall again",
            ));
        }
        let lease = self.effects.clone();
        let identity = crate::identity::BuildIdentity::current();
        let system = match identity.target {
            "aarch64-apple-darwin" => "aarch64-darwin",
            "aarch64-unknown-linux-gnu" => "aarch64-linux",
            "x86_64-unknown-linux-gnu" => "x86_64-linux",
            _ => return Err(TransactionError::Verification("unsupported target")),
        };
        #[cfg(not(test))]
        let source = release::ReleaseSource::production(identity.target, &lease)
            .map_err(|_| TransactionError::Verification("release transport"))?;
        #[cfg(test)]
        let source = if self.fixture {
            release::ReleaseSource::owned_fixture(identity.target, &lease)
        } else {
            release::ReleaseSource::production(identity.target, &lease)
                .map_err(|_| TransactionError::Verification("release transport"))?
        };
        let bootstrap_evidence = if matches!(executor, Executor::Bootstrap(_)) {
            let record = self.read_record().await?;
            Some(
                self.verify_executor(&lease, executor, &source, system, &record, false)
                    .await?,
            )
        } else {
            None
        };
        self.recover_shell_rerun().await?;
        let recovery = self.inspect_recovery().await?;
        if matches!(request, snapshot::Request::RecoverOnly) {
            let commands = if recovery.introduces_command_links() {
                Some(self.observe_command_path().await?)
            } else {
                None
            };
            let review = self
                .effects
                .review(recovery.summary(&request, None))
                .await?;
            self.recheck_recovery(&recovery).await?;
            if let Some(commands) = commands {
                self.recheck_command_path(commands).await?;
            }
            self.effects.admit_reviewed(review)?;
            self.effects.observe(Activity::Recovering);
            self.apply_recovery(recovery).await?;
            self.effects
                .completed(crate::diagnostics::CompletedStep::Recovery);
            self.effects
                .completed(crate::diagnostics::CompletedStep::FinalSettlement);
            return Ok(None);
        }
        let commands = self.observe_command_path().await?;
        self.effects.observe(Activity::CheckingNix);
        let before = recovery.projected.committed.clone();
        let offline = matches!(&request, snapshot::Request::Rollback)
            || matches!(&request, snapshot::Request::Install(None)) && before.is_some();
        if !offline {
            super::cache_setup::check_pending()?;
        }
        let observed_nix = self.observe_local_daemon().await?;
        self.effects.observe_row(0, Row::Existing);
        let executor_evidence = match bootstrap_evidence {
            Some(evidence) => evidence,
            None => {
                self.verify_executor(
                    &lease,
                    executor,
                    &source,
                    system,
                    &recovery.projected,
                    offline,
                )
                .await?
            }
        };
        self.effects.observe(Activity::Selecting);
        let selected = match executor.demand(recovery.projected.ready_demand(&request)?) {
            snapshot::Demand::Recorded(selected) => selected,
            snapshot::Demand::SelectExact(version) => source
                .select_exact(&version)
                .await
                .map_err(|error| error.transaction("exact candidate selection"))?,
            snapshot::Demand::SelectLatest => source
                .select_latest()
                .await
                .map_err(|error| error.transaction("complete candidate selection"))?,
            snapshot::Demand::None => {
                return Err(TransactionError::InvalidState(
                    "operation has no selected candidate",
                ));
            }
        };
        self.effects
            .completed(crate::diagnostics::CompletedStep::CandidateSelection);
        self.effects.observe(Activity::PreparingReview);
        let prepared = self
            .prepare_snapshot(&selected, &source, identity.target, offline)
            .await?;
        self.verify_remote(&lease, &prepared.candidate, system, !offline)
            .await?;
        let shell_setup = self.prepare_shell().await?;
        let mut summary = recovery.summary(&request, Some(selected.version.clone()));
        summary.changes.extend([
            super::ReviewedChange::ShellPath,
            super::ReviewedChange::ShellStartup,
        ]);
        if before
            .as_ref()
            .and_then(|roles| roles.active.selection.as_ref())
            == Some(&selected)
        {
            summary.changes.push(super::ReviewedChange::AlreadyActive);
        }
        let cache_plan = if offline {
            None
        } else {
            self.cache_plan().await?
        };
        let config_row = if cache_plan.is_some() {
            Row::Working
        } else {
            Row::Unchanged
        };
        self.effects.observe_row(1, config_row);
        self.effects.observe_row(2, config_row);
        if let Some(plan) = &cache_plan {
            summary.changes.push(plan.review());
        }
        let review = self.effects.review(summary).await?;
        self.recheck_shell(&shell_setup).await?;
        self.recheck_recovery(&recovery).await?;
        self.recheck_snapshot(&prepared).await?;
        self.recheck_executor(&executor_evidence).await?;
        if self.observe_local_daemon().await? != observed_nix {
            return Err(self.effects.review_changed());
        }
        self.recheck_command_path(commands).await?;
        if !offline && self.cache_plan().await? != cache_plan {
            return Err(self.effects.review_changed());
        }
        self.effects.admit_reviewed(review)?;
        if let Some(plan) = cache_plan {
            let result = self
                .observe(
                    &self.effects,
                    process::WorkerRequest::ApplyCacheSettings(plan),
                )
                .await?;
            if result.exit_code != Some(0)
                || result.failure.is_some()
                || self.cache_plan().await?.is_some()
            {
                return Err(TransactionError::NixBlocked(
                    super::nix::NixBlock::CacheConfiguration,
                ));
            }
            self.observe_local_daemon().await?;
            self.effects.observe_row(1, Row::Verified);
            self.effects.observe_row(2, Row::Verified);
        }
        self.effects
            .completed(crate::diagnostics::CompletedStep::Prerequisites);
        self.effects.observe(Activity::Recovering);
        self.apply_recovery(recovery).await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::Recovery);
        let candidate = self
            .publish_prepared_snapshot(&selected, prepared, identity.target)
            .await?;
        let pair = candidate
            .selected(system)
            .map_err(|_| TransactionError::Verification("candidate system"))?
            .output("pair")
            .ok_or(TransactionError::Verification("candidate pair"))?;
        self.effects.observe(Activity::PreparingPair);
        self.effects.observe_row(3, Row::Working);
        self.effects.observe_row(4, Row::Working);
        let retained = self
            .retain_candidate(&lease, &selected, pair, offline)
            .await?;
        self.effects.observe(Activity::VerifyingPair);
        self.effects.observe_row(5, Row::Working);
        let ready = self
            .verify_retention_observation(&lease, &candidate, system, retained)
            .await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::PairVerification);
        for row in 3..6 {
            self.effects.observe_row(row, Row::Verified);
        }
        self.observe_command_path().await?;
        let _home_lease = self.install_shell(&shell_setup).await?;
        if let Some(prior) = &before {
            self.verify_shell_pair(&shell_setup, &prior.active).await?;
        }
        self.effects
            .filesystem(super::FilesystemJob::EnsureCommandDirectory(
                self.layout.command_directory.clone(),
            ))
            .await?;
        let expected_pair = ready.clone();
        if before.as_ref().is_none_or(|roles| roles.active != ready) {
            let operation = match request {
                snapshot::Request::Rollback => Operation::Rollback,
                snapshot::Request::Install(version) => Operation::Install(if version.is_some() {
                    InstallSelection::ExactVersion
                } else {
                    InstallSelection::LatestCompleteStable
                }),
                snapshot::Request::Update(version) => Operation::Update(if version.is_some() {
                    InstallSelection::ExactVersion
                } else {
                    InstallSelection::LatestCompleteStable
                }),
                snapshot::Request::RecoverOnly | snapshot::Request::Uninstall => {
                    return Err(TransactionError::InvalidState(
                        "recovery requested activation",
                    ));
                }
            };
            let after = self.next_receipt(before.as_ref(), ready);
            self.effects.observe(Activity::Activating);
            self.switch_pair(before.clone(), after, operation).await?;
        }
        if before.is_none() {
            self.verify_shell_pair(&shell_setup, &expected_pair).await?;
        }
        self.effects.observe(Activity::Finishing);
        let mut record = self.read_record().await?;
        record.previous_observation = match record
            .committed
            .as_ref()
            .and_then(|roles| roles.previous.as_ref())
        {
            Some(previous) => {
                self.previous_output_observation(previous.output.clone())
                    .await?
            }
            _ => PriorObservation::NotChecked,
        };
        self.write_record(&record, false).await?;
        self.finish_resources(&lease).await?;
        self.prune_snapshots().await?;
        let mut record = self.read_record().await?;
        record.complete_selection(&selected)?;
        self.write_record(&record, false).await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::FinalSettlement);
        Ok(Some(Outcome {
            version: candidate.version().into(),
            open_new_shell: !std::env::var_os("PATH").is_some_and(|path| {
                std::env::split_paths(&path).any(|dir| dir == self.layout.command_directory)
            }),
            previous_missing: record.previous_observation == PriorObservation::MissingOutput,
            has_previous: record
                .committed
                .as_ref()
                .is_some_and(|roles| roles.previous.is_some()),
        }))
    }

    async fn observe_local_daemon(&self) -> Result<super::nix::LocalDaemon, TransactionError> {
        use super::nix::{LocalDaemon, NixBlock};

        let paths = LocalDaemon::paths().map_err(TransactionError::NixBlocked)?;
        let blocked = TransactionError::NixBlocked(NixBlock::DaemonUnavailable);
        let observation = self
            .observe(&self.effects, process::WorkerRequest::LocalDaemonInfo)
            .await
            .map_err(|error| error.preserve_stop(blocked))?;
        if observation.exit_code != Some(0) || observation.failure.is_some() {
            return Err(TransactionError::NixBlocked(NixBlock::DaemonUnavailable));
        }
        let settled = LocalDaemon::paths().map_err(TransactionError::NixBlocked)?;
        if settled != paths {
            return Err(TransactionError::NixBlocked(NixBlock::DaemonMismatch));
        }
        paths
            .with_response(&observation.stdout)
            .map_err(TransactionError::NixBlocked)
    }

    async fn previous_output_observation(
        &self,
        path: PathBuf,
    ) -> Result<PriorObservation, TransactionError> {
        match self.inspect_path(path).await {
            Ok(None) => Ok(PriorObservation::MissingOutput),
            Err(error @ (TransactionError::StopRequested | TransactionError::CleanupUnknown)) => {
                Err(error)
            }
            _ => Ok(PriorObservation::NotChecked),
        }
    }

    async fn recheck_executor(&self, evidence: &ExecutorEvidence) -> Result<(), TransactionError> {
        let super::FilesystemResult::Executable(current) = self
            .effects
            .filesystem(super::FilesystemJob::CanonicalExecutable)
            .await?
        else {
            return Err(TransactionError::InvalidState("executor completion type"));
        };
        if current != evidence.executable {
            return Err(self.effects.review_changed());
        }
        self.recheck_sources(&evidence.sources).await?;
        if let Some(expected) = &evidence.native {
            self.effects
                .filesystem(super::FilesystemJob::HashExecutor {
                    path: current,
                    expected: expected.clone(),
                })
                .await?;
        }
        if let Some(output) = &evidence.installed {
            self.successful_observation(
                &self.effects,
                process::WorkerRequest::VerifyContents {
                    pair: output.as_str().into(),
                },
            )
            .await?;
        }
        Ok(())
    }

    async fn verify_executor(
        &self,
        lease: &super::Effects,
        executor: Executor,
        source: &release::ReleaseSource,
        system: &str,
        record: &super::InstallationRecord,
        offline: bool,
    ) -> Result<ExecutorEvidence, TransactionError> {
        let super::FilesystemResult::Executable(executable) = self
            .effects
            .filesystem(super::FilesystemJob::CanonicalExecutable)
            .await?
        else {
            return Err(TransactionError::InvalidState("executor completion type"));
        };
        let build = crate::identity::BuildIdentity::current();
        match executor {
            Executor::NativeInstaller | Executor::Bootstrap(_) => {
                let locator_selection = match executor {
                    Executor::Bootstrap(locator) => {
                        let selected = locator.selection();
                        if selected.version != build.product_version {
                            return Err(TransactionError::Verification("bootstrap build version"));
                        }
                        Some(selected)
                    }
                    _ => None,
                };
                let mut saved = Vec::new();
                if let Some(selected) = &locator_selection {
                    saved.push(selected);
                }

                if let Some(selected) = &record.pinned {
                    saved.push(selected);
                }
                if let Some(roles) = &record.committed {
                    for role in std::iter::once(&roles.active).chain(roles.previous.iter()) {
                        if let Some(selected) = &role.selection {
                            saved.push(selected);
                        }
                    }
                }
                let mut authorized = None;
                let mut sources = Vec::new();
                for selected in saved {
                    if selected.version != build.product_version
                        || locator_selection
                            .as_ref()
                            .is_some_and(|locator| locator != selected)
                    {
                        continue;
                    }
                    let path = self
                        .layout
                        .state_directory
                        .join("snapshots")
                        .join(&selected.candidate_id)
                        .join("candidate.json");
                    if let Some(bytes) = self.read_snapshot(path.clone(), 2 * 1024 * 1024).await? {
                        sources.push(snapshot::SourcePreimage::new(
                            path,
                            2 * 1024 * 1024,
                            Some(&bytes),
                        ));
                        let manifest =
                            candidate::metadata_manifest(&bytes, build.target).map_err(|_| {
                                TransactionError::Verification("saved executor authorization")
                            })?;
                        if manifest.candidate_id != selected.candidate_id {
                            return Err(TransactionError::Verification("saved executor identity"));
                        }
                        authorized = Some(manifest);
                        break;
                    }
                }
                let manifest = match authorized {
                    Some(manifest) => manifest,
                    None if offline => {
                        return Err(TransactionError::Verification(
                            "offline executor authorization missing",
                        ));
                    }
                    None => {
                        source
                            .core(build.product_version)
                            .await
                            .map_err(|error| error.transaction("native executor authorization"))?
                            .1
                    }
                };
                if let Executor::Bootstrap(locator) = executor {
                    locator.verify(&manifest, build.product_version)?;
                }
                let expected = manifest.native_installer;
                self.effects
                    .filesystem(super::FilesystemJob::HashExecutor {
                        path: executable.clone(),
                        expected: expected.clone(),
                    })
                    .await?;
                Ok(ExecutorEvidence {
                    executable,
                    native: Some(expected),
                    installed: None,
                    sources,
                })
            }
            Executor::InstalledNeo => {
                let roles = record
                    .committed
                    .as_ref()
                    .ok_or(TransactionError::NoInstallation)?;
                for role in std::iter::once(&roles.active).chain(roles.previous.iter()) {
                    let selected = role
                        .selection
                        .as_ref()
                        .ok_or(TransactionError::InvalidState("executor metadata missing"))?;
                    let prepared = self
                        .prepare_snapshot(selected, source, build.target, offline)
                        .await?;
                    let candidate = prepared.candidate;
                    let outputs = candidate
                        .selected(system)
                        .map_err(|_| TransactionError::Verification("executor system"))?;
                    let neo = outputs
                        .output("neo")
                        .ok_or(TransactionError::Verification("executor output"))?;
                    if PathBuf::from(neo.as_str()).join("bin/neo") != executable {
                        continue;
                    }
                    self.verify_remote(lease, &candidate, system, false).await?;
                    let mut closure = BTreeSet::<OutputPath>::new();
                    let mut pending = vec![neo.clone()];
                    while let Some(path) = pending.pop() {
                        if closure.insert(path.clone()) {
                            pending.extend(
                                outputs
                                    .closure()
                                    .get(&path)
                                    .ok_or(TransactionError::Verification("executor closure"))?
                                    .references()
                                    .iter()
                                    .cloned(),
                            );
                        }
                    }
                    let rows = closure
                        .iter()
                        .map(|path| {
                            Ok((
                                path,
                                outputs
                                    .closure()
                                    .get(path)
                                    .ok_or(TransactionError::Verification("executor closure"))?,
                            ))
                        })
                        .collect::<Result<Vec<_>, TransactionError>>()?;
                    self.metadata_observations(lease, None, &rows).await?;
                    self.successful_observation(
                        lease,
                        process::WorkerRequest::VerifyContents {
                            pair: neo.as_str().into(),
                        },
                    )
                    .await?;
                    return Ok(ExecutorEvidence {
                        executable,
                        native: None,
                        installed: Some(neo.clone()),
                        sources: prepared.local,
                    });
                }
                Err(TransactionError::Verification(
                    "installed executor is not an owned Neo output",
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::RootLease;
    use super::*;
    use std::process::Command;

    #[test]
    #[ignore = "requires working system Nix"]
    fn configured_daemon_needs_no_cache_changes() {
        use std::os::unix::fs::PermissionsExt;

        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&directory, fs::Permissions::from_mode(0o700)).unwrap();
        let owner = InstallationOwner::new(Layout::new(directory.clone(), directory.clone()));
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        super::super::coordinator::test_with(&owner.effects, &lease, async {
            owner.observe_local_daemon().await?;
            assert!(owner.cache_plan().await?.is_none());
            Ok::<(), TransactionError>(())
        })
        .unwrap();
    }

    #[test]
    fn bootstrap_rejects_fetched_candidate_id_before_executor_or_state_changes() {
        check_bootstrap_binding(false, true);
    }

    #[test]
    fn bootstrap_rejects_a_different_executable_under_the_exact_candidate_id() {
        check_bootstrap_binding(true, false);
    }

    #[test]
    fn bootstrap_accepts_the_exact_candidate_and_executable_before_review() {
        check_bootstrap_binding(true, true);
    }

    fn check_bootstrap_binding(use_exact_id: bool, use_exact_file: bool) {
        use sha2::{Digest, Sha256};
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::os::unix::fs::PermissionsExt;
        let build = crate::identity::BuildIdentity::current();
        let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("contract/fixtures/v1");
        let index: serde_json::Value =
            serde_json::from_slice(&fs::read(fixtures.join("fixture-set.json")).unwrap()).unwrap();
        let name = index["candidate_ids"]
            .as_object()
            .unwrap()
            .keys()
            .next()
            .unwrap();
        let mut core: serde_json::Value =
            serde_json::from_slice(&fs::read(fixtures.join(name).join("candidate.json")).unwrap())
                .unwrap();
        core["version"] = build.product_version.into();
        core["context"]["version_seed"]["version"] = build.product_version.into();
        let executable = std::env::current_exe().unwrap().canonicalize().unwrap();
        let bytes = fs::read(&executable).unwrap();
        let executable_sha = format!("{:x}", Sha256::digest(&bytes));
        let identity = &mut core["native_installers"][build.target]["file"];
        identity["size"] = bytes.len().into();
        identity["sha256"] = if use_exact_file {
            executable_sha.clone()
        } else {
            "ba".repeat(32)
        }
        .into();
        drop(bytes);
        let core = serde_json::to_vec(&core).unwrap();
        let manifest = candidate::metadata_manifest(&core, build.target).unwrap();
        let digit = if manifest.candidate_id.starts_with('a') {
            "b"
        } else {
            "a"
        };
        let candidate_id = if use_exact_id {
            manifest.candidate_id.clone()
        } else {
            digit.repeat(64)
        };
        let locator =
            CandidateLocator::parse(&format!("{}@{candidate_id}", build.product_version)).unwrap();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let version = build.product_version;
        let server = std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            stream
                .set_read_timeout(Some(std::time::Duration::from_secs(5)))
                .unwrap();
            let mut reader = BufReader::new(stream.try_clone().unwrap());
            let mut line = String::new();
            reader.read_line(&mut line).unwrap();
            assert!(line.contains(&format!("/neo-v{version}/candidate.json")));
            loop {
                line.clear();
                reader.read_line(&mut line).unwrap();
                if line == "\r\n" {
                    break;
                }
            }
            write!(
                stream,
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                core.len()
            )
            .unwrap();
            stream.write_all(&core).unwrap();
        });
        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().canonicalize().unwrap();
        let state = directory.join("state");
        fs::create_dir(&state).unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let owner =
            InstallationOwner::new(Layout::new(state.clone(), directory.join("bin")));
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let source = release::ReleaseSource::bootstrap_fixture(build.target, &owner.effects, port);
        let record = super::super::InstallationRecord::default();
        let result = super::super::coordinator::test_with(
            &owner.effects,
            &lease,
            owner.verify_executor(
                &owner.effects,
                Executor::Bootstrap(locator),
                &source,
                "unused",
                &record,
                false,
            ),
        );
        match (use_exact_id, use_exact_file, result) {
            (false, _, Err(TransactionError::Verification("bootstrap executor identity"))) => {}
            (true, false, Err(TransactionError::Verification("native executor hash"))) => {}
            (true, true, Ok(observed)) => {
                assert_eq!(observed.executable, executable);
                assert_eq!(observed.native.unwrap().sha, executable_sha);
            }
            (id, file, result) => panic!(
                "unexpected verification result for id={id}, file={file}: {:?}",
                result.err()
            ),
        }
        server.join().unwrap();
        assert!(!state.join("installation.json").exists());
        assert!(!directory.join("bin").exists());
    }

    #[test]
    #[ignore]
    fn owned_container_lifecycle() {
        let base = Path::new("/home/jess/coupled-lifecycle-fixture");
        let scenario: serde_json::Value =
            serde_json::from_slice(&fs::read(base.join("scenario.json")).unwrap()).unwrap();
        let version = scenario["version"].as_str().map(str::to_owned);
        let request = match scenario["operation"].as_str().unwrap() {
            "install" => snapshot::Request::Install(version),
            "update" => snapshot::Request::Update(version),
            "rollback" => snapshot::Request::Rollback,
            "recover" => snapshot::Request::RecoverOnly,
            other => panic!("unknown fixture operation {other}"),
        };
        let owner =
            InstallationOwner::owned_fixture(Layout::new(base.join("state"), base.join("bin")));
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let result = super::super::coordinator::test_with_review(
            &owner.effects,
            &lease,
            owner.run_owned(request, Executor::NativeInstaller),
        );
        if scenario["failure"].as_bool().unwrap_or(false) {
            assert!(result.is_err(), "fixture operation unexpectedly succeeded");
            println!("fixture-error {:?}", result.err().unwrap());
        } else {
            let outcome = result.unwrap();
            if let Some(outcome) = outcome {
                assert_eq!(outcome.version, scenario["active"].as_str().unwrap());
            }
            let record =
                super::super::coordinator::test_with(&owner.effects, &lease, owner.read_record())
                    .unwrap();
            assert!(record.resources.pending.is_none());
            if let Some(roles) = &record.committed {
                let profile = record.resources.links.get("prepared").unwrap();
                let generation = record.resources.links.get(&profile.target).unwrap();
                assert_eq!(generation.target, roles.active.output.to_str().unwrap());
                let expected_roots = 1 + usize::from(roles.previous.is_some());
                assert_eq!(record.resources.links.len(), 2 + expected_roots);
            }
        }
        if let Some(version) = scenario["active"].as_str() {
            for name in ["neo", "neoctl"] {
                let output = Command::new(base.join("bin").join(name))
                    .arg("--version")
                    .output()
                    .unwrap();
                assert!(output.status.success());
                assert_eq!(output.stdout, format!("{name} {version}\n").as_bytes());
            }
        } else {
            assert!(fs::symlink_metadata(base.join("state/active")).is_err());
        }
    }
    #[test]
    fn previous_output_only_not_found_is_missing_and_cancellation_is_preserved() {
        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().canonicalize().unwrap();
        let owner = InstallationOwner::new(Layout::new(directory.clone(), directory.clone()));
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let file = directory.join("ordinary-file");
        fs::write(&file, b"value").unwrap();
        super::super::coordinator::test_with(&owner.effects, &lease, async {
            assert_eq!(
                owner
                    .previous_output_observation(directory.join("missing"))
                    .await
                    .unwrap(),
                PriorObservation::MissingOutput
            );
            assert_eq!(
                owner
                    .previous_output_observation(file.clone())
                    .await
                    .unwrap(),
                PriorObservation::NotChecked
            );
            assert_eq!(
                owner
                    .previous_output_observation(file.join("not-a-directory"))
                    .await
                    .unwrap(),
                PriorObservation::NotChecked
            );
            owner.effects.cancel_at("stop");
            owner.effects.hit("stop");
            assert_eq!(
                owner.previous_output_observation(file).await,
                Err(TransactionError::StopRequested)
            );
        });
    }
    #[test]
    fn review_executor_drift_rejects_changed_running_copy() {
        use std::os::unix::fs::PermissionsExt;
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().canonicalize().unwrap();
        let executable = root.join("executor-fixture");
        fs::copy(std::env::current_exe().unwrap(), &executable).unwrap();
        fs::set_permissions(&executable, fs::Permissions::from_mode(0o700)).unwrap();
        let output = Command::new(&executable)
            .args([
                "--exact",
                "installation::runtime::tests::fixture_review_executor_drift",
                "--ignored",
                "--test-threads=1",
            ])
            .env("NEO_EXECUTOR_DRIFT_FIXTURE", &root)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stdout)
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("1 passed; 0 failed"));
        assert_eq!(fs::read(executable).unwrap(), b"replacement executor bytes");
        assert!(!root.join("state/installation.json").exists());
        assert!(!root.join("bin").exists());
    }

    #[test]
    #[ignore]
    fn fixture_review_executor_drift() {
        use sha2::{Digest, Sha256};
        use std::os::unix::fs::PermissionsExt;
        let root = PathBuf::from(std::env::var_os("NEO_EXECUTOR_DRIFT_FIXTURE").unwrap());
        let state = root.join("state");
        fs::create_dir(&state).unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let owner = InstallationOwner::new(Layout::new(state, root.join("bin")));
        let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
        let executable = std::env::current_exe().unwrap().canonicalize().unwrap();
        let bytes = fs::read(&executable).unwrap();
        let evidence = ExecutorEvidence {
            executable: executable.clone(),
            native: Some(candidate::FileIdentity {
                name: "executor-fixture".into(),
                sha: format!("{:x}", Sha256::digest(&bytes)),
                size: bytes.len(),
            }),
            installed: None,
            sources: vec![],
        };
        let result = super::super::coordinator::test_with_review(&owner.effects, &lease, async {
            let identity = owner
                .effects
                .review(super::super::coordinator::ReviewSummary {
                    operation: super::super::coordinator::ReviewOperation::Install,
                    version: Some("1.0.0".into()),
                    recovery: super::super::coordinator::RecoveryPending::None,
                    changes: vec![],
                })
                .await?;
            owner.recheck_executor(&evidence).await?;
            fs::rename(&executable, root.join("running-original")).unwrap();
            fs::write(&executable, b"replacement executor bytes").unwrap();
            owner.recheck_executor(&evidence).await?;
            owner.effects.admit_reviewed(identity)?;
            Err::<(), _>(TransactionError::InvalidState(
                "changed executor was admitted",
            ))
        });
        assert!(matches!(
            result,
            Err(TransactionError::StopRequested | TransactionError::Verification(_))
        ));
        assert!(!owner.layout.record().exists());
        assert!(matches!(
            RootLease::acquire(&owner.layout.lock()),
            Err(TransactionError::Busy)
        ));
    }
}
