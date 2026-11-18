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
}

pub(crate) struct Outcome {
    pub(crate) version: String,
    pub(crate) previous_missing: bool,
    pub(crate) has_previous: bool,
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
        let lease = self.effects.clone();
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
        let observed_nix = self.observe_nix_base(&lease).await?;
        self.effects.observe_row(0, Row::Existing);
        self.effects.observe_row(1, Row::Unchanged);
        self.effects.observe_row(2, Row::Unchanged);
        let before = recovery.projected.committed.clone();
        let offline = matches!(&request, snapshot::Request::Rollback)
            || matches!(&request, snapshot::Request::Install(None)) && before.is_some();
        #[cfg(not(test))]
        let requires_configuration = !offline;
        #[cfg(test)]
        let requires_configuration = !offline && !self.fixture;
        if requires_configuration {
            observed_nix.require_configuration_authority()?;
        }
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
        let executor_evidence = self
            .verify_executor(
                &lease,
                executor,
                &source,
                system,
                &recovery.projected,
                offline,
            )
            .await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::Prerequisites);
        self.effects.observe(Activity::Selecting);
        let selected = match recovery.projected.ready_demand(&request)? {
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
        let review = self
            .effects
            .review(recovery.summary(&request, Some(selected.version.clone())))
            .await?;
        self.recheck_recovery(&recovery).await?;
        self.recheck_snapshot(&prepared).await?;
        self.recheck_executor(&executor_evidence).await?;
        if self.observe_nix_base(&lease).await? != observed_nix {
            return Err(self.effects.review_changed());
        }
        if requires_configuration {
            observed_nix.require_configuration_authority()?;
        }
        self.recheck_command_path(commands).await?;
        self.effects.admit_reviewed(review)?;
        self.effects.observe(Activity::Recovering);
        self.apply_recovery(recovery).await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::Recovery);
        self.effects
            .filesystem(super::FilesystemJob::EnsureCommandDirectory(
                self.layout.command_directory.clone(),
            ))
            .await?;
        let candidate = self
            .publish_prepared_snapshot(&selected, prepared, identity.target)
            .await?;
        self.verify_remote(&lease, &candidate, system, !offline)
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
                snapshot::Request::RecoverOnly => {
                    return Err(TransactionError::InvalidState(
                        "recovery requested activation",
                    ));
                }
            };
            let after = self.next_receipt(before.as_ref(), ready);
            self.effects.observe(Activity::Activating);
            self.switch_pair(before.clone(), after, operation).await?;
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
            previous_missing: record.previous_observation == PriorObservation::MissingOutput,
            has_previous: record
                .committed
                .as_ref()
                .is_some_and(|roles| roles.previous.is_some()),
        }))
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
            Executor::NativeInstaller => {
                let mut saved = Vec::new();
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
                    if selected.version != build.product_version {
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
                    let candidate = self.parse_prepared_snapshot(&prepared.snapshot)?;
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
