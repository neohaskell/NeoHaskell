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
}

struct ExecutorEvidence {
    executable: PathBuf,
    native: Option<candidate::FileIdentity>,
    installed: Option<OutputPath>,
    sources: Vec<snapshot::SourcePreimage>,
}

#[derive(PartialEq, Eq)]
struct NixPolicyHelperPlan {
    source: PathBuf,
    source_identity: (u64, u64, u64, u32, u32, u32, i64, i64, i64, i64),
    release_version: String,
    candidate_id: String,
    release: candidate::FileIdentity,
    nar_hash: String,
    client: PathBuf,
    expected: OutputPath,
}

#[derive(PartialEq, Eq)]
pub(super) struct NixPolicyPlan {
    #[cfg(test)]
    lower_level_fixture: bool,
    #[cfg(target_os = "linux")]
    discovery: Option<super::nix_owner::peer_handoff::launcher::DiscoveredPolicy>,
    helper: NixPolicyHelperPlan,
    baseline: super::nix_owner::ObservedNixBase,
}

impl NixPolicyPlan {
    pub(super) fn policy_operation(&self) -> super::nix_owner::policy_observation::PolicyOperation {
        #[cfg(target_os = "linux")]
        if self.baseline.can_apply_existing_policy() {
            return super::nix_owner::policy_observation::PolicyOperation::ApplyUnchanged;
        }
        super::nix_owner::policy_observation::PolicyOperation::ConventionalAppend
    }
}

#[cfg(target_os = "linux")]
impl NixPolicyPlan {
    pub(super) fn with_discovery(
        mut self,
        discovery: super::nix_owner::peer_handoff::launcher::DiscoveredPolicy,
    ) -> Result<Self, TransactionError> {
        if &self.baseline.common_policy_inputs()? != discovery.common_inputs() {
            return Err(TransactionError::InvalidState("discovery baseline differs"));
        }
        if !discovery.matches_helper(
            Path::new(self.helper.expected.as_str()),
            &self.helper.release,
        ) {
            return Err(TransactionError::InvalidState("discovery helper differs"));
        }
        self.discovery = Some(discovery);
        Ok(self)
    }
}

struct RecheckedNixPolicyPlan {
    plan: NixPolicyPlan,
    binding: super::coordinator::PolicyReviewBinding,
}

#[cfg(target_os = "linux")]
enum ExistingNixPolicyPlan {
    Installed(OutputPath),
    Downloaded(NixPolicyHelperPlan),
}

enum InstallPrerequisite {
    NotNeeded,
    #[cfg(target_os = "linux")]
    ExistingPolicy(ExistingNixPolicyPlan),
    #[cfg(target_os = "linux")]
    StockConventional(NixPolicyPlan),
    #[cfg(target_os = "linux")]
    ReviewedConventional {
        plan: NixPolicyPlan,
        runner: PolicyRunner,
    },
}

enum InstallReview {
    Ordinary(u64),
    #[cfg(target_os = "linux")]
    ExistingPolicy {
        plan: ExistingNixPolicyPlan,
        identity: u64,
    },
    #[cfg(target_os = "linux")]
    StockConventional {
        plan: NixPolicyPlan,
        binding: super::coordinator::PolicyReviewBinding,
    },
    #[cfg(target_os = "linux")]
    Conventional {
        plan: NixPolicyPlan,
        binding: super::coordinator::PolicyReviewBinding,
        runner: PolicyRunner,
    },
}

#[cfg(target_os = "linux")]
struct PolicyRunner {
    executable: super::nix_owner::peer_handoff::ExactExecutable,
    authority: super::nix_owner::peer_handoff::LaunchAuthority,
}

#[cfg(all(test, target_os = "linux"))]
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum PolicyFixtureMode {
    CompareOnly,
    PublishOnly,
    ManagedRestart,
    SudoManagedRestart,
}

#[cfg(all(test, target_os = "linux"))]
#[derive(Clone)]
pub(super) struct InstallPolicyFixture {
    pub(super) source: PathBuf,
    pub(super) service: PathBuf,
    pub(super) daemon: PathBuf,
    pub(super) pid: u32,
    pub(super) helper: OutputPath,
    pub(super) mode: PolicyFixtureMode,
}

#[cfg(target_os = "linux")]
impl PolicyRunner {
    fn discover(
        &self,
    ) -> Result<super::nix_owner::peer_handoff::launcher::DiscoveredPolicy, TransactionError> {
        self.executable
            .discover_policy(
                super::nix_owner::peer_handoff::Rendezvous::fresh()
                    .map_err(|_| TransactionError::Verification("helper rendezvous unavailable"))?,
                self.authority.clone(),
            )
            .map_err(|_| TransactionError::Verification("helper policy discovery"))?
            .map_err(|_| {
                TransactionError::NixBlocked(super::nix_owner::NixBlock::ConfigurationOwnerUnknown)
            })
    }
}

struct PreparedNixPolicyHelper {
    expected: OutputPath,
    release: candidate::FileIdentity,
    retention: super::resource_io::HelperRetention,
}

struct LaunchTarget<'a> {
    prepared: &'a PreparedNixPolicyHelper,
    #[cfg(target_os = "linux")]
    rendezvous: super::nix_owner::peer_handoff::Rendezvous,
    #[cfg(target_os = "linux")]
    executable: super::nix_owner::peer_handoff::ExactExecutable,
}

impl LaunchTarget<'_> {
    #[cfg(target_os = "linux")]
    fn observe(
        self,
        authority: super::nix_owner::peer_handoff::LaunchAuthority,
    ) -> Result<super::nix_owner::peer_handoff::QuiescentReply, TransactionError> {
        let outcome = self
            .executable
            .observe(self.rendezvous, authority)
            .map_err(|_| TransactionError::Verification("helper read-only observation"));
        let _retained_until_quiescence = &self.prepared.retention;
        outcome
    }

    fn executable(&self) -> &OutputPath {
        &self.prepared.expected
    }
}

impl InstallationOwner {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    async fn observe_darwin_nix(
        &self,
    ) -> Result<super::nix_owner::darwin::ObservedDarwinNix, TransactionError> {
        use super::nix_owner::{NixBlock, darwin};

        let blocked = TransactionError::NixBlocked(NixBlock::UnprovedService);
        let observation = self
            .observe(&self.effects, process::WorkerRequest::ObserveDarwinNix)
            .await
            .map_err(|error| error.preserve_stop(blocked))?;
        if observation.exit_code != Some(0)
            || observation.failure.is_some()
            || !observation.stderr.is_empty()
        {
            return Err(TransactionError::NixBlocked(NixBlock::UnprovedService));
        }
        let identity = darwin::decode_observation(&observation.stdout)
            .map_err(TransactionError::NixBlocked)?;
        self.effects.observe_row(0, Row::Existing);
        Ok(identity)
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    async fn observe_install_nix_base(
        &self,
    ) -> Result<super::nix_owner::ObservedNixBase, TransactionError> {
        self.observe_darwin_nix().await?;
        Err(TransactionError::NixBlocked(
            super::nix_owner::NixBlock::ConfigurationOwnerUnknown,
        ))
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    async fn inspect_darwin_policy(
        &self,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
        recovery: &super::review::PreparedRecovery,
        request: &snapshot::Request,
    ) -> Result<(), TransactionError> {
        use super::coordinator::{DarwinPublicationStep, RecoveryPending};
        use super::nix_owner::darwin::PublicationOutcome;
        use super::{FilesystemJob, FilesystemResult};
        use crate::diagnostics::CompletedStep;

        let ordinary = self.observe_darwin_nix().await?;
        if !ordinary.supports_inspection() {
            return Ok(());
        }
        let plan = self.plan_nix_policy_helper(evidence, source).await?;
        let version = plan.release_version.clone();
        let mut summary = recovery.summary(request, Some(version.clone()));
        summary.changes = vec![super::ReviewedChange::DarwinPolicyInspection];
        let review = self.effects.review(summary).await?;
        self.recheck_recovery(recovery).await?;
        self.recheck_sources(&evidence.sources).await?;
        if self.observe_darwin_nix().await? != ordinary
            || self.plan_nix_policy_helper(evidence, source).await? != plan
        {
            return Err(self.effects.review_changed());
        }
        self.effects.admit_reviewed(review)?;
        let prepared = self.prepare_policy_helper(plan).await?;
        self.recheck_prepared_nix_policy_helper(&prepared).await?;
        let root = match self
            .effects
            .filesystem(FilesystemJob::ObserveDarwinPolicy {
                path: PathBuf::from(prepared.expected.as_str()),
                release: prepared.release.clone(),
            })
            .await
        {
            Err(error @ (TransactionError::StopRequested | TransactionError::CleanupUnknown)) => {
                return Err(error);
            }
            Ok(FilesystemResult::DarwinPolicyObserved(root)) => root,
            other => {
                self.release_prepared_nix_policy_helper(prepared).await?;
                return Err(other.err().unwrap_or(TransactionError::InvalidState(
                    "Darwin policy observation completion type",
                )));
            }
        };
        if !root.confirms(&ordinary) {
            self.release_prepared_nix_policy_helper(prepared).await?;
            return Err(self.effects.review_changed());
        }
        self.effects.completed(CompletedStep::NixPolicyObservation);
        let Some(publication) = root.publication_request()? else {
            self.release_prepared_nix_policy_helper(prepared).await?;
            return Ok(());
        };
        self.effects.completed(CompletedStep::NixPolicyPlan);
        let intent = self
            .prepare_darwin_policy_intent(publication, &prepared.expected, &prepared.release)
            .await?;
        self.effects.recovery_pending(RecoveryPending::Resources);
        let review = self
            .effects
            .review_darwin_publication(recovery.summary(request, Some(version)), intent.clone())
            .await?;
        self.recheck_sources(&evidence.sources).await?;
        self.recheck_prepared_nix_policy_helper(&prepared).await?;
        self.effects.admit_reviewed(review)?;
        self.effects.recovery_pending(RecoveryPending::Unknown);
        let effect = |step| {
            self.effects
                .filesystem(FilesystemJob::DarwinPolicyPublication {
                    intent: intent.clone(),
                    step,
                })
        };
        let FilesystemResult::Applied = effect(DarwinPublicationStep::StoreIntent).await? else {
            return Err(TransactionError::InvalidState(
                "Darwin policy intent completion type",
            ));
        };
        match effect(DarwinPublicationStep::Launch).await? {
            FilesystemResult::DarwinPolicyPublished(PublicationOutcome::Refused) => {
                let FilesystemResult::DarwinPolicyIntentCleared =
                    effect(DarwinPublicationStep::ClearIntent).await?
                else {
                    return Err(TransactionError::InvalidState(
                        "Darwin policy intent clearing completion type",
                    ));
                };
                self.effects.recovery_pending(RecoveryPending::Resources);
                self.release_prepared_nix_policy_helper(prepared).await?;
                self.effects.recovery_pending(RecoveryPending::None);
                Err(TransactionError::Verification(
                    "reviewed Darwin Nix policy was refused",
                ))
            }
            FilesystemResult::DarwinPolicyPublished(PublicationOutcome::Published) => {
                self.effects.completed(CompletedStep::NixPolicyPublication);
                Err(TransactionError::NixPolicyRecoveryPending)
            }
            FilesystemResult::DarwinPolicyPublished(PublicationOutcome::Pending) => {
                Err(TransactionError::NixPolicyRecoveryPending)
            }
            _ => Err(TransactionError::InvalidState(
                "Darwin policy publication completion type",
            )),
        }
    }

    #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
    async fn observe_install_nix_base(
        &self,
    ) -> Result<super::nix_owner::ObservedNixBase, TransactionError> {
        #[cfg(all(test, target_os = "linux"))]
        if let Some(input) = self.effects.install_policy_fixture() {
            if !self.fixture {
                return Err(TransactionError::NixBlocked(
                    super::nix_owner::NixBlock::ConfigurationOwnerUnknown,
                ));
            }
            let mut observed = super::nix_owner::policy_review_fixture(&input.source, 1);
            observed.set_fixture_pid(input.pid);
            return Ok(observed);
        }
        self.observe_nix_base(&self.effects).await
    }

    async fn plan_install_prerequisite(
        &self,
        offline: bool,
        observed: &super::nix_owner::ObservedNixBase,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<InstallPrerequisite, TransactionError> {
        if offline {
            return Ok(InstallPrerequisite::NotNeeded);
        }
        #[cfg(all(test, target_os = "linux"))]
        if let Some(input) = self.effects.install_policy_fixture() {
            let plan = self.plan_nix_policy(evidence, source).await?;
            if plan.helper.expected != input.helper {
                return Err(TransactionError::Verification("fixture helper fixed path"));
            }
            let executable = super::nix_owner::peer_handoff::ExactExecutable::open(
                Path::new(plan.helper.expected.as_str()),
                &plan.helper.release,
            )
            .map_err(|_| TransactionError::Verification("helper exact executable unavailable"))?;
            let runner = PolicyRunner {
                executable,
                authority: if input.mode == PolicyFixtureMode::SudoManagedRestart {
                    super::nix_owner::peer_handoff::LaunchAuthority::SudoManagedFixture
                } else {
                    super::nix_owner::peer_handoff::LaunchAuthority::PolicyFixture {
                        mode: if input.mode == PolicyFixtureMode::ManagedRestart {
                            "managed"
                        } else {
                            "ok"
                        },
                        source: input.source,
                        service: input.service,
                        executable: input.daemon,
                        pid: input.pid,
                    }
                },
            };
            let plan = plan.with_discovery(runner.discover()?)?;
            return Ok(InstallPrerequisite::ReviewedConventional { plan, runner });
        }
        #[cfg(test)]
        if self.fixture {
            return Ok(InstallPrerequisite::NotNeeded);
        }
        #[cfg(target_os = "linux")]
        {
            if observed.has_existing_policy()
                && (evidence.installed.is_some() || !observed.can_apply_existing_policy())
            {
                let plan = match &evidence.installed {
                    Some(output) => ExistingNixPolicyPlan::Installed(output.clone()),
                    None => ExistingNixPolicyPlan::Downloaded(
                        self.plan_nix_policy_helper(evidence, source).await?,
                    ),
                };
                return Ok(InstallPrerequisite::ExistingPolicy(plan));
            }
            let plan = self.plan_nix_policy(evidence, source).await?;
            if &plan.baseline != observed {
                return Err(TransactionError::Verification(
                    "policy baseline changed during planning",
                ));
            }
            Ok(InstallPrerequisite::StockConventional(plan))
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = (evidence, source);
            observed.require_configuration_authority()?;
            Ok(InstallPrerequisite::NotNeeded)
        }
    }

    #[cfg(target_os = "linux")]
    async fn verify_reviewed_existing_policy(
        &self,
        plan: ExistingNixPolicyPlan,
        identity: u64,
        observed: &super::nix_owner::ObservedNixBase,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<super::nix_owner::peer_handoff::launcher::VerifiedExistingPolicy, TransactionError>
    {
        use super::filesystem::ExistingNixInspector;
        use super::nix_owner::peer_handoff::{ExactExecutable, launcher::VerifiedCaller};

        let lease = self.effects.policy_state_lease(&self.layout)?;
        let stamp = observed.replacement_policy_stamp()?;
        let checked = match plan {
            ExistingNixPolicyPlan::Installed(output) => {
                self.effects.admit_reviewed(identity)?;
                self.effects
                    .filesystem(super::FilesystemJob::VerifyExistingNixPolicy {
                        inspector: ExistingNixInspector::Installed(output),
                        lease,
                        stamp,
                    })
                    .await
            }
            ExistingNixPolicyPlan::Downloaded(helper) => {
                let fresh = self
                    .plan_nix_policy_helper(evidence, source)
                    .await
                    .map_err(|error| match error {
                        TransactionError::StopRequested | TransactionError::CleanupUnknown => error,
                        _ => self.effects.review_changed(),
                    })?;
                if helper != fresh || &self.observe_install_nix_base().await? != observed {
                    return Err(self.effects.review_changed());
                }
                let caller =
                    VerifiedCaller::open(&helper.source, &helper.release, &helper.source_identity)
                        .map_err(|_| self.effects.review_changed())?;
                self.effects.admit_reviewed(identity)?;
                let prepared = self.prepare_policy_helper(helper).await?;
                let checked = async {
                    self.recheck_prepared_nix_policy_helper(&prepared).await?;
                    let executable = ExactExecutable::open(
                        Path::new(prepared.expected.as_str()),
                        &prepared.release,
                    )
                    .map_err(|_| TransactionError::Verification("helper exact executable"))?;
                    self.effects
                        .filesystem(super::FilesystemJob::VerifyExistingNixPolicy {
                            inspector: ExistingNixInspector::Retained { executable, caller },
                            lease,
                            stamp,
                        })
                        .await
                }
                .await;
                if let Err(
                    error @ (TransactionError::StopRequested | TransactionError::CleanupUnknown),
                ) = checked
                {
                    return Err(error);
                }
                self.release_prepared_nix_policy_helper(prepared).await?;
                checked
            }
        }?;
        let super::FilesystemResult::ExistingNixPolicyVerified(proof) = checked else {
            return Err(TransactionError::InvalidState(
                "existing policy completion type",
            ));
        };
        if !proof.matches(lease, observed)? {
            return Err(self.effects.review_changed());
        }
        self.effects
            .completed(crate::diagnostics::CompletedStep::NixPolicyObservation);
        Ok(proof)
    }

    #[cfg(target_os = "linux")]
    async fn recheck_existing_policy(
        &self,
        proof: &super::nix_owner::peer_handoff::launcher::VerifiedExistingPolicy,
        expected: &super::nix_owner::ObservedNixBase,
    ) -> Result<(), TransactionError> {
        let actual = self.observe_install_nix_base().await?;
        let lease = self.effects.policy_state_lease(&self.layout)?;
        if &actual != expected || !proof.matches(lease, &actual)? {
            return Err(self.effects.review_changed());
        }
        Ok(())
    }

    #[cfg(target_os = "linux")]
    async fn apply_reviewed_stock_policy(
        &self,
        plan: NixPolicyPlan,
        binding: super::coordinator::PolicyReviewBinding,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<
        Option<(
            super::nix_owner::ObservedNixBase,
            super::nix_owner::peer_handoff::launcher::VerifiedExistingPolicy,
        )>,
        TransactionError,
    > {
        use super::nix_owner::peer_handoff::launcher::VerifiedCaller;

        self.effects.check_policy_recheck(&binding)?;
        let fresh = self
            .plan_nix_policy(evidence, source)
            .await
            .map_err(|error| match error {
                TransactionError::StopRequested | TransactionError::CleanupUnknown => error,
                _ => self.effects.review_changed(),
            })?;
        self.effects.check_policy_recheck(&binding)?;
        if plan != fresh {
            return Err(self.effects.review_changed());
        }
        let caller = VerifiedCaller::open(
            &plan.helper.source,
            &plan.helper.release,
            &plan.helper.source_identity,
        )
        .map_err(|_| self.effects.review_changed())?;
        let common_inputs = plan.baseline.common_policy_inputs()?;
        let operation = plan.policy_operation();
        let reviewed = RecheckedNixPolicyPlan { plan, binding };
        self.admit_nix_policy(&reviewed)?;
        let (prepared, binding) = self.prepare_nix_policy_helper(reviewed).await?;
        let observation = async {
            let target = self.reviewed_helper_target(&prepared).await?;
            self.effects
                .filesystem(super::FilesystemJob::ObserveStockPolicy {
                    executable: target.executable,
                    rendezvous: target.rendezvous,
                    caller,
                    common_inputs,
                    operation,
                })
                .await
        }
        .await;
        let (executable, caller, discovery) = match observation {
            Ok(super::FilesystemResult::StockPolicyObserved {
                executable,
                caller,
                discovery,
            }) => (executable, caller, discovery),
            Err(error @ (TransactionError::StopRequested | TransactionError::CleanupUnknown)) => {
                return Err(error);
            }
            result => {
                self.release_prepared_nix_policy_helper(prepared).await?;
                return Err(result.err().unwrap_or(TransactionError::InvalidState(
                    "policy observation completion type",
                )));
            }
        };
        self.effects
            .completed(crate::diagnostics::CompletedStep::NixPolicyObservation);
        let rendezvous = super::nix_owner::peer_handoff::Rendezvous::fresh()
            .map_err(|_| TransactionError::Verification("helper rendezvous unavailable"))?;
        let pending = executable
            .pending_stock_policy(&rendezvous, discovery)
            .map_err(|_| TransactionError::Verification("helper policy intent"))?;
        let intent = self.prepare_stock_policy_intent(binding, pending).await?;
        let expected = intent.record_bytes().to_vec();
        self.effects
            .recovery_pending(super::coordinator::RecoveryPending::Unknown);
        let super::FilesystemResult::StockPolicyPublished(mut verified) = self
            .effects
            .filesystem(super::FilesystemJob::PublishStockPolicy {
                executable,
                rendezvous,
                caller,
                intent,
            })
            .await?
        else {
            return Err(TransactionError::InvalidState(
                "policy application completion type",
            ));
        };
        self.effects
            .completed(crate::diagnostics::CompletedStep::NixPolicyApplication);
        self.effects
            .applied(crate::diagnostics::AppliedChange::NixPolicyApplied);
        let replacement = verified.take_replacement();
        self.settle_policy_intent(verified, expected).await?;
        self.release_prepared_nix_policy_helper(prepared).await?;
        let Some(proof) = replacement else {
            return Ok(None);
        };
        let observed = self.observe_install_nix_base().await?;
        let lease = self.effects.policy_state_lease(&self.layout)?;
        if !proof.matches(lease, &observed)? {
            return Err(self.effects.review_changed());
        }
        Ok(Some((observed, proof)))
    }

    #[cfg(target_os = "linux")]
    async fn recheck_install_policy(
        &self,
        plan: NixPolicyPlan,
        binding: super::coordinator::PolicyReviewBinding,
        runner: PolicyRunner,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<(), TransactionError> {
        self.effects.check_policy_recheck(&binding)?;
        let comparison = plan
            .discovery
            .as_ref()
            .ok_or(TransactionError::InvalidState("policy discovery missing"))?
            .comparison();
        let fresh = self.plan_nix_policy(evidence, source).await?;
        let discovered = runner.discover()?;
        let confirmed = runner
            .executable
            .confirm_discovery(
                &self.effects,
                &binding,
                discovered,
                comparison,
                super::nix_owner::peer_handoff::Rendezvous::fresh()
                    .map_err(|_| self.effects.review_changed())?,
                runner.authority.clone(),
            )
            .map_err(|_| self.effects.review_changed())?;
        let fresh = fresh
            .with_discovery(confirmed)
            .map_err(|_| self.effects.review_changed())?;
        let _rechecked = self.recheck_nix_policy(plan, binding, fresh)?;
        #[cfg(test)]
        if self
            .effects
            .install_policy_fixture()
            .is_some_and(|fixture| fixture.mode != PolicyFixtureMode::CompareOnly)
        {
            use super::nix_owner::policy_observation::PolicyResult;
            let discovery = _rechecked
                .plan
                .discovery
                .ok_or(TransactionError::InvalidState(
                    "confirmed policy discovery missing",
                ))?;
            let rendezvous = super::nix_owner::peer_handoff::Rendezvous::fresh()
                .map_err(|_| TransactionError::Verification("helper rendezvous unavailable"))?;
            let pending = runner
                .executable
                .pending_policy(&rendezvous, &discovery)
                .map_err(|_| TransactionError::Verification("helper policy intent"))?;
            let persisted = self
                .persist_policy_intent(_rechecked.binding, pending)
                .await?;
            let published = runner
                .executable
                .publish_policy_fixture(
                    &self.effects,
                    persisted,
                    rendezvous,
                    runner.authority,
                    discovery,
                )
                .map_err(|_| {
                    TransactionError::Verification("policy publication fixture handoff")
                })?;
            return Err(TransactionError::Verification(match published.policy {
                Some(PolicyResult::SourcePublishedApplicationPending) => {
                    "policy source published; daemon application pending"
                }
                Some(PolicyResult::ManagedRestartObserved) => {
                    "policy source published; managed restart observed; readiness unproved"
                }
                _ => "policy publication fixture did not confirm source",
            }));
        }
        Err(TransactionError::Verification(
            "policy application completion unavailable",
        ))
    }

    async fn reviewed_helper_target<'a>(
        &self,
        prepared: &'a PreparedNixPolicyHelper,
    ) -> Result<LaunchTarget<'a>, TransactionError> {
        self.recheck_prepared_nix_policy_helper(prepared).await?;
        #[cfg(target_os = "linux")]
        {
            let rendezvous = super::nix_owner::peer_handoff::Rendezvous::fresh()
                .map_err(|_| TransactionError::Verification("helper rendezvous unavailable"))?;
            let executable = super::nix_owner::peer_handoff::ExactExecutable::open(
                Path::new(prepared.expected.as_str()),
                &prepared.release,
            )
            .map_err(|_| TransactionError::Verification("helper exact executable"))?;
            Ok(LaunchTarget {
                prepared,
                rendezvous,
                executable,
            })
        }
        #[cfg(not(target_os = "linux"))]
        Err(TransactionError::Verification(
            "helper rendezvous unsupported",
        ))
    }

    async fn plan_nix_policy_helper(
        &self,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<NixPolicyHelperPlan, TransactionError> {
        let native = evidence
            .native
            .as_ref()
            .ok_or(TransactionError::Verification(
                "helper requires native release identity",
            ))?;
        if evidence.installed.is_some() {
            return Err(TransactionError::Verification(
                "helper installed executor unsupported",
            ));
        }
        let build = crate::identity::BuildIdentity::current();
        let (_, manifest) = source
            .core(build.product_version)
            .await
            .map_err(|error| error.transaction("helper fresh release authorization"))?;
        if &manifest.native_installer != native {
            return Err(TransactionError::Verification(
                "helper fresh release identity",
            ));
        }
        let release = &manifest.native_installer;
        let source_identity = super::filesystem::helper_snapshot(
            &self
                .inspect_path(evidence.executable.clone())
                .await?
                .ok_or(TransactionError::Verification("helper source missing"))?,
        );
        let super::FilesystemResult::Digest(nar_hash) = self
            .effects
            .filesystem(super::FilesystemJob::HashPolicyHelper {
                path: evidence.executable.clone(),
                expected: release.clone(),
                location: super::filesystem::PolicyHelperLocation::Source,
            })
            .await?
        else {
            return Err(TransactionError::InvalidState(
                "helper hash completion type",
            ));
        };
        let after = super::filesystem::helper_snapshot(
            &self
                .inspect_path(evidence.executable.clone())
                .await?
                .ok_or(TransactionError::Verification("helper source missing"))?,
        );
        if source_identity != after {
            return Err(TransactionError::Verification("helper source changed"));
        }
        let super::FilesystemResult::Executable(client) = self
            .effects
            .filesystem(super::FilesystemJob::NixPolicyHelperClient)
            .await?
        else {
            return Err(TransactionError::InvalidState(
                "helper client completion type",
            ));
        };
        let expected = self
            .helper_process_path(process::WorkerRequest::HelperFixedPath {
                client: client.clone(),
                nar_hash: nar_hash.clone(),
            })
            .await?;
        Ok(NixPolicyHelperPlan {
            source: evidence.executable.clone(),
            source_identity,
            release_version: manifest.version,
            candidate_id: manifest.candidate_id,
            release: release.clone(),
            nar_hash,
            client,
            expected,
        })
    }

    async fn plan_nix_policy(
        &self,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<NixPolicyPlan, TransactionError> {
        let helper = self.plan_nix_policy_helper(evidence, source).await?;
        let baseline = self.observe_install_nix_base().await?;
        #[cfg(target_os = "linux")]
        baseline.stock_policy_operation()?;
        #[cfg(not(target_os = "linux"))]
        baseline.require_verified_append()?;
        Ok(NixPolicyPlan {
            #[cfg(test)]
            lower_level_fixture: false,
            helper,
            baseline,
            #[cfg(target_os = "linux")]
            discovery: None,
        })
    }

    async fn review_nix_policy(
        &self,
        plan: NixPolicyPlan,
        summary: super::coordinator::ReviewSummary,
        evidence: &ExecutorEvidence,
        source: &release::ReleaseSource,
    ) -> Result<RecheckedNixPolicyPlan, TransactionError> {
        let (plan, binding) = self.effects.review_nix_policy(plan, summary).await?;
        let fresh = self
            .plan_nix_policy(evidence, source)
            .await
            .map_err(|error| match error {
                TransactionError::StopRequested | TransactionError::CleanupUnknown => error,
                _ => self.effects.review_changed(),
            })?;
        self.recheck_nix_policy(plan, binding, fresh)
    }

    fn recheck_nix_policy(
        &self,
        plan: NixPolicyPlan,
        binding: super::coordinator::PolicyReviewBinding,
        fresh: NixPolicyPlan,
    ) -> Result<RecheckedNixPolicyPlan, TransactionError> {
        self.effects.check_policy_recheck(&binding)?;
        #[cfg(all(test, target_os = "linux"))]
        let discovery_required = !plan.lower_level_fixture || plan.discovery.is_some();
        #[cfg(all(not(test), target_os = "linux"))]
        let discovery_required = true;
        #[cfg(target_os = "linux")]
        if discovery_required
            && !fresh
                .discovery
                .as_ref()
                .is_some_and(|value| value.is_confirmed(&binding))
        {
            return Err(self.effects.review_changed());
        }
        if plan != fresh {
            return Err(self.effects.review_changed());
        }
        Ok(RecheckedNixPolicyPlan {
            plan: fresh,
            binding,
        })
    }

    fn admit_nix_policy(&self, rechecked: &RecheckedNixPolicyPlan) -> Result<(), TransactionError> {
        self.effects.admit_policy_review(&rechecked.binding)
    }

    async fn prepare_nix_policy_helper(
        &self,
        reviewed: RecheckedNixPolicyPlan,
    ) -> Result<
        (
            PreparedNixPolicyHelper,
            super::coordinator::PolicyReviewBinding,
        ),
        TransactionError,
    > {
        self.effects
            .check_admitted_policy_review(&reviewed.binding)?;
        let prepared = self.prepare_policy_helper(reviewed.plan.helper).await?;
        Ok((prepared, reviewed.binding))
    }

    async fn prepare_policy_helper(
        &self,
        helper: NixPolicyHelperPlan,
    ) -> Result<PreparedNixPolicyHelper, TransactionError> {
        let NixPolicyHelperPlan {
            source,
            release,
            client,
            expected,
            ..
        } = helper;
        let imported = self
            .helper_process_path(process::WorkerRequest::ImportPolicyHelper { client, source })
            .await?;
        if imported != expected {
            return Err(TransactionError::Verification(
                "helper import changed expected path",
            ));
        }
        self.verify_stored_helper(&expected, &release).await?;
        let retention = self
            .retain_nix_policy_helper(&self.effects, &expected)
            .await?;
        if let Err(error) = self.verify_stored_helper(&expected, &release).await {
            if !matches!(
                error,
                TransactionError::StopRequested | TransactionError::CleanupUnknown
            ) {
                self.release_nix_policy_helper(&self.effects).await?;
            }
            return Err(error);
        }
        Ok(PreparedNixPolicyHelper {
            expected,
            release,
            retention,
        })
    }

    async fn helper_process_path(
        &self,
        request: process::WorkerRequest,
    ) -> Result<OutputPath, TransactionError> {
        let result = self.observe(&self.effects, request).await?;
        if result.exit_code != Some(0) || result.failure.is_some() {
            return Err(TransactionError::Verification("helper Nix operation"));
        }
        process::helper_output(&result.stdout)
            .map_err(|_| TransactionError::Verification("helper Nix output"))
    }

    async fn verify_stored_helper(
        &self,
        expected: &OutputPath,
        release: &candidate::FileIdentity,
    ) -> Result<(), TransactionError> {
        self.effects
            .filesystem(super::FilesystemJob::HashPolicyHelper {
                path: PathBuf::from(expected.as_str()),
                expected: release.clone(),
                location: super::filesystem::PolicyHelperLocation::Store,
            })
            .await?;
        Ok(())
    }

    async fn recheck_prepared_nix_policy_helper(
        &self,
        prepared: &PreparedNixPolicyHelper,
    ) -> Result<(), TransactionError> {
        if prepared.retention.target() != &prepared.expected {
            return Err(TransactionError::Verification("helper retention target"));
        }
        self.verify_stored_helper(&prepared.expected, &prepared.release)
            .await
    }

    async fn release_prepared_nix_policy_helper(
        &self,
        _prepared: PreparedNixPolicyHelper,
    ) -> Result<(), TransactionError> {
        self.release_nix_policy_helper(&self.effects).await
    }
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
    #[cfg(all(test, target_os = "linux"))]
    async fn inspect_fixture_policy(
        &self,
        record: &super::InstallationRecord,
        pending: &super::PendingNixPolicy,
    ) -> Result<(), TransactionError> {
        use super::nix_owner::peer_handoff::ExactExecutable;

        let unresolved = TransactionError::NixPolicyRecoveryPending;
        self.effects.checkpoint()?;
        if pending.state_lease != Some(self.effects.policy_state_lease(&self.layout)?) {
            return Err(unresolved);
        }
        let before = self
            .record_bytes()
            .await?
            .ok_or_else(|| unresolved.clone())?;
        if &self.decode_record(Some(&before))? != record {
            return Err(unresolved);
        }
        let Some(verified) =
            ExactExecutable::inspect_pending_fixture(pending).map_err(|_| unresolved.clone())?
        else {
            return Err(unresolved);
        };
        self.settle_policy_intent(verified, before).await?;
        Ok(())
    }

    #[cfg(target_os = "linux")]
    async fn verify_pending_helper_root(
        &self,
        record: &super::InstallationRecord,
        pending: &super::PendingNixPolicy,
    ) -> Result<(), TransactionError> {
        let unresolved = TransactionError::NixPolicyRecoveryPending;
        let helper = super::resources::HELPER_ROOT;
        let expected = record
            .resources
            .links
            .get(helper)
            .ok_or_else(|| unresolved.clone())?;
        let super::FilesystemResult::ObservedResources(Some((parent, links))) = self
            .effects
            .filesystem(super::FilesystemJob::InspectResourceDirectory {
                path: self.layout.state_directory.join("resources"),
                limit: record.resources.links.len() + 1,
            })
            .await?
        else {
            return Err(unresolved);
        };
        if record.resources.parent.as_ref() != Some(&parent)
            || expected.target != pending.helper_path
            || links.get(helper) != Some(expected)
        {
            return Err(unresolved);
        }
        Ok(())
    }

    #[cfg(target_os = "linux")]
    async fn inspect_pending_stock_policy(
        &self,
        record: &super::InstallationRecord,
        pending: &super::PendingNixPolicy,
    ) -> Result<(), TransactionError> {
        use super::coordinator::StockPolicyIntent;
        use super::nix_owner::peer_handoff::launcher::StockPolicyInspection;

        let unresolved = TransactionError::NixPolicyRecoveryPending;
        self.effects.checkpoint()?;
        if pending.state_lease != Some(self.effects.policy_state_lease(&self.layout)?) {
            return Err(unresolved);
        }
        let before = self
            .record_bytes()
            .await?
            .ok_or_else(|| unresolved.clone())?;
        if &self.decode_record(Some(&before))? != record {
            return Err(unresolved);
        }
        self.verify_pending_helper_root(record, pending).await?;
        let intent =
            StockPolicyIntent::open(&self.effects, &self.layout, pending.clone(), before.clone())?;
        let super::FilesystemResult::StockPolicyInspected(inspected) = self
            .effects
            .review_and_inspect_stock_policy(
                intent,
                self.verify_pending_helper_root(record, pending),
            )
            .await?
        else {
            return Err(unresolved);
        };
        let verified = match inspected {
            StockPolicyInspection::Applied(verified) => verified,
            StockPolicyInspection::Prepared(prepared) => {
                let intent = StockPolicyIntent::open(
                    &self.effects,
                    &self.layout,
                    pending.clone(),
                    before.clone(),
                )?;
                let super::FilesystemResult::StockPolicyResumed(verified) = self
                    .effects
                    .review_and_resume_stock_policy(
                        prepared,
                        intent,
                        self.verify_pending_helper_root(record, pending),
                    )
                    .await?
                else {
                    return Err(unresolved);
                };
                self.effects
                    .completed(crate::diagnostics::CompletedStep::NixPolicyApplication);
                self.effects
                    .applied(crate::diagnostics::AppliedChange::NixPolicyApplied);
                verified
            }
            StockPolicyInspection::Pending => return Err(unresolved),
        };
        self.verify_pending_helper_root(record, pending).await?;
        self.settle_policy_intent(verified, before).await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::Recovery);
        Ok(())
    }

    pub(super) async fn run_owned(
        &self,
        request: snapshot::Request,
        executor: Executor,
    ) -> Result<Option<Outcome>, TransactionError> {
        let record = self.read_record().await?;
        if let Some(_pending) = record.pending_nix_policy.as_ref() {
            self.effects
                .recovery_pending(super::coordinator::RecoveryPending::Unknown);
            #[cfg(all(test, target_os = "linux"))]
            if self.effects.inspect_policy_fixture() {
                self.inspect_fixture_policy(&record, _pending).await?;
                return Err(TransactionError::NixPolicyRecoveryCompleted);
            }
            #[cfg(target_os = "linux")]
            if matches!(executor, Executor::NativeInstaller | Executor::Bootstrap(_)) {
                self.inspect_pending_stock_policy(&record, _pending).await?;
                return Err(TransactionError::NixPolicyRecoveryCompleted);
            }
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
        #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
        if let Some(evidence) = bootstrap_evidence.as_ref()
            && !matches!(&request, snapshot::Request::Rollback)
            && !(matches!(&request, snapshot::Request::Install(None))
                && recovery.projected.committed.is_some())
        {
            self.inspect_darwin_policy(evidence, &source, &recovery, &request)
                .await?;
            return Err(TransactionError::NixBlocked(
                super::nix_owner::NixBlock::ConfigurationOwnerUnknown,
            ));
        }
        let observed_nix = self.observe_install_nix_base().await?;
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
            #[cfg(target_os = "linux")]
            if !observed_nix.has_existing_policy()
                || (observed_nix.can_apply_existing_policy()
                    && !matches!(executor, Executor::InstalledNeo))
            {
                if matches!(executor, Executor::InstalledNeo) {
                    observed_nix.require_configuration_authority()?;
                } else {
                    observed_nix.stock_policy_operation()?;
                }
            }
            #[cfg(not(target_os = "linux"))]
            observed_nix.require_configuration_authority()?;
        }
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
        let prerequisite = self
            .plan_install_prerequisite(offline, &observed_nix, &executor_evidence, &source)
            .await?;
        if matches!(&prerequisite, InstallPrerequisite::NotNeeded) {
            self.effects
                .completed(crate::diagnostics::CompletedStep::Prerequisites);
        }
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
        let shell_setup = self.prepare_shell().await?;
        let mut summary = recovery.summary(&request, Some(selected.version.clone()));
        summary.changes.extend([
            super::ReviewedChange::ShellPath,
            super::ReviewedChange::ShellStartup,
        ]);
        let review = match prerequisite {
            InstallPrerequisite::NotNeeded => {
                InstallReview::Ordinary(self.effects.review(summary).await?)
            }
            #[cfg(target_os = "linux")]
            InstallPrerequisite::ExistingPolicy(plan) => {
                summary
                    .changes
                    .push(super::ReviewedChange::ExistingNixPolicy);
                let identity = self.effects.review(summary).await?;
                InstallReview::ExistingPolicy { plan, identity }
            }
            #[cfg(target_os = "linux")]
            InstallPrerequisite::StockConventional(plan) => {
                let (plan, binding) = self.effects.review_nix_policy(plan, summary).await?;
                InstallReview::StockConventional { plan, binding }
            }
            #[cfg(target_os = "linux")]
            InstallPrerequisite::ReviewedConventional { plan, runner } => {
                let (plan, binding) = self.effects.review_nix_policy(plan, summary).await?;
                InstallReview::Conventional {
                    plan,
                    binding,
                    runner,
                }
            }
        };
        self.recheck_shell(&shell_setup).await?;
        self.recheck_recovery(&recovery).await?;
        self.recheck_snapshot(&prepared).await?;
        self.recheck_executor(&executor_evidence).await?;
        if self.observe_install_nix_base().await? != observed_nix {
            return Err(self.effects.review_changed());
        }
        self.recheck_command_path(commands).await?;
        #[cfg(target_os = "linux")]
        let mut existing_policy = None;
        #[cfg(target_os = "linux")]
        let mut replacement_nix = None;
        match review {
            InstallReview::Ordinary(identity) => self.effects.admit_reviewed(identity)?,
            #[cfg(target_os = "linux")]
            InstallReview::ExistingPolicy { plan, identity } => {
                existing_policy = Some(
                    self.verify_reviewed_existing_policy(
                        plan,
                        identity,
                        &observed_nix,
                        &executor_evidence,
                        &source,
                    )
                    .await?,
                );
                self.effects
                    .completed(crate::diagnostics::CompletedStep::Prerequisites);
            }
            #[cfg(target_os = "linux")]
            InstallReview::StockConventional { plan, binding } => {
                if let Some((observed, proof)) = self
                    .apply_reviewed_stock_policy(plan, binding, &executor_evidence, &source)
                    .await?
                {
                    replacement_nix = Some(observed);
                    existing_policy = Some(proof);
                }
                self.effects
                    .completed(crate::diagnostics::CompletedStep::Prerequisites);
            }
            #[cfg(target_os = "linux")]
            InstallReview::Conventional {
                plan,
                binding,
                runner,
            } => {
                self.recheck_install_policy(plan, binding, runner, &executor_evidence, &source)
                    .await?;
                return Err(TransactionError::InvalidState(
                    "policy application completion missing",
                ));
            }
        }
        #[cfg(target_os = "linux")]
        let observed_nix = replacement_nix.as_ref().unwrap_or(&observed_nix);
        self.effects.observe(Activity::Recovering);
        self.apply_recovery(recovery).await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::Recovery);
        self.verify_remote(&lease, &prepared.candidate, system, !offline)
            .await?;
        #[cfg(target_os = "linux")]
        if let Some(proof) = &existing_policy {
            self.recheck_existing_policy(proof, observed_nix).await?;
        }
        let candidate = self
            .publish_prepared_snapshot(&selected, prepared, identity.target)
            .await?;
        let pair = candidate
            .selected(system)
            .map_err(|_| TransactionError::Verification("candidate system"))?
            .output("pair")
            .ok_or(TransactionError::Verification("candidate pair"))?;
        #[cfg(target_os = "linux")]
        if let Some(proof) = &existing_policy {
            self.recheck_existing_policy(proof, observed_nix).await?;
        }
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
        #[cfg(target_os = "linux")]
        if let Some(proof) = &existing_policy {
            self.recheck_existing_policy(proof, observed_nix).await?;
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
            #[cfg(target_os = "linux")]
            if let Some(proof) = &existing_policy {
                self.recheck_existing_policy(proof, observed_nix).await?;
            }
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
        let state = temporary.path().join("state");
        fs::create_dir(&state).unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let owner =
            InstallationOwner::new(Layout::new(state.clone(), temporary.path().join("bin")));
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
        assert!(!temporary.path().join("bin").exists());
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

#[cfg(test)]
pub(super) fn policy_review_fixture(path: &Path, drift: &str) -> NixPolicyPlan {
    let mut plan = NixPolicyPlan {
        lower_level_fixture: true,
        #[cfg(target_os = "linux")]
        discovery: None,
        helper: NixPolicyHelperPlan {
            source: PathBuf::from("/fixture/installer"),
            source_identity: (1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
            release_version: "1.0.0".into(),
            candidate_id: "a".repeat(64),
            release: candidate::FileIdentity {
                name: "installer".into(),
                sha: "b".repeat(64),
                size: 3,
            },
            nar_hash: "c".repeat(64),
            client: PathBuf::from("/nix/store/fixture/bin/nix"),
            expected: OutputPath::parse(
                "/nix/store/i0md420ln0dwnjs0ibm8v6z2jkqpdaqi-neo-policy-helper",
            )
            .unwrap(),
        },
        baseline: super::nix_owner::policy_review_fixture(
            path,
            if drift == "service" { 2 } else { 1 },
        ),
    };
    match drift {
        "release" => plan.helper.candidate_id = "d".repeat(64),
        "helper" => plan.helper.release.sha = "e".repeat(64),
        "source" => plan.helper.source_identity.1 += 1,
        "client" => plan.helper.client = PathBuf::from("/nix/store/foreign/bin/nix"),
        _ => (),
    }
    plan.baseline.require_verified_append().unwrap();
    plan
}

#[cfg(test)]
pub(super) fn test_policy_recheck(
    owner: &InstallationOwner,
    plan: NixPolicyPlan,
    binding: super::coordinator::PolicyReviewBinding,
    fresh: NixPolicyPlan,
) -> Result<super::coordinator::PolicyReviewBinding, TransactionError> {
    Ok(owner.recheck_nix_policy(plan, binding, fresh)?.binding)
}

#[cfg(all(test, target_os = "linux"))]
pub(super) fn discovered_policy_review_fixture(
    path: &Path,
    pid: u32,
    release: &candidate::FileIdentity,
    expected: &OutputPath,
) -> NixPolicyPlan {
    let mut plan = policy_review_fixture(path, "none");
    plan.lower_level_fixture = false;
    plan.baseline.set_fixture_pid(pid);
    plan.helper.release = release.clone();
    plan.helper.expected = expected.clone();
    plan
}
