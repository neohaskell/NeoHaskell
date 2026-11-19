use super::dashboard::terminal::ParkedTerminal;
use super::filesystem::{FilesystemJob, FilesystemResult, FilesystemTask};
use super::nix_owner::policy_observation::PolicyOperation;
use super::{
    InstallationOwner, Layout, PairIdentity, RootLease, TransactionError, process, runtime,
    snapshot,
};
use std::cell::RefCell;
use std::future::Future;
use std::pin::Pin;
use std::rc::{Rc, Weak};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::task::{Context, Poll, Wake, Waker};
use std::time::{Duration, Instant};

#[derive(Clone, Default)]
pub(super) struct Cancellation {
    requested: Arc<AtomicBool>,
    #[cfg(test)]
    cut: Arc<std::sync::Mutex<Option<&'static str>>>,
}

impl Cancellation {
    pub(super) fn request(&self) {
        self.requested.store(true, Ordering::Release);
    }
    pub(super) fn requested(&self) -> bool {
        self.requested.load(Ordering::Acquire)
    }
    #[cfg(test)]
    pub(super) fn is_cut(&self, point: &'static str) -> bool {
        self.cut.lock().unwrap().as_ref() == Some(&point)
    }
    #[cfg(test)]
    pub(super) fn hit(&self, point: &'static str) {
        if self.is_cut(point) {
            self.request();
        }
    }
}

pub(crate) enum RunOutcome {
    Complete(Option<runtime::Outcome>),
    Stopped(StoppedOperation),
    Unadmitted(UnadmittedOperation),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum UnadmittedReason {
    ConsentRequired,
    Declined,
    Cancelled,
    ReviewChanged,
}

pub(crate) struct UnadmittedOperation {
    pub(crate) reason: UnadmittedReason,
    pub(crate) recovery: RecoveryPending,
    _quiescent: Quiescent,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RecoveryPending {
    Uninstall,
    Shell,
    Unknown,
    None,
    Activation,
    Resources,
    ActivationAndResources,
}

pub(crate) struct ReviewSummary {
    pub(crate) operation: ReviewOperation,
    pub(crate) version: Option<String>,
    pub(crate) recovery: RecoveryPending,
    pub(crate) changes: Vec<ReviewedChange>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ReviewOperation {
    Uninstall,
    Install,
    Update,
    Rollback,
    Recover,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ReviewedChange {
    NixPolicy,
    ExistingNixPolicy,
    ApplyExistingNixPolicy,
    #[cfg(target_os = "linux")]
    NixPolicyRecoveryInspection,
    #[cfg(target_os = "linux")]
    ResumePreparedNixPolicy,
    Uninstall,
    ShellPath,
    ShellStartup,
    Recovery,
    CommandDirectory,
    CommandLinks,
    Selection,
    Retention,
    Activation,
    Cleanup,
}

pub(super) struct ReviewOffer {
    summary: ReviewSummary,
    token: ReviewToken,
}
pub(super) struct ReviewToken {
    owner: Weak<RefCell<EffectState>>,
    identity: u64,
}

pub(super) struct PolicyReviewBinding {
    owner: Weak<RefCell<EffectState>>,
    identity: u64,
    operation: PolicyOperation,
}

pub(super) struct PolicyComparisonWitness {
    owner: Weak<RefCell<EffectState>>,
    identity: u64,
}

#[cfg(all(test, target_os = "linux"))]
pub(super) struct PersistedPolicyIntent {
    binding: PolicyReviewBinding,
    pending: super::PendingNixPolicy,
}

#[cfg(all(test, target_os = "linux"))]
impl PersistedPolicyIntent {
    pub(super) fn check_owner(&self, effects: &Effects) -> Result<(), TransactionError> {
        effects.check_policy_recheck(&self.binding)?;
        let state = effects.state.borrow();
        let lease = state.lease.as_ref().ok_or(TransactionError::InvalidState(
            "Nix policy intent has no owner lease",
        ))?;
        if self.pending.state_lease != Some(lease.policy_identity(&lease.path)?) {
            return Err(TransactionError::InvalidState(
                "Nix policy state lease changed",
            ));
        }
        Ok(())
    }

    pub(super) fn binding(&self) -> &PolicyReviewBinding {
        &self.binding
    }

    pub(super) fn pending(&self) -> &super::PendingNixPolicy {
        &self.pending
    }
}

#[cfg(target_os = "linux")]
pub(super) struct PreparedStockPolicyIntent {
    intent: StockPolicyIntent,
    expected: Option<Vec<u8>>,
}

#[cfg(target_os = "linux")]
impl PreparedStockPolicyIntent {
    pub(super) fn pending(&self) -> &super::PendingNixPolicy {
        self.intent.pending()
    }

    pub(super) fn record_bytes(&self) -> &[u8] {
        &self.intent.bytes
    }

    pub(super) fn persist(
        self,
        policy: &super::filesystem::ReadPolicy,
    ) -> Result<StockPolicyIntent, TransactionError> {
        self.intent.verify_lease()?;
        super::filesystem::publish_policy_intent(
            &self.intent.layout,
            self.expected.as_deref(),
            &self.intent.bytes,
            policy,
        )?;
        Ok(self.intent)
    }
}

#[cfg(target_os = "linux")]
pub(super) struct StockPolicyIntent {
    lock: RootLease,
    layout: Layout,
    pending: super::PendingNixPolicy,
    bytes: Vec<u8>,
}

#[cfg(target_os = "linux")]
impl StockPolicyIntent {
    pub(super) fn open(
        effects: &Effects,
        layout: &Layout,
        pending: super::PendingNixPolicy,
        bytes: Vec<u8>,
    ) -> Result<Self, TransactionError> {
        effects.checkpoint()?;
        let intent = Self::with_lease(effects, layout, pending, bytes)?;
        intent.verify()?;
        Ok(intent)
    }

    fn with_lease(
        effects: &Effects,
        layout: &Layout,
        pending: super::PendingNixPolicy,
        bytes: Vec<u8>,
    ) -> Result<Self, TransactionError> {
        let state = effects.state.borrow();
        let lease = state.lease.as_ref().ok_or(TransactionError::InvalidState(
            "Nix policy intent has no owner lease",
        ))?;
        Ok(Self {
            lock: RootLease {
                file: lease.file.try_clone().map_err(|_| TransactionError::Io)?,
                path: lease.path.clone(),
            },
            layout: layout.clone(),
            pending,
            bytes,
        })
    }

    pub(super) fn pending(&self) -> &super::PendingNixPolicy {
        &self.pending
    }

    fn verify_lease(&self) -> Result<(), TransactionError> {
        let changed = || TransactionError::InvalidState("Nix policy intent changed before commit");
        let expected_lease = self.pending.state_lease.ok_or_else(changed)?;
        if self.lock.policy_identity(&self.layout.lock())? != expected_lease {
            return Err(changed());
        }
        Ok(())
    }

    pub(super) fn verify(&self) -> Result<(), TransactionError> {
        use std::io::Read;
        self.verify_lease()?;
        let changed = || TransactionError::InvalidState("Nix policy intent changed before commit");
        let file =
            super::open_owned_file(&self.layout.record(), true, false)?.ok_or_else(changed)?;
        let mut bytes = Vec::new();
        file.take(self.bytes.len() as u64 + 1)
            .read_to_end(&mut bytes)
            .map_err(|_| TransactionError::Io)?;
        if bytes != self.bytes {
            return Err(changed());
        }
        Ok(())
    }
}

impl PolicyComparisonWitness {
    pub(super) fn same_review(&self, other: &PolicyReviewBinding) -> bool {
        Weak::ptr_eq(&self.owner, &other.owner) && self.identity == other.identity
    }
}

impl ReviewOffer {
    pub(super) fn summary(&self) -> &ReviewSummary {
        &self.summary
    }
}

#[derive(Clone, Copy)]
pub(super) enum ReviewDecision {
    Accept,
    Decline,
    ConsentRequired,
}

pub(super) enum OwnerInterest<'a> {
    Ready,
    Waiting {
        descriptor: Option<(std::os::fd::BorrowedFd<'a>, libc::c_short)>,
        deadline: Option<Instant>,
    },
}

enum ReviewGate {
    Inspecting,
    Awaiting {
        identity: u64,
        offer: Option<ReviewOffer>,
    },
    Rechecking(u64),
    #[cfg(all(test, target_os = "linux"))]
    WritingPolicyIntent,
    #[cfg(target_os = "linux")]
    ClearingPolicyIntent,
    Accepted {
        identity: u64,
        progress: AcceptedProgress,
    },
    #[cfg(target_os = "linux")]
    AcceptedPolicyRecovery,
    #[cfg(target_os = "linux")]
    AcceptedPolicyResumption {
        pending: super::PendingNixPolicy,
        progress: AcceptedProgress,
    },
    Stopped(UnadmittedReason),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AcceptedProgress {
    NoMutationAdmitted,
    MutationAdmitted,
}

pub(super) struct MutationPermission {
    _private: (),
}

pub(crate) struct StoppedOperation {
    pub(crate) observation: StopObservation,
    _quiescent: Quiescent,
}

pub(crate) enum StopObservation {
    NotRequired,
    ObservedNoPair,
    ObservedPair(PairIdentity),
    #[cfg(target_os = "linux")]
    PolicyRecoveryInterrupted,
}

struct Quiescent;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SettlementKind {
    Recovery,
    Activation,
    Resource,
    Shell,
    Observation,
}

#[derive(Clone)]
pub(super) struct SettlementPermit {
    identity: u64,
    kind: SettlementKind,
    failed_journal: Option<(Option<Vec<u8>>, Vec<u8>)>,
}

impl SettlementPermit {
    pub(super) fn kind(&self) -> SettlementKind {
        self.kind
    }
}

#[derive(Clone)]
pub(super) enum Admission {
    Requested,
    Settlement(SettlementPermit),
}

#[derive(Clone)]
pub(super) struct Effects {
    state: Rc<RefCell<EffectState>>,
    cancellation: Cancellation,
    admission: Admission,
}

struct EffectState {
    #[cfg(all(test, target_os = "linux"))]
    install_policy_fixture: Option<super::runtime::InstallPolicyFixture>,
    #[cfg(all(test, target_os = "linux"))]
    inspect_policy_fixture: bool,
    lease: Option<RootLease>,
    slot: EffectSlot,
    settlement: Option<SettlementPermit>,
    next_identity: u64,
    review: ReviewGate,
    recovery: RecoveryPending,
    journal_attempt: Option<JournalAttempt>,
    observation: super::observation::PresentationObservation,
    diagnostics: super::observation::InvocationObservation,
}

struct JournalAttempt {
    review: u64,
    kind: SettlementKind,
    path: std::path::PathBuf,
    before: Option<Vec<u8>>,
    bytes: Vec<u8>,
    publication: Result<(), TransactionError>,
}

#[cfg(target_os = "linux")]
impl InstallationOwner {
    pub(super) async fn prepare_stock_policy_intent(
        &self,
        binding: PolicyReviewBinding,
        pending: super::PendingNixPolicy,
    ) -> Result<PreparedStockPolicyIntent, TransactionError> {
        self.effects.check_admitted_policy_review(&binding)?;
        if pending.operation != binding.operation {
            return Err(TransactionError::InvalidState(
                "Nix policy operation differs from review",
            ));
        }
        let (expected, pending, bytes) = self.policy_intent_record(pending).await?;
        self.effects.consume_policy_review(binding)?;
        let intent = StockPolicyIntent::with_lease(&self.effects, &self.layout, pending, bytes)?;
        Ok(PreparedStockPolicyIntent { intent, expected })
    }

    async fn policy_intent_record(
        &self,
        mut pending: super::PendingNixPolicy,
    ) -> Result<(Option<Vec<u8>>, super::PendingNixPolicy, Vec<u8>), TransactionError> {
        let expected = self.record_bytes().await?;
        let mut record = self.decode_record(expected.as_deref())?;
        if record.pending_nix_policy.is_some() {
            return Err(TransactionError::InvalidState(
                "Nix policy outcome is pending",
            ));
        }
        pending.state_lease = Some(self.effects.policy_state_lease(&self.layout)?);
        record.pending_nix_policy = Some(pending.clone());
        let bytes = self.encode_record(&record)?;
        Ok((expected, pending, bytes))
    }

    #[cfg(test)]
    pub(super) async fn persist_policy_intent(
        &self,
        binding: PolicyReviewBinding,
        pending: super::PendingNixPolicy,
    ) -> Result<PersistedPolicyIntent, TransactionError> {
        self.effects.check_policy_intent_review(&binding)?;
        let (expected, pending, bytes) = self.policy_intent_record(pending).await?;
        self.effects.check_policy_intent_review(&binding)?;
        let review = std::mem::replace(
            &mut self.effects.state.borrow_mut().review,
            ReviewGate::WritingPolicyIntent,
        );
        self.effects.recovery_pending(RecoveryPending::Unknown);
        let result = self
            .effects
            .filesystem(FilesystemJob::PublishPolicyIntent {
                layout: self.layout.clone(),
                expected,
                bytes,
            })
            .await;
        self.effects.state.borrow_mut().review = review;
        let FilesystemResult::Applied = result? else {
            return Err(TransactionError::InvalidState(
                "Nix policy intent completion type",
            ));
        };
        self.effects.check_policy_intent_review(&binding)?;
        Ok(PersistedPolicyIntent { binding, pending })
    }

    pub(super) async fn settle_policy_intent(
        &self,
        verified: super::nix_owner::peer_handoff::launcher::VerifiedPolicyInspection,
        expected: Vec<u8>,
    ) -> Result<(), TransactionError> {
        self.effects.checkpoint()?;
        let record = self.decode_record(Some(&expected))?;
        let pending = record
            .pending_nix_policy
            .as_ref()
            .ok_or(TransactionError::InvalidState(
                "Nix policy intent is missing",
            ))?;
        if !verified.matches(pending)
            || pending.state_lease != Some(self.effects.policy_state_lease(&self.layout)?)
            || self.record_bytes().await?.as_deref() != Some(expected.as_slice())
        {
            return Err(TransactionError::InvalidState(
                "Nix policy intent changed during inspection",
            ));
        }
        self.effects.checkpoint()?;
        let review = std::mem::replace(
            &mut self.effects.state.borrow_mut().review,
            ReviewGate::ClearingPolicyIntent,
        );
        let result = self
            .effects
            .filesystem(FilesystemJob::ClearPolicyIntent {
                layout: self.layout.clone(),
                expected,
            })
            .await;
        self.effects.state.borrow_mut().review = review;
        let FilesystemResult::Applied = result? else {
            return Err(TransactionError::InvalidState(
                "Nix policy intent settlement type",
            ));
        };
        self.effects.recovery_pending(RecoveryPending::None);
        Ok(())
    }
}

impl Drop for EffectState {
    fn drop(&mut self) {
        let slot = std::mem::replace(&mut self.slot, EffectSlot::Idle);
        drop(slot);
    }
}

enum EffectSlot {
    Idle,
    Admitted(EffectRequest),
    Active(ActiveEffect),
    AdministratorFinished(Result<EffectResult, TransactionError>),
    Completed(Result<EffectResult, TransactionError>),
}

enum EffectRequest {
    Filesystem(FilesystemJob, Admission),
    Process(process::WorkerRequest),
}

#[cfg(target_os = "linux")]
enum PolicyIntentOperation {
    #[cfg(test)]
    Publish,
    Clear,
}

impl EffectRequest {
    fn accepts_administrator_terminal(&self) -> bool {
        #[cfg(target_os = "linux")]
        if let Self::Filesystem(
            FilesystemJob::ObserveStockPolicy { .. }
            | FilesystemJob::PublishStockPolicy { .. }
            | FilesystemJob::VerifyExistingNixPolicy { .. }
            | FilesystemJob::InspectStockPolicy(_)
            | FilesystemJob::ResumeStockPolicy(_),
            Admission::Requested,
        ) = self
        {
            return true;
        }
        false
    }

    fn mutates(&self) -> bool {
        match self {
            Self::Filesystem(job, _) => job.mutates(),
            Self::Process(request) => request.mutates(),
        }
    }

    #[cfg(target_os = "linux")]
    fn policy_intent_operation(&self) -> Option<(&Layout, PolicyIntentOperation)> {
        match self {
            #[cfg(test)]
            Self::Filesystem(FilesystemJob::PublishPolicyIntent { layout, .. }, _) => {
                Some((layout, PolicyIntentOperation::Publish))
            }
            Self::Filesystem(FilesystemJob::ClearPolicyIntent { layout, .. }, _) => {
                Some((layout, PolicyIntentOperation::Clear))
            }
            _ => None,
        }
    }
}

impl EffectSlot {
    fn mutates(&self) -> bool {
        matches!(self, Self::Admitted(request) if request.mutates())
    }
}

enum ActiveEffect {
    Filesystem(FilesystemTask),
    Process {
        owner: process::OperationOwner,
        request: Option<process::WorkerRequest>,
        stop: ProcessStop,
    },
}

enum ProcessStop {
    Running,
    Cancellation,
    SubmissionFailure,
}

enum EffectResult {
    Filesystem(FilesystemResult),
    Process(process::OperationResult),
}

struct EffectFuture {
    effects: Effects,
    request: Option<EffectRequest>,
    admitted: bool,
}

impl Future for EffectFuture {
    type Output = Result<EffectResult, TransactionError>;
    fn poll(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        if !this.admitted {
            if let Err(error) = this.effects.check_admission(this.request.as_ref().unwrap()) {
                return Poll::Ready(Err(error));
            }
            let mut state = this.effects.state.borrow_mut();
            if !matches!(state.slot, EffectSlot::Idle) {
                return Poll::Ready(Err(TransactionError::InvalidState(
                    "effect already admitted",
                )));
            }
            if matches!(this.effects.admission, Admission::Requested) {
                state.journal_attempt = None;
            }
            state.slot = EffectSlot::Admitted(this.request.take().unwrap());
            if state.slot.mutates() {
                match &mut state.review {
                    ReviewGate::Accepted { progress, .. } => {
                        *progress = AcceptedProgress::MutationAdmitted;
                    }
                    #[cfg(target_os = "linux")]
                    ReviewGate::AcceptedPolicyResumption { progress, .. } => {
                        *progress = AcceptedProgress::MutationAdmitted;
                    }
                    _ => {}
                }
            }
            this.admitted = true;
            return Poll::Pending;
        }
        let mut state = this.effects.state.borrow_mut();
        if !matches!(state.slot, EffectSlot::Completed(_)) {
            return Poll::Pending;
        }
        let EffectSlot::Completed(result) = std::mem::replace(&mut state.slot, EffectSlot::Idle)
        else {
            unreachable!()
        };
        Poll::Ready(result)
    }
}

impl Effects {
    pub(super) fn observe(&self, activity: super::observation::Activity) {
        let mut state = self.state.borrow_mut();
        state.observation.activity(activity);
        state.diagnostics.activity(activity);
    }
    pub(super) fn completed(&self, step: crate::diagnostics::CompletedStep) {
        self.state.borrow_mut().diagnostics.completed(step);
    }
    pub(super) fn applied(&self, change: crate::diagnostics::AppliedChange) {
        self.state.borrow_mut().diagnostics.applied(change);
    }
    pub(super) fn observe_row(&self, row: usize, value: super::observation::Row) {
        self.state.borrow_mut().observation.rows[row] = value;
    }
    pub(super) fn recovery_pending(&self, pending: RecoveryPending) {
        self.state.borrow_mut().recovery = pending;
    }

    pub(super) async fn review(&self, summary: ReviewSummary) -> Result<u64, TransactionError> {
        self.checkpoint()?;
        self.state.borrow_mut().observation.review(&summary)?;
        let identity = {
            let mut state = self.state.borrow_mut();
            if !matches!(state.review, ReviewGate::Inspecting)
                || !matches!(state.slot, EffectSlot::Idle)
            {
                return Err(TransactionError::InvalidState("review already prepared"));
            }
            state.next_identity = state
                .next_identity
                .checked_add(1)
                .ok_or(TransactionError::InvalidState("owner identity exhausted"))?;
            let identity = state.next_identity;
            state.diagnostics.review();
            state.review = ReviewGate::Awaiting {
                identity,
                offer: Some(ReviewOffer {
                    summary,
                    token: ReviewToken {
                        owner: Rc::downgrade(&self.state),
                        identity,
                    },
                }),
            };
            identity
        };
        std::future::poll_fn(|_| {
            if self.stopped() {
                return Poll::Ready(Err(TransactionError::StopRequested));
            }
            match self.state.borrow().review {
                ReviewGate::Awaiting { .. } => Poll::Pending,
                ReviewGate::Rechecking(current) if current == identity => Poll::Ready(Ok(identity)),
                ReviewGate::Stopped(_) => Poll::Ready(Err(TransactionError::StopRequested)),
                _ => Poll::Ready(Err(TransactionError::InvalidState("review expired"))),
            }
        })
        .await
    }

    #[cfg(target_os = "linux")]
    pub(super) async fn review_and_inspect_stock_policy(
        &self,
        intent: StockPolicyIntent,
        recheck_helper: impl Future<Output = Result<(), TransactionError>>,
    ) -> Result<FilesystemResult, TransactionError> {
        let identity = self
            .review(ReviewSummary {
                operation: ReviewOperation::Recover,
                version: None,
                recovery: RecoveryPending::Unknown,
                changes: vec![ReviewedChange::NixPolicyRecoveryInspection],
            })
            .await?;
        recheck_helper.await?;
        intent.verify().map_err(|_| self.review_changed())?;
        self.admit_policy_recovery(identity, ReviewGate::AcceptedPolicyRecovery)?;
        self.filesystem(FilesystemJob::InspectStockPolicy(intent))
            .await
    }

    #[cfg(target_os = "linux")]
    pub(super) async fn review_and_resume_stock_policy(
        &self,
        prepared: super::nix_owner::peer_handoff::launcher::VerifiedPreparedPolicy,
        intent: StockPolicyIntent,
        recheck_helper: impl Future<Output = Result<(), TransactionError>>,
    ) -> Result<FilesystemResult, TransactionError> {
        if !prepared.matches(intent.pending()) {
            return Err(self.review_changed());
        }
        intent.verify().map_err(|_| self.review_changed())?;
        {
            let mut state = self.state.borrow_mut();
            if !matches!(state.review, ReviewGate::AcceptedPolicyRecovery)
                || !matches!(state.slot, EffectSlot::Idle)
                || state.settlement.is_some()
            {
                return Err(TransactionError::InvalidState(
                    "Nix policy resumption requires completed inspection",
                ));
            }
            state.review = ReviewGate::Inspecting;
        }
        let identity = self
            .review(ReviewSummary {
                operation: ReviewOperation::Recover,
                version: None,
                recovery: RecoveryPending::Unknown,
                changes: vec![ReviewedChange::ResumePreparedNixPolicy],
            })
            .await?;
        recheck_helper.await?;
        intent.verify().map_err(|_| self.review_changed())?;
        self.admit_policy_recovery(
            identity,
            ReviewGate::AcceptedPolicyResumption {
                pending: intent.pending().clone(),
                progress: AcceptedProgress::NoMutationAdmitted,
            },
        )?;
        self.filesystem(FilesystemJob::ResumeStockPolicy(intent))
            .await
    }

    #[cfg(target_os = "linux")]
    fn admit_policy_recovery(
        &self,
        identity: u64,
        accepted: ReviewGate,
    ) -> Result<(), TransactionError> {
        self.checkpoint()?;
        let mut state = self.state.borrow_mut();
        if !matches!(state.review, ReviewGate::Rechecking(current) if current == identity)
            || !matches!(state.slot, EffectSlot::Idle)
            || state.settlement.is_some()
        {
            return Err(TransactionError::InvalidState(
                "Nix policy recovery review was not rechecked",
            ));
        }
        state.diagnostics.admitted();
        state.review = accepted;
        Ok(())
    }

    pub(super) async fn review_nix_policy(
        &self,
        plan: super::runtime::NixPolicyPlan,
        mut summary: ReviewSummary,
    ) -> Result<(super::runtime::NixPolicyPlan, PolicyReviewBinding), TransactionError> {
        let operation = plan.policy_operation();
        let change = if operation.is_append() {
            ReviewedChange::NixPolicy
        } else {
            ReviewedChange::ApplyExistingNixPolicy
        };
        if !summary.changes.contains(&change) {
            summary.changes.push(change);
        }
        let identity = self.review(summary).await?;
        Ok((
            plan,
            PolicyReviewBinding {
                owner: Rc::downgrade(&self.state),
                identity,
                operation,
            },
        ))
    }

    pub(super) fn check_policy_recheck(
        &self,
        binding: &PolicyReviewBinding,
    ) -> Result<(), TransactionError> {
        self.checkpoint()?;
        if !Weak::ptr_eq(&binding.owner, &Rc::downgrade(&self.state)) {
            return Err(TransactionError::InvalidState("foreign policy review"));
        }
        if !matches!(self.state.borrow().review, ReviewGate::Rechecking(identity) if identity == binding.identity)
        {
            return Err(TransactionError::InvalidState(
                "policy review is not rechecking",
            ));
        }
        Ok(())
    }

    #[cfg(all(test, target_os = "linux"))]
    fn check_policy_intent_review(
        &self,
        binding: &PolicyReviewBinding,
    ) -> Result<(), TransactionError> {
        self.checkpoint()?;
        if !Weak::ptr_eq(&binding.owner, &Rc::downgrade(&self.state))
            || !matches!(
                self.state.borrow().review,
                ReviewGate::Rechecking(identity) | ReviewGate::Accepted { identity, .. }
                    if identity == binding.identity
            )
        {
            return Err(TransactionError::InvalidState("Nix policy review changed"));
        }
        Ok(())
    }

    pub(super) fn policy_comparison_witness(
        &self,
        binding: &PolicyReviewBinding,
    ) -> Result<PolicyComparisonWitness, TransactionError> {
        self.check_policy_recheck(binding)?;
        Ok(PolicyComparisonWitness {
            owner: binding.owner.clone(),
            identity: binding.identity,
        })
    }

    pub(super) fn admit_policy_review(
        &self,
        binding: &PolicyReviewBinding,
    ) -> Result<(), TransactionError> {
        self.check_policy_recheck(binding)?;
        self.admit_reviewed(binding.identity)
    }

    pub(super) fn consume_policy_review(
        &self,
        binding: PolicyReviewBinding,
    ) -> Result<(), TransactionError> {
        self.check_admitted_policy_review(&binding)
    }

    pub(super) fn check_admitted_policy_review(
        &self,
        binding: &PolicyReviewBinding,
    ) -> Result<(), TransactionError> {
        self.checkpoint()?;
        if !Weak::ptr_eq(&binding.owner, &Rc::downgrade(&self.state))
            || !matches!(self.state.borrow().review, ReviewGate::Accepted { identity, .. } if identity == binding.identity)
        {
            return Err(TransactionError::InvalidState(
                "policy review was not admitted",
            ));
        }
        Ok(())
    }

    fn take_review(&self) -> Option<ReviewOffer> {
        match &mut self.state.borrow_mut().review {
            ReviewGate::Awaiting { offer, .. } => offer.take(),
            _ => None,
        }
    }

    fn answer(
        &self,
        token: ReviewToken,
        withhold: Option<UnadmittedReason>,
    ) -> Result<(), TransactionError> {
        self.checkpoint()?;
        if !Weak::ptr_eq(&token.owner, &Rc::downgrade(&self.state)) {
            return Err(TransactionError::InvalidState("foreign review token"));
        }
        let mut state = self.state.borrow_mut();
        if !matches!(state.review, ReviewGate::Awaiting { identity, .. } if identity == token.identity)
        {
            return Err(TransactionError::InvalidState("expired review token"));
        }
        state.diagnostics.answer(match withhold {
            None => crate::diagnostics::ReviewAnswer::Accepted,
            Some(UnadmittedReason::Declined) => crate::diagnostics::ReviewAnswer::Declined,
            Some(UnadmittedReason::ConsentRequired) => {
                crate::diagnostics::ReviewAnswer::ConsentRequired
            }
            Some(_) => crate::diagnostics::ReviewAnswer::Cancelled,
        });
        state.review = match withhold {
            Some(reason) => ReviewGate::Stopped(reason),
            None => ReviewGate::Rechecking(token.identity),
        };
        Ok(())
    }

    pub(super) fn admit_reviewed(&self, identity: u64) -> Result<(), TransactionError> {
        self.checkpoint()?;
        let mut state = self.state.borrow_mut();
        if !matches!(state.review, ReviewGate::Rechecking(current) if current == identity) {
            return Err(TransactionError::InvalidState("review was not rechecked"));
        }
        state.diagnostics.admitted();
        state.review = ReviewGate::Accepted {
            identity,
            progress: AcceptedProgress::NoMutationAdmitted,
        };
        Ok(())
    }

    pub(super) fn review_changed(&self) -> TransactionError {
        if self.mutation_admitted() {
            return TransactionError::Verification(
                "reviewed installation changed after changes began",
            );
        }
        self.state.borrow_mut().review = ReviewGate::Stopped(UnadmittedReason::ReviewChanged);
        TransactionError::StopRequested
    }

    fn mutation_admitted(&self) -> bool {
        #[cfg(target_os = "linux")]
        if self.policy_resumption_admitted() {
            return true;
        }
        matches!(
            self.state.borrow().review,
            ReviewGate::Accepted {
                progress: AcceptedProgress::MutationAdmitted,
                ..
            }
        )
    }

    #[cfg(target_os = "linux")]
    fn policy_resumption_admitted(&self) -> bool {
        matches!(
            self.state.borrow().review,
            ReviewGate::AcceptedPolicyResumption {
                progress: AcceptedProgress::MutationAdmitted,
                ..
            }
        )
    }

    pub(super) async fn begin_reviewed_recovery(
        &self,
        job: FilesystemJob,
    ) -> Result<Self, TransactionError> {
        self.checkpoint()?;
        if !job.mutates() || !job.permits_settlement(SettlementKind::Recovery) {
            return Err(TransactionError::InvalidState(
                "invalid first recovery effect",
            ));
        }
        let settlement = {
            let mut state = self.state.borrow_mut();
            if !matches!(state.review, ReviewGate::Accepted { .. })
                || !matches!(state.slot, EffectSlot::Idle)
                || state.settlement.is_some()
            {
                return Err(TransactionError::InvalidState(
                    "recovery reservation unavailable",
                ));
            }
            state.next_identity = state
                .next_identity
                .checked_add(1)
                .ok_or(TransactionError::InvalidState("owner identity exhausted"))?;
            let permit = SettlementPermit {
                identity: state.next_identity,
                kind: SettlementKind::Recovery,
                failed_journal: None,
            };
            state.settlement = Some(permit.clone());
            if let ReviewGate::Accepted { progress, .. } = &mut state.review {
                *progress = AcceptedProgress::MutationAdmitted;
            }
            state.slot = EffectSlot::Admitted(EffectRequest::Filesystem(
                job,
                Admission::Settlement(permit.clone()),
            ));
            Self {
                state: self.state.clone(),
                cancellation: self.cancellation.clone(),
                admission: Admission::Settlement(permit),
            }
        };
        EffectFuture {
            effects: settlement.clone(),
            request: None,
            admitted: true,
        }
        .await?;
        Ok(settlement)
    }

    fn new(cancellation: Cancellation) -> Self {
        Self {
            state: Rc::new(RefCell::new(EffectState {
                #[cfg(all(test, target_os = "linux"))]
                install_policy_fixture: None,
                #[cfg(all(test, target_os = "linux"))]
                inspect_policy_fixture: false,
                lease: None,
                slot: EffectSlot::Idle,
                settlement: None,
                next_identity: 0,
                review: ReviewGate::Inspecting,
                recovery: RecoveryPending::Unknown,
                journal_attempt: None,
                observation: super::observation::PresentationObservation::new(),
                diagnostics: super::observation::InvocationObservation::new(),
            })),
            cancellation,
            admission: Admission::Requested,
        }
    }

    #[cfg(all(test, target_os = "linux"))]
    pub(super) fn install_policy_fixture(&self) -> Option<super::runtime::InstallPolicyFixture> {
        self.state.borrow().install_policy_fixture.clone()
    }

    #[cfg(all(test, target_os = "linux"))]
    pub(super) fn inspect_policy_fixture(&self) -> bool {
        self.state.borrow().inspect_policy_fixture
    }

    #[cfg(target_os = "linux")]
    pub(super) fn policy_state_lease(
        &self,
        layout: &Layout,
    ) -> Result<super::PolicyStateLease, TransactionError> {
        let state = self.state.borrow();
        let lease = state.lease.as_ref().ok_or(TransactionError::InvalidState(
            "Nix policy intent has no owner lease",
        ))?;
        lease.policy_identity(&layout.lock())
    }

    pub(super) fn stopped(&self) -> bool {
        self.cancellation.requested()
    }
    #[cfg(test)]
    pub(super) fn hit(&self, point: &'static str) {
        self.cancellation.hit(point);
    }

    fn check_admission(&self, request: &EffectRequest) -> Result<(), TransactionError> {
        if matches!(
            request,
            EffectRequest::Filesystem(FilesystemJob::Startup(_), _)
        ) && self.state.borrow().lease.is_some()
        {
            return Err(TransactionError::InvalidState("startup already completed"));
        }
        #[cfg(target_os = "linux")]
        if let EffectRequest::Filesystem(
            job @ (FilesystemJob::InspectStockPolicy(intent)
            | FilesystemJob::ResumeStockPolicy(intent)),
            _,
        ) = request
        {
            self.checkpoint()?;
            let state = self.state.borrow();
            let reviewed = match (&state.review, job) {
                (ReviewGate::AcceptedPolicyRecovery, FilesystemJob::InspectStockPolicy(_)) => true,
                (
                    ReviewGate::AcceptedPolicyResumption {
                        pending: expected, ..
                    },
                    FilesystemJob::ResumeStockPolicy(_),
                ) => expected == intent.pending(),
                _ => false,
            };
            return if matches!(self.admission, Admission::Requested)
                && reviewed
                && state.lease.as_ref().is_some_and(|owner| {
                    owner
                        .policy_identity(&intent.layout.lock())
                        .is_ok_and(|lease| Some(lease) == intent.pending().state_lease)
                }) {
                Ok(())
            } else {
                Err(TransactionError::InvalidState(
                    "Nix policy recovery inspection admission",
                ))
            };
        }
        #[cfg(target_os = "linux")]
        if let EffectRequest::Filesystem(FilesystemJob::VerifyExistingNixPolicy { lease, .. }, _) =
            request
        {
            self.checkpoint()?;
            let state = self.state.borrow();
            return if matches!(self.admission, Admission::Requested)
                && matches!(state.review, ReviewGate::Accepted { .. })
                && state.lease.as_ref().is_some_and(|owner| {
                    owner.policy_identity(&owner.path).ok().as_ref() == Some(lease)
                }) {
                Ok(())
            } else {
                Err(TransactionError::InvalidState(
                    "existing Nix policy admission",
                ))
            };
        }
        #[cfg(target_os = "linux")]
        if let Some((layout, operation)) = request.policy_intent_operation() {
            self.checkpoint()?;
            let state = self.state.borrow();
            let policy_intent_admitted = match (&state.review, operation) {
                #[cfg(test)]
                (ReviewGate::WritingPolicyIntent, PolicyIntentOperation::Publish) => true,
                (ReviewGate::ClearingPolicyIntent, PolicyIntentOperation::Clear) => true,
                _ => false,
            };
            return if matches!(self.admission, Admission::Requested)
                && policy_intent_admitted
                && state
                    .lease
                    .as_ref()
                    .is_some_and(|lease| lease.policy_identity(&layout.lock()).is_ok())
            {
                Ok(())
            } else {
                Err(TransactionError::InvalidState(
                    "Nix policy intent admission",
                ))
            };
        }
        match &self.admission {
            Admission::Requested if self.stopped() => Err(TransactionError::StopRequested),
            Admission::Requested => {
                let state = self.state.borrow();
                match state.review {
                    ReviewGate::Accepted { .. } => Ok(()),
                    #[cfg(target_os = "linux")]
                    ReviewGate::AcceptedPolicyRecovery
                    | ReviewGate::AcceptedPolicyResumption { .. }
                        if !request.mutates() && !request.accepts_administrator_terminal() =>
                    {
                        Ok(())
                    }
                    ReviewGate::Inspecting | ReviewGate::Rechecking(_) if !request.mutates() => {
                        Ok(())
                    }
                    ReviewGate::Stopped(_) => Err(TransactionError::StopRequested),
                    _ => Err(TransactionError::InvalidState(
                        "effect requires accepted review",
                    )),
                }
            }
            Admission::Settlement(permit) => {
                let state = self.state.borrow();
                if !state.settlement.as_ref().is_some_and(|active| {
                    active.identity == permit.identity && active.kind == permit.kind
                }) {
                    return Err(TransactionError::InvalidState(
                        "settlement authority expired",
                    ));
                }
                match request {
                    EffectRequest::Filesystem(job, _) if job.permits_settlement(permit.kind) => {
                        Ok(())
                    }
                    _ => Err(TransactionError::InvalidState(
                        "effect is outside settlement",
                    )),
                }
            }
        }
    }

    pub(super) async fn filesystem(
        &self,
        job: FilesystemJob,
    ) -> Result<FilesystemResult, TransactionError> {
        let request = EffectRequest::Filesystem(job, self.admission.clone());
        match (EffectFuture {
            effects: self.clone(),
            request: Some(request),
            admitted: false,
        })
        .await?
        {
            EffectResult::Filesystem(FilesystemResult::JournalPublication {
                kind,
                path,
                before,
                bytes,
                result,
            }) => {
                let mut state = self.state.borrow_mut();
                let ReviewGate::Accepted { identity, .. } = state.review else {
                    return Err(TransactionError::InvalidState(
                        "journal publication without accepted review",
                    ));
                };
                if state
                    .lease
                    .as_ref()
                    .and_then(|lease| lease.path.parent())
                    .map(|parent| parent.join(super::STATE_NAME))
                    .as_ref()
                    != Some(&path)
                {
                    return Err(TransactionError::InvalidState(
                        "journal publication is outside the owned record",
                    ));
                }
                state.journal_attempt = Some(JournalAttempt {
                    review: identity,
                    kind,
                    path,
                    before,
                    bytes,
                    publication: result.clone(),
                });
                result.map(|_| FilesystemResult::Applied)
            }
            EffectResult::Filesystem(value) => Ok(value),
            _ => Err(TransactionError::InvalidState("filesystem completion type")),
        }
    }

    pub(super) async fn process(
        &self,
        request: process::WorkerRequest,
    ) -> Result<process::OperationResult, TransactionError> {
        match (EffectFuture {
            effects: self.clone(),
            request: Some(EffectRequest::Process(request)),
            admitted: false,
        })
        .await?
        {
            EffectResult::Process(value) => Ok(value),
            _ => Err(TransactionError::InvalidState("process completion type")),
        }
    }

    pub(super) fn settlement(&self, kind: SettlementKind) -> Result<Self, TransactionError> {
        if kind == SettlementKind::Recovery {
            return Err(TransactionError::InvalidState(
                "recovery requires its first reserved mutation",
            ));
        }
        if kind != SettlementKind::Observation {
            let mut state = self.state.borrow_mut();
            let ReviewGate::Accepted {
                identity,
                progress: AcceptedProgress::MutationAdmitted,
            } = state.review
            else {
                return Err(TransactionError::InvalidState(
                    "settlement requires admitted mutation",
                ));
            };
            if !state.journal_attempt.as_ref().is_some_and(|attempt| {
                attempt.kind == kind && attempt.review == identity && attempt.publication.is_ok()
            }) {
                return Err(TransactionError::InvalidState(
                    "settlement requires its successful journal publication",
                ));
            }
            state.journal_attempt = None;
        }
        self.issue_settlement(kind)
    }

    fn issue_settlement(&self, kind: SettlementKind) -> Result<Self, TransactionError> {
        let mut state = self.state.borrow_mut();
        #[cfg(target_os = "linux")]
        if matches!(
            state.review,
            ReviewGate::AcceptedPolicyRecovery | ReviewGate::AcceptedPolicyResumption { .. }
        ) {
            return Err(TransactionError::InvalidState(
                "settlement is outside Nix policy recovery",
            ));
        }
        if state.settlement.is_some() || !matches!(state.slot, EffectSlot::Idle) {
            return Err(TransactionError::InvalidState("settlement already pending"));
        }
        state.next_identity = state
            .next_identity
            .checked_add(1)
            .ok_or(TransactionError::InvalidState("owner identity exhausted"))?;
        let permit = SettlementPermit {
            identity: state.next_identity,
            kind,
            failed_journal: None,
        };
        state.settlement = Some(permit.clone());
        Ok(Self {
            state: self.state.clone(),
            cancellation: self.cancellation.clone(),
            admission: Admission::Settlement(permit),
        })
    }

    pub(super) async fn recover_failed_publication(
        &self,
        kind: SettlementKind,
        layout: &Layout,
    ) -> Result<Self, TransactionError> {
        let attempt = {
            let mut state = self.state.borrow_mut();
            let ReviewGate::Accepted {
                identity,
                progress: AcceptedProgress::MutationAdmitted,
            } = state.review
            else {
                return Err(TransactionError::InvalidState(
                    "failed publication was not admitted",
                ));
            };
            if !state.journal_attempt.as_ref().is_some_and(|attempt| {
                attempt.kind == kind
                    && attempt.review == identity
                    && attempt.publication.is_err()
                    && attempt.path == layout.record()
            }) {
                return Err(TransactionError::InvalidState(
                    "failed journal identity is unavailable",
                ));
            }
            state.journal_attempt.take().unwrap()
        };
        let observation = self.settlement(SettlementKind::Observation)?;
        let observed = observation
            .filesystem(FilesystemJob::ReadRecord(layout.clone()))
            .await;
        observation.finish_settlement()?;
        let FilesystemResult::Bytes(current) = observed? else {
            return Err(TransactionError::InvalidState(
                "journal observation completion type",
            ));
        };
        if current.as_ref() != Some(&attempt.bytes) {
            if current != attempt.before {
                return Err(TransactionError::InvalidState(
                    "failed journal changed before recovery",
                ));
            }
            if let Some(bytes) = &current {
                let before: super::InstallationRecord =
                    serde_json::from_slice(bytes).map_err(|_| {
                        TransactionError::InvalidState("failed journal prior state is invalid")
                    })?;
                if before.uninstall.is_some()
                    || before.activation.is_some()
                    || before.resources.pending.is_some()
                    || before
                        .shell
                        .as_ref()
                        .is_some_and(super::shell::Ledger::pending)
                {
                    return Err(TransactionError::InvalidState(
                        "failed journal prior state has unrelated recovery",
                    ));
                }
            }
        }
        let mut recovery = self.issue_settlement(kind)?;
        let Admission::Settlement(permit) = &mut recovery.admission else {
            unreachable!();
        };
        permit.failed_journal = Some((attempt.before, attempt.bytes));
        self.state.borrow_mut().settlement = Some(permit.clone());
        Ok(recovery)
    }

    pub(super) fn check_recovery_preimage(
        &self,
        bytes: Option<&[u8]>,
    ) -> Result<(), TransactionError> {
        if let Admission::Settlement(permit) = &self.admission {
            if let Some((before, pending)) = &permit.failed_journal {
                if bytes != before.as_deref() && bytes != Some(pending.as_slice()) {
                    return Err(TransactionError::InvalidState(
                        "failed journal changed during recovery inspection",
                    ));
                }
            }
        }
        Ok(())
    }

    pub(super) fn finish_settlement(&self) -> Result<(), TransactionError> {
        let Admission::Settlement(permit) = &self.admission else {
            return Err(TransactionError::InvalidState(
                "settlement was not admitted",
            ));
        };
        let mut state = self.state.borrow_mut();
        if !matches!(state.slot, EffectSlot::Idle)
            || !state
                .settlement
                .as_ref()
                .is_some_and(|active| active.identity == permit.identity)
        {
            return Err(TransactionError::InvalidState("settlement is not complete"));
        }
        if permit.kind == SettlementKind::Recovery {
            state
                .diagnostics
                .completed(crate::diagnostics::CompletedStep::Recovery);
            state
                .diagnostics
                .applied(crate::diagnostics::AppliedChange::RecoveryRepairsSettled);
        }
        state.settlement = None;
        Ok(())
    }

    pub(super) fn checkpoint(&self) -> Result<(), TransactionError> {
        if matches!(self.admission, Admission::Requested) && self.stopped() {
            Err(TransactionError::StopRequested)
        } else {
            Ok(())
        }
    }

    #[cfg(test)]
    fn advance_effect(&self) {
        self.advance_effect_with_terminal(None);
    }

    fn administrator_pending(&self) -> bool {
        let state = self.state.borrow();
        let EffectSlot::Admitted(request) = &state.slot else {
            return false;
        };
        if !request.accepts_administrator_terminal() || state.lease.is_none() || self.stopped() {
            return false;
        }
        #[cfg(target_os = "linux")]
        if matches!(
            request,
            EffectRequest::Filesystem(FilesystemJob::InspectStockPolicy(_), _)
        ) {
            return matches!(state.review, ReviewGate::AcceptedPolicyRecovery);
        }
        #[cfg(target_os = "linux")]
        if let EffectRequest::Filesystem(FilesystemJob::ResumeStockPolicy(intent), _) = request {
            return matches!(&state.review, ReviewGate::AcceptedPolicyResumption { pending, .. } if pending == intent.pending());
        }
        matches!(state.review, ReviewGate::Accepted { .. })
    }

    fn advance_effect_with_terminal(&self, terminal: Option<ParkedTerminal>) {
        let attended = terminal.is_some();
        if attended && !self.administrator_pending() {
            let error = if self.stopped() {
                TransactionError::StopRequested
            } else {
                TransactionError::InvalidState(
                    "administrator terminal without accepted review and lease",
                )
            };
            self.state.borrow_mut().slot = EffectSlot::AdministratorFinished(Err(error));
            return;
        }
        let slot = {
            let mut state = self.state.borrow_mut();
            std::mem::replace(&mut state.slot, EffectSlot::Idle)
        };
        let next = match slot {
            EffectSlot::Admitted(request) => {
                #[cfg(not(target_os = "linux"))]
                let mutation_allowed = self.mutation_admitted();
                #[cfg(target_os = "linux")]
                let mutation_allowed = match &request {
                    EffectRequest::Filesystem(FilesystemJob::InspectStockPolicy(_), _) => {
                        self.check_admission(&request).is_ok()
                    }
                    _ => self.mutation_admitted() || request.policy_intent_operation().is_some(),
                };
                let permission = if request.mutates() && mutation_allowed {
                    Some(MutationPermission { _private: () })
                } else {
                    None
                };
                let active = match request {
                    EffectRequest::Filesystem(job, admission) => {
                        let lease = self
                            .state
                            .borrow()
                            .lease
                            .as_ref()
                            .map(|lease| lease.file.try_clone())
                            .transpose();
                        match lease {
                            Ok(lease) => FilesystemTask::start(
                                job,
                                lease,
                                self.cancellation.clone(),
                                admission,
                                permission,
                                terminal,
                            )
                            .map(ActiveEffect::Filesystem),
                            Err(_) => Err(TransactionError::Io),
                        }
                    }
                    EffectRequest::Process(request) => {
                        if request.mutates() && permission.is_none() {
                            self.state.borrow_mut().slot = EffectSlot::Completed(Err(
                                TransactionError::InvalidState("process requires accepted review"),
                            ));
                            return;
                        }
                        let retained = {
                            let state = self.state.borrow();
                            state
                                .lease
                                .as_ref()
                                .ok_or(TransactionError::InvalidState("process before lease"))
                                .and_then(|lease| {
                                    Ok(RootLease {
                                        file: lease.file.try_clone().map_err(|_| {
                                            TransactionError::Verification("worker admission")
                                        })?,
                                        path: lease.path.clone(),
                                    })
                                })
                        };
                        let started = retained.and_then(|lease| {
                            process::OperationOwner::begin(&lease)
                                .map_err(|_| TransactionError::Verification("worker admission"))
                        });
                        started.map(|owner| ActiveEffect::Process {
                            owner,
                            request: Some(request),
                            stop: ProcessStop::Running,
                        })
                    }
                };
                match active {
                    Ok(active) => EffectSlot::Active(active),
                    Err(error) if attended => EffectSlot::AdministratorFinished(Err(error)),
                    Err(error) => EffectSlot::Completed(Err(error)),
                }
            }
            EffectSlot::Active(mut active) => {
                let attended =
                    matches!(&active, ActiveEffect::Filesystem(task) if task.uses_terminal());
                let result = match &mut active {
                    ActiveEffect::Filesystem(task) => task
                        .poll()
                        .map(|result| result.map(EffectResult::Filesystem)),
                    ActiveEffect::Process {
                        owner,
                        request,
                        stop,
                    } => {
                        if self.stopped() && matches!(stop, ProcessStop::Running) {
                            owner.request_cancel();
                            *stop = ProcessStop::Cancellation;
                        }
                        match owner.poll() {
                            process::ProcessPoll::Pending => None,
                            process::ProcessPoll::Ready => {
                                if matches!(stop, ProcessStop::Running) {
                                    if let Some(request) = request.take() {
                                        if owner.submit(request).is_err() {
                                            owner.request_cancel();
                                            *stop = ProcessStop::SubmissionFailure;
                                        }
                                    }
                                }
                                None
                            }
                            process::ProcessPoll::ContainmentLost(error) => {
                                drop(error);
                                Some(Err(TransactionError::CleanupUnknown))
                            }
                            process::ProcessPoll::Quiescent(_)
                                if matches!(stop, ProcessStop::SubmissionFailure) =>
                            {
                                Some(Err(TransactionError::Verification("worker submission")))
                            }
                            process::ProcessPoll::Quiescent(result) => Some(match result {
                                Ok(result) if result.failure.is_none() => {
                                    Ok(EffectResult::Process(result))
                                }
                                Ok(_) => Err(TransactionError::Verification("client containment")),
                                Err(error)
                                    if error.kind() == std::io::ErrorKind::Interrupted
                                        && matches!(stop, ProcessStop::Cancellation) =>
                                {
                                    Err(TransactionError::StopRequested)
                                }
                                Err(_) if request.is_some() => {
                                    Err(TransactionError::Verification("worker admission"))
                                }
                                Err(_) => Err(TransactionError::Verification("worker protocol")),
                            }),
                        }
                    }
                };
                match result {
                    Some(result) if attended => EffectSlot::AdministratorFinished(result),
                    Some(result) => {
                        drop(active);
                        EffectSlot::Completed(result)
                    }
                    None => EffectSlot::Active(active),
                }
            }
            other => other,
        };
        self.state.borrow_mut().slot = next;
    }

    fn wait(&self) {
        let interest = {
            let state = self.state.borrow();
            match &state.slot {
                EffectSlot::Active(ActiveEffect::Filesystem(task)) => Some(task.interest()),
                EffectSlot::Active(ActiveEffect::Process { owner, .. }) => Some(owner.interest()),
                _ => None,
            }
        };
        if let Some(interest) = interest {
            interest.wait();
        }
    }
}

pub(super) struct WaitInterest {
    pub(super) descriptor: Option<(std::os::fd::RawFd, libc::c_short)>,
    pub(super) deadline: Instant,
}

impl WaitInterest {
    fn timeout_millis(&self, now: Instant) -> i32 {
        self.deadline
            .saturating_duration_since(now)
            .as_nanos()
            .div_ceil(1_000_000)
            .min(10) as i32
    }

    pub(super) fn wait(&self) {
        let milliseconds = self.timeout_millis(Instant::now());
        if let Some((fd, events)) = self.descriptor {
            let mut descriptor = libc::pollfd {
                fd,
                events,
                revents: 0,
            };
            unsafe {
                libc::poll(&mut descriptor, 1, milliseconds);
            }
        } else {
            std::thread::sleep(Duration::from_millis(milliseconds as u64));
        }
    }
}

struct CallerWake;
impl Wake for CallerWake {
    fn wake(self: Arc<Self>) {}
}

pub(super) struct Coordinator {
    effects: Effects,
    continuation: Pin<Box<dyn Future<Output = Result<RunOutcome, TransactionError>>>>,
}

impl Coordinator {
    pub(super) fn new(
        request: snapshot::Request,
        executor: runtime::Executor,
        layout: Option<Layout>,
    ) -> Self {
        let effects = Effects::new(Cancellation::default());
        let continuation = Box::pin(lifecycle(effects.clone(), request, executor, layout));
        Self {
            effects,
            continuation,
        }
    }

    pub(super) fn request_cancel(&self) {
        self.effects.cancellation.request();
        self.effects.state.borrow_mut().diagnostics.cancelled();
        self.effects.observe(super::observation::Activity::Stopping);
    }

    pub(super) fn take_review(&mut self) -> Option<ReviewOffer> {
        self.effects.take_review()
    }

    pub(super) fn answer(
        &mut self,
        offer: ReviewOffer,
        decision: ReviewDecision,
    ) -> Result<(), TransactionError> {
        self.effects.answer(
            offer.token,
            match decision {
                ReviewDecision::Accept => None,
                ReviewDecision::Decline => Some(UnadmittedReason::Declined),
                ReviewDecision::ConsentRequired => Some(UnadmittedReason::ConsentRequired),
            },
        )?;
        self.effects.observe(match decision {
            ReviewDecision::Accept => super::observation::Activity::Rechecking,
            _ => super::observation::Activity::Declined,
        });
        Ok(())
    }

    pub(super) fn diagnostics(&self) -> crate::diagnostics::InvocationFacts {
        self.effects.state.borrow().diagnostics.snapshot()
    }

    pub(super) fn presentation(
        &self,
    ) -> std::cell::Ref<'_, super::observation::PresentationObservation> {
        std::cell::Ref::map(self.effects.state.borrow(), |state| &state.observation)
    }

    pub(super) fn interest(&self) -> OwnerInterest<'_> {
        let state = self.effects.state.borrow();
        let active = match &state.slot {
            EffectSlot::Active(ActiveEffect::Filesystem(task)) => Some(task.interest()),
            EffectSlot::Active(ActiveEffect::Process { owner, .. }) => Some(owner.interest()),
            _ => None,
        };
        if let Some(interest) = active {
            return OwnerInterest::Waiting {
                descriptor: interest.descriptor.map(|(fd, events)| {
                    (unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) }, events)
                }),
                deadline: Some(interest.deadline),
            };
        }
        if matches!(state.slot, EffectSlot::Idle)
            && matches!(state.review, ReviewGate::Awaiting { .. })
            && !self.effects.stopped()
        {
            OwnerInterest::Waiting {
                descriptor: None,
                deadline: None,
            }
        } else {
            OwnerInterest::Ready
        }
    }

    pub(super) fn advance(&mut self) -> Poll<Result<RunOutcome, TransactionError>> {
        self.advance_with_terminal(None)
    }

    pub(super) fn administrator_pending(&self) -> bool {
        self.effects.administrator_pending()
    }

    pub(super) fn administrator_running(&self) -> bool {
        matches!(
            &self.effects.state.borrow().slot,
            EffectSlot::Active(ActiveEffect::Filesystem(task)) if task.uses_terminal()
        )
    }

    pub(super) fn administrator_finished(&self) -> Option<bool> {
        let state = self.effects.state.borrow();
        if let EffectSlot::AdministratorFinished(result) = &state.slot {
            Some(!matches!(result, Err(TransactionError::CleanupUnknown)))
        } else {
            None
        }
    }

    pub(super) fn complete_administrator(&self) {
        let mut state = self.effects.state.borrow_mut();
        let slot = std::mem::replace(&mut state.slot, EffectSlot::Idle);
        state.slot = match slot {
            EffectSlot::AdministratorFinished(result) => EffectSlot::Completed(result),
            other => other,
        };
    }

    pub(super) fn advance_with_terminal(
        &mut self,
        terminal: Option<ParkedTerminal>,
    ) -> Poll<Result<RunOutcome, TransactionError>> {
        #[cfg(test)]
        super::dashboard::note_advance();
        self.effects.advance_effect_with_terminal(terminal);
        let waker = Waker::from(Arc::new(CallerWake));
        let mut context = Context::from_waker(&waker);
        match self.continuation.as_mut().poll(&mut context) {
            Poll::Ready(result) => {
                if !matches!(self.effects.state.borrow().slot, EffectSlot::Idle) {
                    return Poll::Ready(Err(TransactionError::InvalidState(
                        "owner finished with an effect",
                    )));
                }
                let mut state = self.effects.state.borrow_mut();
                state.observation.finish(&result);
                let activity = state.observation.activity;
                state.diagnostics.activity(activity);
                if self.effects.stopped() {
                    state.diagnostics.cancelled();
                }
                Poll::Ready(result)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(test)]
pub(super) fn drive_plain(
    coordinator: &mut Coordinator,
    consent: bool,
    output: &mut impl std::io::Write,
) -> Result<RunOutcome, TransactionError> {
    loop {
        if let Poll::Ready(result) = coordinator.advance() {
            return result;
        }
        if let Some(offer) = coordinator.take_review() {
            crate::write_review(output, &offer.summary)
                .and_then(|_| output.flush())
                .map_err(|_| TransactionError::Io)?;
            coordinator.answer(
                offer,
                if consent {
                    ReviewDecision::Accept
                } else {
                    ReviewDecision::ConsentRequired
                },
            )?;
        }
        coordinator.effects.wait();
    }
}

async fn lifecycle(
    effects: Effects,
    request: snapshot::Request,
    executor: runtime::Executor,
    layout: Option<Layout>,
) -> Result<RunOutcome, TransactionError> {
    let startup = effects.filesystem(FilesystemJob::Startup(layout)).await;
    let (layout, lease) = match startup {
        Ok(FilesystemResult::Startup { layout, lease }) => (layout, lease),
        Err(TransactionError::StopRequested) => {
            return unadmitted(&effects);
        }
        Err(error) => return Err(error),
        _ => return Err(TransactionError::InvalidState("startup completion type")),
    };
    effects.state.borrow_mut().lease = Some(lease);
    let owner = InstallationOwner {
        layout,
        effects: effects.clone(),
        #[cfg(test)]
        fixture: false,
    };
    let result = owner.run_owned(request, executor).await;
    match result {
        Ok(outcome) if !effects.stopped() => Ok(RunOutcome::Complete(outcome)),
        Ok(_) | Err(TransactionError::StopRequested) => {
            #[cfg(target_os = "linux")]
            if effects.policy_resumption_admitted() {
                return stopped(&effects, StopObservation::PolicyRecoveryInterrupted);
            }
            if !effects.mutation_admitted() {
                return unadmitted(&effects);
            }
            let observation = owner.observe_stopped().await?;
            stopped(&effects, observation)
        }
        Err(error) => Err(error),
    }
}

fn unadmitted(effects: &Effects) -> Result<RunOutcome, TransactionError> {
    let mut state = effects.state.borrow_mut();
    if !matches!(state.slot, EffectSlot::Idle) || state.settlement.is_some() {
        return Err(TransactionError::InvalidState(
            "unadmitted operation has live work",
        ));
    }
    let reason = match state.review {
        ReviewGate::Stopped(reason) => reason,
        _ => UnadmittedReason::Cancelled,
    };
    state.review = ReviewGate::Stopped(reason);
    Ok(RunOutcome::Unadmitted(UnadmittedOperation {
        reason,
        recovery: state.recovery,
        _quiescent: Quiescent,
    }))
}

fn stopped(
    effects: &Effects,
    observation: StopObservation,
) -> Result<RunOutcome, TransactionError> {
    let mut state = effects.state.borrow_mut();
    if !matches!(state.slot, EffectSlot::Idle) || state.settlement.is_some() {
        return Err(TransactionError::InvalidState(
            "stopping has unsettled work",
        ));
    }
    state
        .diagnostics
        .completed(crate::diagnostics::CompletedStep::FinalSettlement);
    Ok(RunOutcome::Stopped(StoppedOperation {
        observation,
        _quiescent: Quiescent,
    }))
}

#[cfg(test)]
impl Effects {
    pub(super) fn for_tests() -> Self {
        Self::new(Cancellation::default())
    }
    pub(super) fn cancel_for_test(&self) {
        self.cancellation.request();
    }
    pub(super) fn accept_fixture_review(&self) {
        let mut review = Box::pin(self.review(ReviewSummary {
            operation: ReviewOperation::Recover,
            version: None,
            recovery: RecoveryPending::None,
            changes: vec![],
        }));
        let waker = Waker::from(Arc::new(CallerWake));
        let mut context = Context::from_waker(&waker);
        assert!(review.as_mut().poll(&mut context).is_pending());
        let offer = self.take_review().unwrap();
        self.answer(offer.token, None).unwrap();
        let Poll::Ready(Ok(identity)) = review.as_mut().poll(&mut context) else {
            panic!("fixture review not accepted");
        };
        self.admit_reviewed(identity).unwrap();
    }
    pub(super) fn cancel_at(&self, point: &'static str) {
        *self.cancellation.cut.lock().unwrap() = Some(point);
    }
}

#[cfg(all(test, target_os = "linux"))]
#[test]
fn administrator_terminal_requires_review_owned_lease_and_live_effect() {
    let effects = Effects::for_tests();
    let job = || {
        EffectSlot::Admitted(EffectRequest::Filesystem(
            FilesystemJob::VerifyExistingNixPolicy {
                inspector: super::filesystem::ExistingNixInspector::Installed(
                    crate::candidate::OutputPath::parse(
                        "/nix/store/00000000000000000000000000000000-neo",
                    )
                    .unwrap(),
                ),
                lease: super::PolicyStateLease {
                    device: 1,
                    inode: 2,
                },
                stamp: [1; 32],
            },
            Admission::Requested,
        ))
    };
    effects.state.borrow_mut().slot = job();
    assert!(!effects.administrator_pending());
    effects.state.borrow_mut().slot = EffectSlot::Idle;
    effects.accept_fixture_review();
    effects.state.borrow_mut().slot = job();
    assert!(!effects.administrator_pending());
    let temp = tempfile::tempdir().unwrap();
    effects.state.borrow_mut().lease =
        Some(RootLease::acquire(&temp.path().join("owner")).unwrap());
    assert!(effects.administrator_pending());
    effects.cancel_for_test();
    assert!(!effects.administrator_pending());
}

#[cfg(test)]
pub(super) fn test_with<F: Future>(effects: &Effects, lease: &RootLease, future: F) -> F::Output {
    assert!(effects.state.borrow().lease.is_none());
    effects.state.borrow_mut().lease = Some(RootLease {
        file: lease.file.try_clone().unwrap(),
        path: lease.path.clone(),
    });
    let mut future = Box::pin(future);
    let waker = Waker::from(Arc::new(CallerWake));
    let mut context = Context::from_waker(&waker);
    let result = loop {
        effects.advance_effect();
        if let Poll::Ready(value) = future.as_mut().poll(&mut context) {
            break value;
        }
        effects.wait();
    };
    assert!(matches!(effects.state.borrow().slot, EffectSlot::Idle));
    effects.state.borrow_mut().lease.take();
    result
}

#[cfg(test)]
pub(super) fn test_with_review<F: Future>(
    effects: &Effects,
    lease: &RootLease,
    future: F,
) -> F::Output {
    test_with_review_signal(effects, lease, None, future)
}

#[cfg(test)]
pub(super) fn test_with_review_signal<F: Future>(
    effects: &Effects,
    lease: &RootLease,
    offered: Option<&AtomicBool>,
    future: F,
) -> F::Output {
    let mut future = Box::pin(future);
    test_with(
        effects,
        lease,
        std::future::poll_fn(|context| {
            let result = future.as_mut().poll(context);
            if let Some(offer) = effects.take_review() {
                if let Some(offered) = offered {
                    offered.store(true, Ordering::Release);
                }
                effects.answer(offer.token, None).unwrap();
            }
            result
        }),
    )
}

#[cfg(test)]
mod tests {
    use super::super::{InstallSelection, Operation};
    use super::*;
    use std::fs;
    use std::os::unix::fs::PermissionsExt;

    fn setup() -> (tempfile::TempDir, InstallationOwner, RootLease) {
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
        let layout = Layout::new(root.clone(), root.join("bin"));
        fs::create_dir(&layout.command_directory).unwrap();
        let lease = RootLease::acquire(&layout.lock()).unwrap();
        let owner = InstallationOwner::new(layout);
        owner.effects.accept_fixture_review();
        (temporary, owner, lease)
    }

    fn poll<F: Future>(effects: &Effects, future: Pin<&mut F>) -> Poll<F::Output> {
        effects.advance_effect();
        let waker = Waker::from(Arc::new(CallerWake));
        future.poll(&mut Context::from_waker(&waker))
    }

    fn retain(effects: &Effects, lease: &RootLease) {
        effects.state.borrow_mut().lease = Some(RootLease {
            file: lease.file.try_clone().unwrap(),
            path: lease.path.clone(),
        });
    }

    fn drive<F: Future>(effects: &Effects, future: F) -> F::Output {
        let mut future = Box::pin(future);
        loop {
            if let Poll::Ready(value) = poll(effects, future.as_mut()) {
                return value;
            }
            effects.wait();
        }
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn pending_policy_recovery_review_only_admits_the_unchanged_inspection() {
        for replace_record in [false, true] {
            let (_temporary, owner, lease) = setup();
            let pending = super::super::PendingNixPolicy {
                operation: PolicyOperation::ApplyUnchanged,
                attempt: [1; 32],
                reviewed: [2; 32],
                helper_path: "/nix/store/11111111111111111111111111111111-neo-policy-helper".into(),
                helper: crate::candidate::FileIdentity {
                    name: "neo-installer".into(),
                    sha: "a".repeat(64),
                    size: 1,
                },
                state_lease: Some(lease.policy_identity(&owner.layout.lock()).unwrap()),
            };
            let record = super::super::InstallationRecord {
                pending_nix_policy: Some(pending.clone()),
                ..Default::default()
            };
            let expected = owner.encode_record(&record).unwrap();
            let path = owner.layout.record();
            fs::write(&path, &expected).unwrap();
            fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
            let effects = &owner.effects;
            retain(effects, &lease);
            let open = || {
                StockPolicyIntent::open(effects, &owner.layout, pending.clone(), expected.clone())
                    .unwrap()
            };
            let request = EffectRequest::Filesystem(
                FilesystemJob::InspectStockPolicy(open()),
                Admission::Requested,
            );
            assert!(request.mutates());
            assert!(effects.check_admission(&request).is_err());
            effects.state.borrow_mut().review = ReviewGate::Inspecting;
            assert!(effects.check_admission(&request).is_err());

            let mut inspection = Box::pin(
                effects.review_and_inspect_stock_policy(open(), std::future::ready(Ok(()))),
            );
            assert!(poll(effects, inspection.as_mut()).is_pending());
            let offer = effects.take_review().unwrap();
            assert_eq!(offer.summary.operation, ReviewOperation::Recover);
            assert_eq!(
                offer.summary.changes,
                [ReviewedChange::NixPolicyRecoveryInspection]
            );
            assert!(!effects.administrator_pending());
            effects.answer(offer.token, None).unwrap();
            if replace_record {
                fs::write(&path, b"replacement").unwrap();
                assert!(matches!(
                    poll(effects, inspection.as_mut()),
                    Poll::Ready(Err(TransactionError::StopRequested))
                ));
                assert!(!effects.administrator_pending());
                assert_eq!(fs::read(&path).unwrap(), b"replacement");
                continue;
            }

            assert!(poll(effects, inspection.as_mut()).is_pending());
            assert!(effects.administrator_pending());
            effects.state.borrow_mut().slot = EffectSlot::Idle;
            let read = EffectRequest::Filesystem(
                FilesystemJob::ReadRecord(owner.layout.clone()),
                Admission::Requested,
            );
            assert!(effects.check_admission(&read).is_ok());
            let install = EffectRequest::Filesystem(
                FilesystemJob::EnsureCommandDirectory(owner.layout.command_directory.clone()),
                Admission::Requested,
            );
            assert!(effects.check_admission(&install).is_err());
            let write = EffectRequest::Filesystem(
                FilesystemJob::ClearPolicyIntent {
                    layout: owner.layout.clone(),
                    expected: expected.clone(),
                },
                Admission::Requested,
            );
            assert!(effects.check_admission(&write).is_err());
            let update_inspector = EffectRequest::Filesystem(
                FilesystemJob::VerifyExistingNixPolicy {
                    inspector: super::super::filesystem::ExistingNixInspector::Installed(
                        crate::candidate::OutputPath::parse(
                            "/nix/store/00000000000000000000000000000000-neo",
                        )
                        .unwrap(),
                    ),
                    lease: pending.state_lease.unwrap(),
                    stamp: [0; 32],
                },
                Admission::Requested,
            );
            assert!(effects.check_admission(&update_inspector).is_err());
            assert!(effects.settlement(SettlementKind::Observation).is_err());
            assert_eq!(fs::read(&path).unwrap(), expected);

            let resume = EffectRequest::Filesystem(
                FilesystemJob::ResumeStockPolicy(open()),
                Admission::Requested,
            );
            assert!(effects.check_admission(&resume).is_err());
            let mut foreign = pending.clone();
            foreign.attempt = [3; 32];
            effects.state.borrow_mut().review = ReviewGate::AcceptedPolicyResumption {
                pending: foreign,
                progress: AcceptedProgress::NoMutationAdmitted,
            };
            assert!(effects.check_admission(&resume).is_err());
            effects.state.borrow_mut().review = ReviewGate::AcceptedPolicyResumption {
                pending,
                progress: AcceptedProgress::NoMutationAdmitted,
            };
            assert!(!effects.mutation_admitted());
            assert!(effects.check_admission(&request).is_err());
            assert!(effects.check_admission(&install).is_err());
            assert!(effects.settlement(SettlementKind::Observation).is_err());
            let EffectRequest::Filesystem(job, _) = resume else {
                unreachable!();
            };
            let mut resumption = Box::pin(effects.filesystem(job));
            assert!(poll(effects, resumption.as_mut()).is_pending());
            assert!(effects.administrator_pending());
            assert!(effects.policy_resumption_admitted());
        }
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn prepared_policy_intent_persists_only_with_the_reviewed_record_and_lease() {
        for change in ["none", "record", "lease", "cancel"] {
            let (_temporary, owner, lease) = setup();
            let identity = match owner.effects.state.borrow().review {
                ReviewGate::Accepted { identity, .. } => identity,
                _ => panic!("fixture review was not accepted"),
            };
            let binding = PolicyReviewBinding {
                operation: PolicyOperation::ConventionalAppend,
                owner: Rc::downgrade(&owner.effects.state),
                identity,
            };
            let pending = super::super::PendingNixPolicy {
                operation: PolicyOperation::ConventionalAppend,
                attempt: [1; 32],
                reviewed: [2; 32],
                helper_path: "/nix/store/11111111111111111111111111111111-neo-policy-helper".into(),
                helper: crate::candidate::FileIdentity {
                    name: "neo-installer".into(),
                    sha: "a".repeat(64),
                    size: 1,
                },
                state_lease: None,
            };
            let prepared = test_with(
                &owner.effects,
                &lease,
                owner.prepare_stock_policy_intent(binding, pending),
            )
            .unwrap();
            let record = owner.layout.record();
            assert!(!record.exists());
            if change == "record" {
                fs::write(&record, b"replacement").unwrap();
                fs::set_permissions(&record, fs::Permissions::from_mode(0o600)).unwrap();
            }
            if change == "lease" {
                let held = lease.policy_identity(&owner.layout.lock()).unwrap();
                fs::rename(
                    owner.layout.lock(),
                    owner.layout.lock().with_extension("old"),
                )
                .unwrap();
                let replacement = RootLease::acquire(&owner.layout.lock()).unwrap();
                assert_ne!(
                    replacement.policy_identity(&owner.layout.lock()).unwrap(),
                    held
                );
            }
            if change == "cancel" {
                owner.effects.cancel_for_test();
            }
            let policy = super::super::filesystem::ReadPolicy::requested_for_test(
                owner.effects.cancellation.clone(),
            );
            let result = prepared.persist(&policy);
            match change {
                "none" => result.unwrap().verify().unwrap(),
                "record" => {
                    assert!(result.is_err());
                    assert_eq!(fs::read(record).unwrap(), b"replacement");
                }
                "lease" => {
                    assert!(result.is_err());
                    assert!(!record.exists());
                }
                "cancel" => {
                    assert!(matches!(result, Err(TransactionError::StopRequested)));
                    assert!(!record.exists());
                }
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn nix_policy_review_binds_exact_plan_and_owner_before_mutation() {
        for drift in [
            "none",
            "release",
            "helper",
            "source",
            "client",
            "service",
            "graph",
            "decline",
            "foreign",
            "foreign-drift",
            "generation",
        ] {
            let (_temporary, owner, lease) = setup();
            owner.effects.state.borrow_mut().review = ReviewGate::Inspecting;
            retain(&owner.effects, &lease);
            let path = owner.layout.state_directory.join("nix.conf");
            let original = b"max-jobs = 2\n!include optional.conf\n";
            fs::write(&path, original).unwrap();
            let plan = runtime::policy_review_fixture(&path, "none");
            let summary = ReviewSummary {
                operation: ReviewOperation::Install,
                version: Some("1.0.0".into()),
                recovery: RecoveryPending::None,
                changes: vec![ReviewedChange::Selection],
            };
            let mut review = Box::pin(owner.effects.review_nix_policy(plan, summary));
            assert!(poll(&owner.effects, review.as_mut()).is_pending());
            let offer = owner.effects.take_review().unwrap();
            assert_eq!(
                offer.summary.changes,
                vec![ReviewedChange::Selection, ReviewedChange::NixPolicy]
            );
            let mut rendered = Vec::new();
            crate::write_review(&mut rendered, &offer.summary).unwrap();
            let rendered = String::from_utf8(rendered).unwrap();
            assert!(rendered.contains("/etc/nix/nix.conf"));
            assert!(rendered.contains("https://cache.iog.io"));
            assert!(rendered.contains("https://neohaskell.cachix.org"));
            assert!(!rendered.contains("max-jobs"));
            assert!(!rendered.contains("optional.conf"));
            owner
                .effects
                .answer(
                    offer.token,
                    if drift == "decline" {
                        Some(UnadmittedReason::Declined)
                    } else {
                        None
                    },
                )
                .unwrap();
            let answer = drive(&owner.effects, review);
            if drift == "decline" {
                assert!(answer.is_err());
                assert!(!owner.effects.mutation_admitted());
                continue;
            }
            let (plan, mut binding) = answer.unwrap();
            assert!(!owner.effects.mutation_admitted());
            if drift == "graph" {
                fs::write(path.with_file_name("optional.conf"), b"max-jobs = 3\n").unwrap();
                assert_eq!(fs::read(&path).unwrap(), original);
            }
            let fresh = runtime::policy_review_fixture(
                &path,
                if drift == "foreign-drift" {
                    "helper"
                } else {
                    drift
                },
            );
            if matches!(drift, "foreign" | "foreign-drift") {
                let (_other_temp, other, _other_lease) = setup();
                assert!(runtime::test_policy_recheck(&other, plan, binding, fresh).is_err());
                assert!(matches!(
                    other.effects.state.borrow().review,
                    ReviewGate::Accepted { .. }
                ));
                continue;
            }
            if drift == "generation" {
                binding.identity += 1;
                assert!(runtime::test_policy_recheck(&owner, plan, binding, fresh).is_err());
                assert!(matches!(
                    owner.effects.state.borrow().review,
                    ReviewGate::Rechecking(_)
                ));
                continue;
            }
            let checked = runtime::test_policy_recheck(&owner, plan, binding, fresh);
            if drift == "none" {
                let checked = checked.unwrap();
                let early = PolicyReviewBinding {
                    operation: PolicyOperation::ConventionalAppend,
                    owner: checked.owner.clone(),
                    identity: checked.identity,
                };
                assert!(owner.effects.consume_policy_review(early).is_err());
                owner.effects.admit_policy_review(&checked).unwrap();
                owner.effects.consume_policy_review(checked).unwrap();
            } else {
                assert!(checked.is_err(), "{drift}");
                assert!(matches!(
                    owner.effects.state.borrow().review,
                    ReviewGate::Stopped(UnadmittedReason::ReviewChanged)
                ));
            }
            assert!(!owner.effects.mutation_admitted());
        }
    }

    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "requires a disposable pinned daemon, registered exact test ELF and owned release fixture"]
    fn ordinary_install_reviews_policy_before_any_mutation() {
        run_ordinary_policy_review(runtime::PolicyFixtureMode::CompareOnly);
    }

    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "requires the explicitly opted-in disposable root daemon and registered release fixture"]
    fn reviewed_policy_publishes_source_application_pending() {
        assert_eq!(
            std::env::var("NEO_TEST_POLICY_PUBLISH").as_deref(),
            Ok("disposable-root-fixture")
        );
        assert_eq!(unsafe { libc::geteuid() }, 0);
        run_ordinary_policy_review(runtime::PolicyFixtureMode::PublishOnly);
    }

    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "requires the explicitly opted-in disposable root systemd fixture"]
    fn reviewed_policy_observes_managed_restart() {
        assert_eq!(
            std::env::var("NEO_TEST_POLICY_MANAGED_RESTART").as_deref(),
            Ok("disposable-root-fixture")
        );
        assert_eq!(unsafe { libc::geteuid() }, 0);
        run_ordinary_policy_review(runtime::PolicyFixtureMode::ManagedRestart);
    }

    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "requires UID/GID 1001 in the opted-in disposable sudo managed fixture"]
    fn ordinary_user_reviews_elevated_managed_policy() {
        assert!(matches!(
            std::env::var("NEO_TEST_ADMIN_MANAGED").as_deref(),
            Ok("accept" | "decline")
        ));
        assert_eq!(
            unsafe {
                (
                    libc::getuid(),
                    libc::geteuid(),
                    libc::getgid(),
                    libc::getegid(),
                )
            },
            (1001, 1001, 1001, 1001)
        );
        run_ordinary_policy_review(runtime::PolicyFixtureMode::SudoManagedRestart);
    }

    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "requires the original user state and exact retained sudo fixture helper"]
    fn ordinary_user_recovers_elevated_managed_policy() {
        use super::super::{runtime, snapshot};

        let expected = std::env::var("NEO_TEST_ADMIN_RECOVERY").unwrap();
        assert!(matches!(expected.as_str(), "applied" | "pending"));
        let owner = InstallationOwner::new(Layout::for_user().unwrap());
        let lease = owner.hold_lock_for_test().unwrap();
        let before = fs::read(owner.layout.record()).unwrap();
        let mut original = owner.decode_record(Some(&before)).unwrap();
        assert!(original.pending_nix_policy.is_some());
        owner.effects.state.borrow_mut().inspect_policy_fixture = true;
        let result = test_with(
            &owner.effects,
            &lease,
            owner.run_owned(
                snapshot::Request::RecoverOnly,
                runtime::Executor::NativeInstaller,
            ),
        );
        let after = fs::read(owner.layout.record()).unwrap();
        if expected == "applied" {
            assert!(matches!(
                result,
                Err(TransactionError::NixPolicyRecoveryCompleted)
            ));
            original.pending_nix_policy = None;
            assert_eq!(after, owner.encode_record(&original).unwrap());
        } else {
            assert!(matches!(
                result,
                Err(TransactionError::NixPolicyRecoveryPending)
            ));
            assert_eq!(after, before);
        }
        assert!(owner.effects.take_review().is_none());
        assert!(!owner.effects.mutation_admitted());
        assert!(!owner.layout.command_directory.exists());
        assert!(!owner.layout.state_directory.join("snapshots").exists());
        eprintln!("ordinary Nix policy recovery {expected}");
    }

    #[test]
    fn pending_nix_policy_blocks_every_owner_request() {
        let (_temporary, owner, lease) = setup();
        let mut record = super::super::InstallationRecord::default();
        let old = serde_json::to_vec(&record).unwrap();
        assert!(
            !std::str::from_utf8(&old)
                .unwrap()
                .contains("pending_nix_policy")
        );
        assert_eq!(owner.decode_record(Some(&old)).unwrap(), record);
        let helper_path = "/nix/store/i0md420ln0dwnjs0ibm8v6z2jkqpdaqi-neo-policy-helper";
        let held = owner.layout.command_directory.join("preserved");
        std::os::unix::fs::symlink(helper_path, &held).unwrap();
        record.pending_nix_policy = Some(super::super::PendingNixPolicy {
            operation: PolicyOperation::ConventionalAppend,
            attempt: [17; 32],
            reviewed: [19; 32],
            helper_path: helper_path.into(),
            helper: crate::candidate::FileIdentity {
                name: "neo-installer".into(),
                size: 123,
                sha: "ab".repeat(32),
            },
            state_lease: None,
        });
        let bytes = owner.encode_record(&record).unwrap();
        assert_eq!(owner.decode_record(Some(&bytes)).unwrap(), record);
        let mut unknown: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
        unknown["pending_nix_policy"]["unknown"] = true.into();
        assert!(
            owner
                .decode_record(Some(&serde_json::to_vec(&unknown).unwrap()))
                .is_err()
        );
        fs::write(owner.layout.record(), &bytes).unwrap();
        fs::set_permissions(owner.layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
        for request in [
            snapshot::Request::Install(None),
            snapshot::Request::Update(None),
            snapshot::Request::Rollback,
            snapshot::Request::RecoverOnly,
            snapshot::Request::Uninstall,
        ] {
            let fresh = InstallationOwner::new(owner.layout.clone());
            assert!(matches!(
                test_with(
                    &fresh.effects,
                    &lease,
                    fresh.run_owned(request, runtime::Executor::NativeInstaller,)
                ),
                Err(TransactionError::NixPolicyRecoveryPending)
            ));
            assert_eq!(fs::read(owner.layout.record()).unwrap(), bytes);
            assert_eq!(
                fs::read_link(&held).unwrap(),
                std::path::Path::new(helper_path)
            );
            assert!(fresh.effects.take_review().is_none());
        }
    }

    #[cfg(target_os = "linux")]
    fn run_ordinary_policy_review(fixture_mode: runtime::PolicyFixtureMode) {
        let publish = fixture_mode != runtime::PolicyFixtureMode::CompareOnly;
        use super::super::{nix_owner::NixBlock, runtime, snapshot};
        let pid = std::env::var("NEO_TEST_DAEMON_PID")
            .unwrap()
            .parse()
            .unwrap();
        let daemon =
            std::path::PathBuf::from(std::env::var_os("NEO_TEST_DAEMON_EXECUTABLE").unwrap());
        let helper =
            crate::candidate::OutputPath::parse(&std::env::var("NEO_TEST_PINNED_STORE").unwrap())
                .unwrap();
        let elevated = fixture_mode == runtime::PolicyFixtureMode::SudoManagedRestart;
        let elevated_answer = if elevated {
            std::env::var("NEO_TEST_ADMIN_MANAGED").unwrap()
        } else {
            String::new()
        };
        let modes: &[&str] = if elevated {
            if elevated_answer == "accept" {
                &["managed"]
            } else {
                &["decline"]
            }
        } else if fixture_mode == runtime::PolicyFixtureMode::ManagedRestart {
            &["managed"]
        } else if publish {
            &["cancel-before-intent", "uncertain-intent", "publish"]
        } else {
            &[
                "accept",
                "decline",
                "source-inode",
                "include",
                "refused",
                "helper-mismatch",
                "missing-authority",
            ]
        };
        for &mode in modes {
            let temporary = tempfile::tempdir().unwrap();
            let root = temporary.path().canonicalize().unwrap();
            fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
            let layout = if elevated
                && mode == "managed"
                && std::env::var("NEO_TEST_POLICY_OWNER_LAYOUT").as_deref() == Ok("normal")
            {
                Layout::for_user().unwrap()
            } else {
                Layout::new(root.clone(), root.join("bin"))
            };
            let owner = if mode == "missing-authority" {
                InstallationOwner::new(layout)
            } else {
                InstallationOwner::owned_fixture(layout)
            };
            let lease = RootLease::acquire(&owner.layout.lock()).unwrap();
            let nested = root.join("nested.conf");
            let (source, service) = if publish {
                (
                    "/etc/nix/nix.conf".into(),
                    "/etc/systemd/system/nix-daemon.service".into(),
                )
            } else {
                let source = root.join("nix.conf");
                let service = root.join("daemon.service");
                fs::write(&source, "include nested.conf\n").unwrap();
                fs::write(
                    &nested,
                    if mode == "refused" {
                        "include nix.conf\n"
                    } else {
                        "max-jobs = 1\n"
                    },
                )
                .unwrap();
                fs::write(&service, "fixed service fixture\n").unwrap();
                (source, service)
            };
            let original = fs::read(&source).unwrap();
            owner.effects.state.borrow_mut().install_policy_fixture =
                Some(runtime::InstallPolicyFixture {
                    source: source.clone(),
                    service,
                    daemon: daemon.clone(),
                    pid,
                    mode: fixture_mode,
                    helper: if mode == "helper-mismatch" {
                        crate::candidate::OutputPath::parse(
                            "/nix/store/i0md420ln0dwnjs0ibm8v6z2jkqpdaqi-unavailable-helper",
                        )
                        .unwrap()
                    } else {
                        helper.clone()
                    },
                });
            if mode == "cancel-before-intent" {
                owner.effects.cancel_at("before_policy_intent_publication");
            }
            if mode == "uncertain-intent" {
                owner
                    .effects
                    .cancel_at("after_policy_intent_publication_error");
            }
            let mut reviews = 0;
            let mut future = Box::pin(
                owner.run_owned(
                    snapshot::Request::Install(Some(
                        crate::identity::BuildIdentity::current()
                            .product_version
                            .into(),
                    )),
                    runtime::Executor::NativeInstaller,
                ),
            );
            let result = test_with(
                &owner.effects,
                &lease,
                std::future::poll_fn(|context| {
                    let result = future.as_mut().poll(context);
                    if let Some(offer) = owner.effects.take_review() {
                        reviews += 1;
                        assert_eq!(reviews, 1);
                        if elevated {
                            eprintln!("ordinary owner review {:?}", offer.summary.changes);
                        }
                        for change in [
                            ReviewedChange::Selection,
                            ReviewedChange::NixPolicy,
                            ReviewedChange::ShellPath,
                            ReviewedChange::ShellStartup,
                        ] {
                            assert!(offer.summary.changes.contains(&change));
                        }
                        assert!(!owner.effects.mutation_admitted());
                        if mode == "source-inode" {
                            fs::rename(&source, source.with_extension("old")).unwrap();
                            fs::write(&source, "include nested.conf\n").unwrap();
                        }
                        if mode == "include" {
                            fs::write(&nested, "max-jobs = 2\n").unwrap();
                        }
                        owner
                            .effects
                            .answer(
                                offer.token,
                                (mode == "decline").then_some(UnadmittedReason::Declined),
                            )
                            .unwrap();
                    }
                    result
                }),
            );
            assert_eq!(
                reviews,
                usize::from(matches!(
                    mode,
                    "managed"
                        | "publish"
                        | "accept"
                        | "decline"
                        | "source-inode"
                        | "include"
                        | "cancel-before-intent"
                        | "uncertain-intent"
                )),
                "{mode}"
            );
            match (mode, result) {
                (
                    "managed",
                    Err(TransactionError::Verification(
                        "policy source published; managed restart observed; readiness unproved",
                    )),
                ) => (),
                (
                    "publish",
                    Err(TransactionError::Verification(
                        "policy source published; daemon application pending",
                    )),
                ) => {
                    assert!(matches!(
                        owner.effects.state.borrow().review,
                        ReviewGate::Rechecking(_)
                    ));
                }
                (
                    "accept",
                    Err(TransactionError::Verification(
                        "policy application completion unavailable",
                    )),
                ) => (),
                ("decline" | "source-inode" | "include", Err(TransactionError::StopRequested)) => {
                    ()
                }
                ("cancel-before-intent", Err(TransactionError::StopRequested)) => (),
                ("uncertain-intent", Err(TransactionError::Io)) => (),
                (
                    "refused" | "missing-authority",
                    Err(TransactionError::NixBlocked(NixBlock::ConfigurationOwnerUnknown)),
                ) => (),
                (
                    "helper-mismatch",
                    Err(TransactionError::Verification("fixture helper fixed path")),
                ) => (),
                (_, result) => panic!("{mode}: unexpected outcome {:?}", result.err()),
            }
            assert!(!owner.effects.mutation_admitted());
            let facts = owner.effects.state.borrow().diagnostics.snapshot();
            assert!(
                !facts
                    .completed_steps
                    .contains(&crate::diagnostics::CompletedStep::Prerequisites)
            );
            assert!(
                !facts
                    .completed_steps
                    .contains(&crate::diagnostics::CompletedStep::Recovery)
            );
            let changes_policy = matches!(mode, "managed" | "publish");
            if changes_policy || mode == "uncertain-intent" {
                let bytes = fs::read(owner.layout.record()).unwrap();
                let pending = owner
                    .decode_record(Some(&bytes))
                    .unwrap()
                    .pending_nix_policy
                    .unwrap();
                assert_eq!(pending.helper_path, helper.as_str());
                assert_ne!(pending.reviewed, [0; 32]);
                for request in [
                    snapshot::Request::Install(None),
                    snapshot::Request::Uninstall,
                ] {
                    let fresh = InstallationOwner::new(owner.layout.clone());
                    assert!(matches!(
                        test_with_review(
                            &fresh.effects,
                            &lease,
                            fresh.run_owned(request, runtime::Executor::NativeInstaller,)
                        ),
                        Err(TransactionError::NixPolicyRecoveryPending)
                    ));
                    assert_eq!(fs::read(owner.layout.record()).unwrap(), bytes);
                }
            } else {
                assert!(!owner.layout.record().exists());
            }
            assert!(!owner.layout.command_directory.exists());
            assert!(!owner.layout.state_directory.join("snapshots").exists());
            let leaf = source.with_file_name("neohaskell-cache-policy.conf");
            if changes_policy {
                if !elevated {
                    assert_eq!(
                        fs::read("/etc/nix/.neo-policy-publication/main-slot").unwrap(),
                        original
                    );
                }
                assert!(fs::read(&source).unwrap().starts_with(&original));
                assert!(leaf.is_file());
            } else {
                assert_eq!(fs::read(&source).unwrap(), original);
                assert!(!leaf.exists());
            }
            eprintln!("ordinary install policy mode {mode} passed");
        }
    }

    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "requires a root-owned disposable pinned daemon with explicit disabled user sources"]
    fn discovered_policy_uses_one_owner_review() {
        use super::super::nix_owner::peer_handoff::launcher::{
            ExactExecutable, LaunchAuthority, Rendezvous,
        };
        use super::super::nix_owner::policy_observation::PolicyResult;
        let pid = std::env::var("NEO_TEST_DAEMON_PID")
            .unwrap()
            .parse()
            .unwrap();
        let daemon =
            std::path::PathBuf::from(std::env::var_os("NEO_TEST_DAEMON_EXECUTABLE").unwrap());
        let pinned = std::path::PathBuf::from(std::env::var_os("NEO_TEST_PINNED_STORE").unwrap());
        let expected = crate::candidate::OutputPath::parse(pinned.to_str().unwrap()).unwrap();
        let release = crate::candidate::FileIdentity {
            name: "neo-policy-helper".into(),
            size: std::env::var("NEO_TEST_PINNED_SIZE")
                .unwrap()
                .parse()
                .unwrap(),
            sha: std::env::var("NEO_TEST_PINNED_SHA").unwrap(),
        };
        for mode in [
            "accept",
            "decline",
            "source-inode",
            "include",
            "refused",
            "foreign-helper",
        ] {
            let (_temporary, owner, lease) = setup();
            owner.effects.state.borrow_mut().review = ReviewGate::Inspecting;
            retain(&owner.effects, &lease);
            let source = owner.layout.state_directory.join("nix.conf");
            let nested = owner.layout.state_directory.join("nested.conf");
            let service = owner.layout.state_directory.join("daemon.service");
            let original = b"include nested.conf\n";
            fs::write(&source, original).unwrap();
            fs::write(
                &nested,
                if mode == "refused" {
                    "include nix.conf\n"
                } else {
                    "max-jobs = 1\n"
                },
            )
            .unwrap();
            fs::write(&service, "fixed service fixture\n").unwrap();
            let executable = ExactExecutable::open(&pinned, &release).unwrap();
            let other_helper = ExactExecutable::open(&pinned, &release).unwrap();
            let authority = || LaunchAuthority::PolicyFixture {
                mode: "ok",
                source: source.clone(),
                service: service.clone(),
                executable: daemon.clone(),
                pid,
            };
            let discovered = executable
                .discover_policy(Rendezvous::fresh().unwrap(), authority())
                .unwrap();
            if mode == "refused" {
                assert!(discovered.is_err());
                assert!(owner.effects.take_review().is_none());
                assert_eq!(fs::read(&source).unwrap(), original);
                assert!(!owner.effects.mutation_admitted());
                eprintln!("discovered policy mode {mode} passed");
                continue;
            }
            let discovered = discovered.unwrap();
            let comparison = discovered.comparison();
            for foreign in [false, true] {
                let early = PolicyReviewBinding {
                    operation: PolicyOperation::ConventionalAppend,
                    owner: if foreign {
                        Weak::new()
                    } else {
                        Rc::downgrade(&owner.effects.state)
                    },
                    identity: 0,
                };
                let premature = executable
                    .discover_policy(Rendezvous::fresh().unwrap(), authority())
                    .unwrap()
                    .unwrap();
                assert!(
                    executable
                        .confirm_discovery(
                            &owner.effects,
                            &early,
                            premature,
                            comparison,
                            Rendezvous::fresh().unwrap(),
                            authority(),
                        )
                        .is_err()
                );
            }
            let mismatched = executable
                .discover_policy(Rendezvous::fresh().unwrap(), authority())
                .unwrap()
                .unwrap();
            assert!(
                runtime::discovered_policy_review_fixture(&source, pid + 1, &release, &expected)
                    .with_discovery(mismatched)
                    .is_err()
            );
            let unrelated = owner.layout.state_directory.join("unrelated.conf");
            fs::write(&unrelated, original).unwrap();
            let mismatched = executable
                .discover_policy(Rendezvous::fresh().unwrap(), authority())
                .unwrap()
                .unwrap();
            assert!(
                runtime::discovered_policy_review_fixture(&unrelated, pid, &release, &expected)
                    .with_discovery(mismatched)
                    .is_err()
            );
            if mode == "accept" {
                let wrong_release = executable
                    .discover_policy(Rendezvous::fresh().unwrap(), authority())
                    .unwrap()
                    .unwrap();
                let mut unexpected = release.clone();
                unexpected.size += 1;
                assert!(
                    runtime::discovered_policy_review_fixture(&source, pid, &unexpected, &expected)
                        .with_discovery(wrong_release)
                        .is_err()
                );
                let wrong_path = executable
                    .discover_policy(Rendezvous::fresh().unwrap(), authority())
                    .unwrap()
                    .unwrap();
                let other_path = crate::candidate::OutputPath::parse(
                    "/nix/store/i0md420ln0dwnjs0ibm8v6z2jkqpdaqi-neo-policy-helper",
                )
                .unwrap();
                assert!(
                    runtime::discovered_policy_review_fixture(&source, pid, &release, &other_path)
                        .with_discovery(wrong_path)
                        .is_err()
                );
            }
            let plan = runtime::discovered_policy_review_fixture(&source, pid, &release, &expected)
                .with_discovery(discovered)
                .unwrap();
            let summary = ReviewSummary {
                operation: ReviewOperation::Install,
                version: Some("1.0.0".into()),
                recovery: RecoveryPending::None,
                changes: vec![],
            };
            let mut review = Box::pin(owner.effects.review_nix_policy(plan, summary));
            assert!(poll(&owner.effects, review.as_mut()).is_pending());
            let offer = owner.effects.take_review().unwrap();
            assert_eq!(offer.summary.changes, vec![ReviewedChange::NixPolicy]);
            owner
                .effects
                .answer(
                    offer.token,
                    if mode == "decline" {
                        Some(UnadmittedReason::Declined)
                    } else {
                        None
                    },
                )
                .unwrap();
            let answer = drive(&owner.effects, review);
            if mode == "decline" {
                assert!(answer.is_err());
                assert_eq!(fs::read(&source).unwrap(), original);
                assert!(!owner.effects.mutation_admitted());
                eprintln!("discovered policy mode {mode} passed");
                continue;
            }
            let (plan, binding) = answer.unwrap();
            owner.effects.check_policy_recheck(&binding).unwrap();
            match mode {
                "source-inode" => {
                    fs::rename(&source, source.with_extension("old")).unwrap();
                    fs::write(&source, original).unwrap();
                }
                "include" => fs::write(&nested, "max-jobs = 2\n").unwrap(),
                _ => (),
            }
            let helper = if mode == "foreign-helper" {
                &other_helper
            } else {
                &executable
            };
            let fresh = helper
                .discover_policy(Rendezvous::fresh().unwrap(), authority())
                .unwrap()
                .unwrap();
            let compared = helper
                .observe_policy(Rendezvous::fresh().unwrap(), authority(), comparison)
                .unwrap();
            assert_eq!(
                compared.policy,
                Some(if matches!(mode, "source-inode" | "include") {
                    PolicyResult::ChangedInputs
                } else {
                    PolicyResult::MatchingInputs
                })
            );
            let fresh = if matches!(mode, "source-inode" | "include") {
                assert!(
                    helper
                        .confirm_discovery(
                            &owner.effects,
                            &binding,
                            fresh,
                            comparison,
                            Rendezvous::fresh().unwrap(),
                            authority()
                        )
                        .is_err()
                );
                helper
                    .discover_policy(Rendezvous::fresh().unwrap(), authority())
                    .unwrap()
                    .unwrap()
            } else {
                helper
                    .confirm_discovery(
                        &owner.effects,
                        &binding,
                        fresh,
                        comparison,
                        Rendezvous::fresh().unwrap(),
                        authority(),
                    )
                    .unwrap()
            };
            if mode == "accept" {
                assert!(fresh.is_confirmed(&binding));
                let stale = PolicyReviewBinding {
                    operation: PolicyOperation::ConventionalAppend,
                    owner: Rc::downgrade(&owner.effects.state),
                    identity: binding.identity + 1,
                };
                let foreign = PolicyReviewBinding {
                    operation: PolicyOperation::ConventionalAppend,
                    owner: Weak::new(),
                    identity: binding.identity,
                };
                assert!(!fresh.is_confirmed(&stale));
                assert!(!fresh.is_confirmed(&foreign));
                for invalid in [&stale, &foreign] {
                    let unconfirmed = executable
                        .discover_policy(Rendezvous::fresh().unwrap(), authority())
                        .unwrap()
                        .unwrap();
                    assert!(
                        executable
                            .confirm_discovery(
                                &owner.effects,
                                invalid,
                                unconfirmed,
                                comparison,
                                Rendezvous::fresh().unwrap(),
                                authority(),
                            )
                            .is_err()
                    );
                }
            }
            let fresh_plan =
                runtime::discovered_policy_review_fixture(&source, pid, &release, &expected)
                    .with_discovery(fresh)
                    .unwrap();
            let checked = runtime::test_policy_recheck(&owner, plan, binding, fresh_plan);
            if mode == "accept" {
                let binding = checked.unwrap();
                assert_eq!(compared.policy, Some(PolicyResult::MatchingInputs));
                owner.effects.admit_policy_review(&binding).unwrap();
                owner.effects.consume_policy_review(binding).unwrap();
            } else {
                assert!(checked.is_err(), "{mode}");
            }
            assert!(owner.effects.take_review().is_none());
            assert_eq!(fs::read(&source).unwrap(), original);
            assert!(
                !source
                    .with_file_name("neohaskell-cache-policy.conf")
                    .exists()
            );
            assert!(!owner.effects.mutation_admitted());
            eprintln!("discovered policy mode {mode} passed");
        }
    }

    #[test]
    fn cancellation_before_startup_admits_no_filesystem_work() {
        let temporary = tempfile::tempdir().unwrap();
        let state = temporary.path().join("not-created");
        let mut coordinator = Coordinator::new(
            snapshot::Request::RecoverOnly,
            runtime::Executor::NativeInstaller,
            Some(Layout::new(state.clone(), state.join("bin"))),
        );
        coordinator.request_cancel();
        let Poll::Ready(Ok(RunOutcome::Unadmitted(stopped))) = coordinator.advance() else {
            panic!("unstarted cancellation did not finish");
        };
        assert_eq!(stopped.reason, UnadmittedReason::Cancelled);
        assert!(!state.exists());
    }

    #[test]
    fn filesystem_cancellation_waits_for_admitted_write_and_retains_busy() {
        let (_temporary, owner, lease) = setup();
        retain(&owner.effects, &lease);
        let lock = lease.path.clone();
        drop(lease);
        let path = owner.layout.state_directory.join("gated-output");
        let (entered, entered_rx) = std::sync::mpsc::sync_channel(1);
        let (release, release_rx) = std::sync::mpsc::channel();
        let mut future = Box::pin(owner.effects.filesystem(FilesystemJob::GatedWrite {
            path: path.clone(),
            entered,
            release: release_rx,
        }));
        assert!(poll(&owner.effects, future.as_mut()).is_pending());
        assert!(poll(&owner.effects, future.as_mut()).is_pending());
        entered_rx.recv_timeout(Duration::from_secs(10)).unwrap();
        owner.effects.cancellation.request();
        assert!(poll(&owner.effects, future.as_mut()).is_pending());
        assert!(matches!(
            RootLease::acquire(&lock),
            Err(TransactionError::Busy)
        ));
        assert!(!path.exists());
        release.send(()).unwrap();
        let result = loop {
            if let Poll::Ready(result) = poll(&owner.effects, future.as_mut()) {
                break result;
            }
            owner.effects.wait();
        };
        assert!(result.is_ok());
        assert_eq!(fs::read(path).unwrap(), b"admitted write completed");
        drop(future);
        let observation = drive(&owner.effects, owner.observe_stopped()).unwrap();
        assert!(matches!(
            stopped(&owner.effects, observation).unwrap(),
            RunOutcome::Stopped(_)
        ));
        assert!(matches!(
            RootLease::acquire(&lock),
            Err(TransactionError::Busy)
        ));
        drop(owner);
        assert!(RootLease::acquire(&lock).is_ok());
    }

    #[test]
    fn live_process_cancellation_waits_for_client_absence() {
        let (_temporary, owner, lease) = setup();
        retain(&owner.effects, &lease);
        let lock = lease.path.clone();
        drop(lease);
        let marker = owner.layout.state_directory.join("marker");
        let mut future = Box::pin(owner.effects.process(process::WorkerRequest::Fixture {
            name: "resistant".into(),
        }));
        let deadline = Instant::now() + Duration::from_secs(15);
        let mut requested = false;
        let result = loop {
            assert!(
                Instant::now() < deadline,
                "contained process cancellation deadline"
            );
            if let Poll::Ready(result) = poll(&owner.effects, future.as_mut()) {
                break result;
            }
            if marker.exists() && !requested {
                owner.effects.cancellation.request();
                requested = true;
                assert!(matches!(
                    RootLease::acquire(&lock),
                    Err(TransactionError::Busy)
                ));
            }
            owner.effects.wait();
        };
        assert!(requested);
        assert!(matches!(result, Err(TransactionError::StopRequested)));
        let pid: i32 = fs::read_to_string(owner.layout.state_directory.join("client-pid"))
            .unwrap()
            .parse()
            .unwrap();
        assert_eq!(unsafe { libc::kill(pid, 0) }, -1);
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ESRCH)
        );
        drop(future);
        let observation = drive(&owner.effects, owner.observe_stopped()).unwrap();
        assert!(matches!(
            stopped(&owner.effects, observation).unwrap(),
            RunOutcome::Stopped(_)
        ));
        drop(owner);
        assert!(RootLease::acquire(&lock).is_ok());
    }

    #[test]
    fn activation_cancellation_cuts_report_the_observed_pair() {
        for cut in [
            "before_intent",
            "after_intent",
            "after_neo_link",
            "after_active_next_creation",
            "after_activation_rename",
            "before_receipt_rename",
            "after_receipt_rename",
            "before_intent_removal",
        ] {
            let (_temporary, owner, lease) = setup();
            let before_pair = PairIdentity {
                version: "1.0.0".into(),
                output: owner.layout.state_directory.join("pair-a"),
                selection: None,
            };
            let after_pair = PairIdentity {
                version: "2.0.0".into(),
                output: owner.layout.state_directory.join("pair-b"),
                selection: None,
            };
            let expected = if cut == "before_intent" {
                before_pair.clone()
            } else {
                after_pair.clone()
            };
            let before = owner.next_receipt(None, before_pair);
            test_with(
                &owner.effects,
                &lease,
                owner.switch_pair(
                    None,
                    before.clone(),
                    Operation::Install(InstallSelection::ExactVersion),
                ),
            )
            .unwrap();
            owner.effects.state.borrow_mut().diagnostics =
                super::super::observation::InvocationObservation::new();
            *owner.effects.cancellation.cut.lock().unwrap() = Some(cut);
            let after = owner.next_receipt(Some(&before), after_pair.clone());
            test_with(&owner.effects, &lease, async {
                assert_eq!(
                    owner
                        .switch_pair(
                            Some(before),
                            after,
                            Operation::Update(InstallSelection::ExactVersion)
                        )
                        .await,
                    Err(TransactionError::StopRequested),
                    "{cut}"
                );
                let observation = owner.observe_stopped().await.unwrap();
                let RunOutcome::Stopped(result) = stopped(&owner.effects, observation).unwrap()
                else {
                    panic!("activation cancellation returned completion");
                };
                assert!(
                    matches!(result.observation, StopObservation::ObservedPair(ref pair) if pair == &expected),
                    "{cut}"
                );
            });
            let record: super::super::InstallationRecord =
                serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
            assert!(record.activation.is_none(), "{cut}");
            let facts = owner.effects.state.borrow().diagnostics.snapshot();
            assert_eq!(
                facts
                    .applied_changes
                    .contains(&crate::diagnostics::AppliedChange::ActivationCommitted),
                cut != "before_intent",
                "{cut}"
            );
            assert_eq!(
                fs::read_link(owner.layout.active_link()).unwrap(),
                expected.output,
                "{cut}"
            );
        }
    }

    #[test]
    fn required_record_observation_survives_latched_stop_but_requested_read_does_not() {
        let (_temporary, owner, lease) = setup();
        owner.effects.cancellation.request();
        test_with(&owner.effects, &lease, async {
            assert_eq!(
                owner.read_record().await,
                Err(TransactionError::StopRequested)
            );
            assert!(matches!(
                owner.observe_stopped().await.unwrap(),
                StopObservation::ObservedNoPair
            ));
        });
    }
    #[test]
    fn submission_failure_remains_failure_when_cancel_arrives_during_reap() {
        let (_temporary, owner, lease) = setup();
        retain(&owner.effects, &lease);
        let request = process::WorkerRequest::Release(super::super::release::FetchRequest {
            fetch: super::super::release::ReleaseFetch::Page { page: 0 },
            authority: None,
        });
        let mut future = Box::pin(owner.effects.process(request));
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut cancelled = false;
        let result = loop {
            assert!(
                Instant::now() < deadline,
                "submission failure did not settle"
            );
            if let Poll::Ready(result) = poll(&owner.effects, future.as_mut()) {
                break result;
            }
            if matches!(
                &owner.effects.state.borrow().slot,
                EffectSlot::Active(ActiveEffect::Process {
                    stop: ProcessStop::SubmissionFailure,
                    ..
                })
            ) {
                owner.effects.cancellation.request();
                cancelled = true;
            }
            owner.effects.wait();
        };
        assert!(cancelled);
        assert!(matches!(
            result,
            Err(TransactionError::Verification("worker submission"))
        ));
    }

    #[test]
    fn activation_failure_preserves_primary_and_failed_recovery() {
        use std::os::unix::fs::symlink;
        let (_temporary, owner, lease) = setup();
        let pair = PairIdentity {
            version: "1.0.0".into(),
            output: owner.layout.state_directory.join("pair-a"),
            selection: None,
        };
        let before = owner.next_receipt(None, pair);
        test_with(
            &owner.effects,
            &lease,
            owner.switch_pair(
                None,
                before.clone(),
                Operation::Install(InstallSelection::ExactVersion),
            ),
        )
        .unwrap();
        retain(&owner.effects, &lease);
        let pair = PairIdentity {
            version: "2.0.0".into(),
            output: owner.layout.state_directory.join("pair-b"),
            selection: None,
        };
        let after = owner.next_receipt(Some(&before), pair);
        let mut future = Box::pin(owner.switch_pair(
            Some(before),
            after,
            Operation::Update(InstallSelection::ExactVersion),
        ));
        let mut changed = false;
        let result = loop {
            if let Poll::Ready(result) = poll(&owner.effects, future.as_mut()) {
                break result;
            }
            let record: super::super::InstallationRecord =
                serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
            if record.activation.is_some() && !changed {
                let command = owner.layout.command_directory.join("neo");
                fs::remove_file(&command).unwrap();
                symlink(owner.layout.state_directory.join("foreign"), command).unwrap();
                changed = true;
            }
            owner.effects.wait();
        };
        assert!(changed);
        assert_eq!(
            result,
            Err(TransactionError::ActivationRecoveryFailed {
                primary: Box::new(TransactionError::CommandLinkConflict),
                recovery: Box::new(TransactionError::CommandLinkConflict),
            })
        );
        assert!(stopped(&owner.effects, StopObservation::NotRequired).is_err());
    }

    #[test]
    fn failed_required_observation_never_constructs_stopped() {
        let (_temporary, owner, lease) = setup();
        fs::write(owner.layout.record(), b"invalid").unwrap();
        fs::set_permissions(owner.layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
        owner.effects.cancellation.request();
        test_with(&owner.effects, &lease, async {
            assert!(matches!(
                owner.observe_stopped().await,
                Err(TransactionError::InvalidState(_))
            ));
            assert!(stopped(&owner.effects, StopObservation::NotRequired).is_err());
        });
    }
    #[test]
    fn cancellation_after_startup_does_not_initialize_fresh_resources() {
        let (_temporary, owner, lease) = setup();
        drop(lease);
        let mut coordinator = Coordinator::new(
            snapshot::Request::RecoverOnly,
            runtime::Executor::NativeInstaller,
            Some(owner.layout.clone()),
        );
        let mut cancelled = false;
        let outcome = loop {
            if let Poll::Ready(result) = coordinator.advance() {
                break result.unwrap();
            }
            if coordinator.effects.state.borrow().lease.is_some() && !cancelled {
                coordinator.request_cancel();
                cancelled = true;
            }
            coordinator.effects.wait();
        };
        assert!(cancelled);
        assert!(matches!(
            outcome,
            RunOutcome::Unadmitted(UnadmittedOperation {
                reason: UnadmittedReason::Cancelled,
                ..
            })
        ));
        assert!(!owner.layout.state_directory.join("resources").exists());
        assert!(!owner.layout.record().exists());
        assert!(
            !owner
                .layout
                .state_directory
                .join("fixture-operations.jsonl")
                .exists()
        );
    }
    #[test]
    fn positive_wait_budgets_never_round_down_to_nonblocking_poll() {
        let now = Instant::now();
        for (remaining, expected) in [
            (Duration::from_nanos(1), 1),
            (Duration::from_micros(500), 1),
            (Duration::from_millis(1), 1),
            (Duration::from_micros(1500), 2),
            (Duration::from_millis(100), 10),
        ] {
            let interest = WaitInterest {
                descriptor: None,
                deadline: now + remaining,
            };
            assert_eq!(interest.timeout_millis(now), expected);
        }
        let expired = WaitInterest {
            descriptor: None,
            deadline: now,
        };
        assert_eq!(expired.timeout_millis(now), 0);
    }

    #[test]
    fn coordinator_correction_record_limit_preserves_original_contract() {
        let (_temporary, owner, lease) = setup();
        let maximum = super::super::MAX_STATE_BYTES as usize;
        let mut at_limit =
            serde_json::to_vec(&super::super::InstallationRecord::default()).unwrap();
        at_limit.resize(maximum, b' ');
        fs::write(owner.layout.record(), at_limit).unwrap();
        fs::set_permissions(owner.layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
        assert_eq!(
            test_with(&owner.effects, &lease, owner.read_record()),
            Ok(super::super::InstallationRecord::default())
        );

        fs::write(owner.layout.record(), vec![b' '; maximum + 1]).unwrap();
        assert_eq!(
            test_with(&owner.effects, &lease, owner.read_record()),
            Err(TransactionError::InvalidState(
                "state record exceeds the size limit"
            ))
        );
    }

    #[test]
    fn coordinator_correction_snapshot_limit_preserves_original_contract() {
        let (_temporary, owner, lease) = setup();
        let maximum = 4096;
        let path = owner.layout.state_directory.join("snapshot.json");
        fs::write(&path, vec![0x5a; maximum]).unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
        assert_eq!(
            test_with(
                &owner.effects,
                &lease,
                owner.read_snapshot(path.clone(), maximum)
            ),
            Ok(Some(vec![0x5a; maximum]))
        );

        fs::write(&path, vec![0x5a; maximum + 1]).unwrap();
        assert_eq!(
            test_with(&owner.effects, &lease, owner.read_snapshot(path, maximum)),
            Err(TransactionError::Verification("snapshot file size"))
        );
    }

    #[test]
    fn coordinator_correction_pre_ready_handshake_failure_is_worker_admission() {
        let (_temporary, owner, lease) = setup();
        retain(&owner.effects, &lease);
        let mut future = Box::pin(owner.effects.process(process::WorkerRequest::Version));
        assert!(poll(&owner.effects, future.as_mut()).is_pending());
        fs::remove_file(&lease.path).unwrap();

        let deadline = Instant::now() + Duration::from_secs(10);
        let result = loop {
            assert!(
                Instant::now() < deadline,
                "pre-ready worker handshake failure did not settle"
            );
            if let Poll::Ready(result) = poll(&owner.effects, future.as_mut()) {
                break result;
            }
            owner.effects.wait();
        };
        assert!(
            matches!(
                result,
                Err(TransactionError::Verification("worker admission"))
            ),
            "expected worker admission, got {result:?}"
        );
        assert!(!lease.path.exists());
    }
    fn review_fixture(after_switch: Option<bool>) -> (tempfile::TempDir, Layout, Coordinator) {
        review_fixture_on_path(after_switch, None)
    }

    fn review_fixture_on_path(
        after_switch: Option<bool>,
        command_path: Option<&std::path::Path>,
    ) -> (tempfile::TempDir, Layout, Coordinator) {
        let temporary = tempfile::tempdir().unwrap();
        let state = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let mut layout = Layout::new(state.clone(), state.join("bin"));
        layout.command_path = command_path.map(|path| path.as_os_str().to_owned());
        if let Some(after_switch) = after_switch {
            let owner = InstallationOwner::new(layout.clone());
            let before = owner.next_receipt(
                None,
                PairIdentity {
                    version: "1.0.0".into(),
                    output: state.join("pair-a"),
                    selection: None,
                },
            );
            let after = owner.next_receipt(
                Some(&before),
                PairIdentity {
                    version: "2.0.0".into(),
                    output: state.join("pair-b"),
                    selection: None,
                },
            );
            let record = super::super::InstallationRecord {
                committed: Some(before.clone()),
                activation: Some(super::super::Intent {
                    schema: super::super::STATE_SCHEMA,
                    operation: Operation::Update(InstallSelection::ExactVersion),
                    before: Some(before.clone()),
                    after: after.clone(),
                }),
                ..Default::default()
            };
            fs::write(layout.record(), serde_json::to_vec(&record).unwrap()).unwrap();
            fs::set_permissions(layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
            std::os::unix::fs::symlink(
                if after_switch {
                    after.active.output
                } else {
                    before.active.output
                },
                layout.active_link(),
            )
            .unwrap();
        }
        let coordinator = Coordinator::new(
            snapshot::Request::RecoverOnly,
            runtime::Executor::NativeInstaller,
            Some(layout.clone()),
        );
        (temporary, layout, coordinator)
    }

    fn offer(coordinator: &mut Coordinator) -> ReviewOffer {
        loop {
            assert!(
                coordinator.advance().is_pending(),
                "owner stopped before offering review"
            );
            if let Some(offer) = coordinator.effects.take_review() {
                return offer;
            }
            coordinator.effects.wait();
        }
    }

    fn finish(coordinator: &mut Coordinator) -> Result<RunOutcome, TransactionError> {
        loop {
            if let Poll::Ready(result) = coordinator.advance() {
                return result;
            }
            coordinator.effects.wait();
        }
    }

    #[test]
    fn review_withheld_and_cancelled_preserve_pending_activation_on_both_sides() {
        for switched in [false, true] {
            for reason in [
                UnadmittedReason::ConsentRequired,
                UnadmittedReason::Declined,
                UnadmittedReason::Cancelled,
            ] {
                let (_temporary, layout, mut coordinator) = review_fixture(Some(switched));
                let before = fs::read(layout.record()).unwrap();
                let active = fs::read_link(layout.active_link()).unwrap();
                let offered = offer(&mut coordinator);
                assert_eq!(offered.summary.recovery, RecoveryPending::Activation);
                assert!(!layout.command_directory.exists());
                assert!(matches!(
                    RootLease::acquire(&layout.lock()),
                    Err(TransactionError::Busy)
                ));
                match reason {
                    UnadmittedReason::Cancelled => coordinator.request_cancel(),
                    UnadmittedReason::ConsentRequired => coordinator
                        .answer(offered, ReviewDecision::ConsentRequired)
                        .unwrap(),
                    UnadmittedReason::Declined => coordinator
                        .answer(offered, ReviewDecision::Decline)
                        .unwrap(),
                    UnadmittedReason::ReviewChanged => unreachable!(),
                }
                let RunOutcome::Unadmitted(stopped) = finish(&mut coordinator).unwrap() else {
                    panic!("withheld review admitted work");
                };
                assert_eq!(stopped.reason, reason);
                let facts = coordinator.diagnostics();
                use crate::diagnostics::{Phase, ReviewAnswer};
                assert_eq!(
                    facts
                        .events
                        .iter()
                        .any(|event| event.phase == Phase::Unadmitted),
                    reason != UnadmittedReason::Cancelled,
                );
                let review = facts.review.unwrap();
                assert_eq!(
                    review.answer,
                    Some(match reason {
                        UnadmittedReason::ConsentRequired => ReviewAnswer::ConsentRequired,
                        UnadmittedReason::Declined => ReviewAnswer::Declined,
                        UnadmittedReason::Cancelled => ReviewAnswer::Cancelled,
                        _ => unreachable!(),
                    })
                );
                assert!(review.wait_ms.is_some());
                assert!(review.admitted_ms.is_none());
                assert!(facts.applied_changes.is_empty());
                assert!(facts.completed_steps.is_empty());
                assert_eq!(stopped.recovery, RecoveryPending::Activation);
                assert_eq!(fs::read(layout.record()).unwrap(), before);
                assert_eq!(fs::read_link(layout.active_link()).unwrap(), active);
                assert!(!layout.command_directory.exists());
            }
        }
    }

    #[test]
    fn diagnostic_owner_timeline_is_bounded_and_keeps_review_summary() {
        let (_temporary, _layout, mut coordinator) = review_fixture(None);
        for _ in 0..100 {
            coordinator
                .effects
                .observe(super::super::observation::Activity::Inspecting);
            coordinator
                .effects
                .observe(super::super::observation::Activity::Selecting);
        }
        let offered = offer(&mut coordinator);
        coordinator
            .effects
            .answer(offered.token, Some(UnadmittedReason::Declined))
            .unwrap();
        assert!(matches!(
            finish(&mut coordinator),
            Ok(RunOutcome::Unadmitted(_))
        ));
        let facts = coordinator.diagnostics();
        assert_eq!(facts.events.len(), 64);
        assert!(facts.events_truncated);
        assert_eq!(facts.phase, crate::diagnostics::Phase::Finished);
        assert_eq!(
            facts.review.as_ref().unwrap().answer,
            Some(crate::diagnostics::ReviewAnswer::Declined)
        );
        assert!(
            facts
                .events
                .windows(2)
                .all(|pair| pair[0].elapsed_ms <= pair[1].elapsed_ms)
        );
        assert!(serde_json::to_vec(&facts).unwrap().len() < 10 * 1024);
    }

    #[test]
    fn review_fresh_withholding_creates_no_requested_state() {
        let (_temporary, layout, mut coordinator) = review_fixture(None);
        let offered = offer(&mut coordinator);
        coordinator
            .effects
            .answer(offered.token, Some(UnadmittedReason::ConsentRequired))
            .unwrap();
        assert!(matches!(
            finish(&mut coordinator),
            Ok(RunOutcome::Unadmitted(_))
        ));
        assert!(!layout.record().exists());
        assert!(!layout.command_directory.exists());
        assert!(!layout.state_directory.join("resources").exists());
        assert!(!layout.state_directory.join("snapshots").exists());
    }

    #[test]
    fn review_acceptance_settles_exact_projected_activation() {
        for switched in [false, true] {
            let (_temporary, layout, mut coordinator) = review_fixture(Some(switched));
            let active = fs::read_link(layout.active_link()).unwrap();
            let offered = offer(&mut coordinator);
            coordinator.effects.answer(offered.token, None).unwrap();
            assert!(matches!(
                finish(&mut coordinator),
                Ok(RunOutcome::Complete(None))
            ));
            let record: super::super::InstallationRecord =
                serde_json::from_slice(&fs::read(layout.record()).unwrap()).unwrap();
            assert!(record.activation.is_none());
            assert_eq!(record.committed.unwrap().active.output, active);
            let facts = coordinator.diagnostics();
            let review = facts.review.as_ref().unwrap();
            assert_eq!(
                review.answer,
                Some(crate::diagnostics::ReviewAnswer::Accepted)
            );
            assert!(review.admitted_ms.unwrap() >= review.entered_ms + review.wait_ms.unwrap());
            assert!(
                facts
                    .completed_steps
                    .contains(&crate::diagnostics::CompletedStep::FinalSettlement)
            );
            assert!(
                facts
                    .applied_changes
                    .contains(&crate::diagnostics::AppliedChange::RecoveryRepairsSettled)
            );
            assert!(
                facts
                    .events
                    .windows(2)
                    .all(|pair| pair[0].elapsed_ms <= pair[1].elapsed_ms)
            );
            for command in ["neo", "neoctl"] {
                assert_eq!(
                    fs::read_link(layout.command_directory.join(command)).unwrap(),
                    layout.expected_command_target(command)
                );
            }
        }
    }

    #[test]
    fn review_directory_creation_cancellation_finishes_reserved_recovery() {
        for cut in [
            "before_reviewed_recovery",
            "after_reviewed_command_directory",
        ] {
            let (_temporary, layout, mut coordinator) = review_fixture(Some(true));
            let before = fs::read(layout.record()).unwrap();
            let offered = offer(&mut coordinator);
            coordinator.effects.cancel_at(cut);
            coordinator.effects.answer(offered.token, None).unwrap();
            let result = finish(&mut coordinator).unwrap();
            let facts = coordinator.diagnostics();
            let review = facts.review.as_ref().unwrap();
            assert_eq!(
                review.answer,
                Some(crate::diagnostics::ReviewAnswer::Accepted)
            );
            assert!(review.admitted_ms.is_some());
            assert!(facts.cancelled_ms.is_some());
            assert_eq!(
                facts
                    .applied_changes
                    .contains(&crate::diagnostics::AppliedChange::RecoveryRepairsSettled),
                cut != "before_reviewed_recovery"
            );
            if cut == "before_reviewed_recovery" {
                assert!(matches!(result, RunOutcome::Unadmitted(_)));
                assert_eq!(fs::read(layout.record()).unwrap(), before);
                assert!(!layout.command_directory.exists());
            } else {
                let RunOutcome::Stopped(stopped) = result else {
                    panic!("admitted recovery did not settle cancellation");
                };
                assert!(
                    matches!(stopped.observation, StopObservation::ObservedPair(ref pair) if pair.version == "2.0.0")
                );
                let record: super::super::InstallationRecord =
                    serde_json::from_slice(&fs::read(layout.record()).unwrap()).unwrap();
                assert!(record.activation.is_none());
                assert!(layout.command_directory.exists());
                assert!(matches!(
                    RootLease::acquire(&layout.lock()),
                    Err(TransactionError::Busy)
                ));
            }
        }
    }

    #[test]
    fn review_accept_then_cancel_before_first_mutation_preserves_pending_bytes() {
        let (_temporary, layout, mut coordinator) = review_fixture(Some(true));
        let before = fs::read(layout.record()).unwrap();
        let offered = offer(&mut coordinator);
        coordinator.effects.answer(offered.token, None).unwrap();
        coordinator.request_cancel();
        assert!(matches!(
            finish(&mut coordinator),
            Ok(RunOutcome::Unadmitted(_))
        ));
        assert_eq!(fs::read(layout.record()).unwrap(), before);
        assert!(!layout.command_directory.exists());
        assert!(coordinator.effects.state.borrow().settlement.is_none());
    }

    #[test]
    fn review_rechecks_command_path_before_introducing_links() {
        let root = tempfile::tempdir().unwrap();
        let foreign_directory = root.path().canonicalize().unwrap();
        let foreign = foreign_directory.join("neoctl");
        let (_temporary, layout, mut coordinator) =
            review_fixture_on_path(Some(true), Some(&foreign_directory));
        let before = fs::read(layout.record()).unwrap();
        let offered = offer(&mut coordinator);
        fs::write(&foreign, "").unwrap();
        fs::set_permissions(&foreign, fs::Permissions::from_mode(0o700)).unwrap();
        coordinator.effects.answer(offered.token, None).unwrap();
        assert!(matches!(
            finish(&mut coordinator),
            Err(TransactionError::CommandPathConflict {
                command: super::super::ManagedCommand::Neoctl,
                path,
            }) if path == foreign
        ));
        let facts = coordinator.diagnostics();
        assert!(facts.review.unwrap().admitted_ms.is_none());
        assert!(facts.applied_changes.is_empty());
        assert_eq!(fs::read(layout.record()).unwrap(), before);
        assert!(!layout.command_directory.exists());
    }

    #[test]
    fn review_rechecks_exact_record_absence_bytes_and_link_identity() {
        for drift in ["bytes", "absence", "link", "parent"] {
            let (_temporary, layout, mut coordinator) =
                review_fixture((drift != "absence").then_some(true));
            let offered = offer(&mut coordinator);
            match drift {
                "bytes" => {
                    let mut bytes = fs::read(layout.record()).unwrap();
                    bytes.push(b'\n');
                    fs::write(layout.record(), bytes).unwrap();
                }
                "absence" => {
                    fs::write(
                        layout.record(),
                        serde_json::to_vec(&super::super::InstallationRecord::default()).unwrap(),
                    )
                    .unwrap();
                    fs::set_permissions(layout.record(), fs::Permissions::from_mode(0o600))
                        .unwrap();
                }
                "link" => {
                    let target = fs::read_link(layout.active_link()).unwrap();
                    std::os::unix::fs::symlink(target, layout.active_next_link()).unwrap();
                    fs::rename(layout.active_next_link(), layout.active_link()).unwrap();
                }
                "parent" => {
                    fs::create_dir(&layout.command_directory).unwrap();
                }
                _ => unreachable!(),
            }
            let current = fs::read(layout.record()).unwrap();
            coordinator.effects.answer(offered.token, None).unwrap();
            let RunOutcome::Unadmitted(stopped) = finish(&mut coordinator).unwrap() else {
                panic!("changed review admitted mutation");
            };
            assert_eq!(stopped.reason, UnadmittedReason::ReviewChanged, "{drift}");
            let facts = coordinator.diagnostics();
            let review = facts.review.unwrap();
            assert_eq!(
                review.answer,
                Some(crate::diagnostics::ReviewAnswer::Accepted)
            );
            assert!(review.admitted_ms.is_none());
            assert!(facts.applied_changes.is_empty());
            assert_eq!(fs::read(layout.record()).unwrap(), current);
            assert!(!coordinator.effects.mutation_admitted());
        }
    }

    #[test]
    fn review_tokens_are_bound_to_owner_generation_and_single_consumption() {
        let (_temporary, _layout, mut coordinator) = review_fixture(None);
        let offered = offer(&mut coordinator);
        let other = Effects::for_tests();
        let foreign = ReviewToken {
            owner: Rc::downgrade(&other.state),
            identity: offered.token.identity,
        };
        assert!(coordinator.effects.answer(foreign, None).is_err());
        let wrong = ReviewToken {
            owner: offered.token.owner.clone(),
            identity: offered.token.identity + 1,
        };
        assert!(coordinator.effects.answer(wrong, None).is_err());
        let replay = ReviewToken {
            owner: offered.token.owner.clone(),
            identity: offered.token.identity,
        };
        coordinator.effects.answer(offered.token, None).unwrap();
        assert!(coordinator.effects.answer(replay, None).is_err());
        assert!(
            coordinator
                .effects
                .settlement(SettlementKind::Recovery)
                .is_err()
        );
        assert!(!coordinator.effects.mutation_admitted());
        assert!(matches!(
            finish(&mut coordinator),
            Ok(RunOutcome::Complete(None))
        ));
    }

    #[test]
    fn review_gate_refuses_mutations_and_settlement_before_recheck() {
        let (_temporary, layout, mut coordinator) = review_fixture(None);
        let offered = offer(&mut coordinator);
        for rechecking in [false, true] {
            if rechecking {
                coordinator
                    .effects
                    .answer(
                        ReviewToken {
                            owner: offered.token.owner.clone(),
                            identity: offered.token.identity,
                        },
                        None,
                    )
                    .unwrap();
            }
            for job in [
                FilesystemJob::EnsureCommandDirectory(layout.command_directory.clone()),
                FilesystemJob::PublishRecord {
                    path: layout.record(),
                    bytes: b"forbidden".to_vec(),
                    committing: false,
                },
                FilesystemJob::EnsureSnapshotDirectory(layout.state_directory.join("snapshots")),
                FilesystemJob::OpenResourceDirectory(layout.state_directory.join("resources")),
            ] {
                assert!(
                    coordinator
                        .effects
                        .check_admission(&EffectRequest::Filesystem(job, Admission::Requested))
                        .is_err()
                );
            }
            assert!(
                coordinator
                    .effects
                    .check_admission(&EffectRequest::Process(
                        process::WorkerRequest::AcquireRetained {
                            pair: "forbidden".into(),
                            candidate: "forbidden".into(),
                            offline: false
                        }
                    ))
                    .is_err()
            );
            for kind in [
                SettlementKind::Recovery,
                SettlementKind::Activation,
                SettlementKind::Resource,
            ] {
                assert!(coordinator.effects.settlement(kind).is_err());
            }
            assert!(!layout.record().exists());
            assert!(!layout.command_directory.exists());
        }
        assert!(
            FilesystemTask::start(
                FilesystemJob::EnsureCommandDirectory(layout.command_directory.clone()),
                Some(
                    coordinator
                        .effects
                        .state
                        .borrow()
                        .lease
                        .as_ref()
                        .unwrap()
                        .file
                        .try_clone()
                        .unwrap()
                ),
                Cancellation::default(),
                Admission::Requested,
                None,
                None
            )
            .is_err()
        );
    }

    #[test]
    fn review_pending_resource_effects_are_observed_without_publication() {
        use super::super::resources::{ParentObservation, ResourceEffect};
        use std::os::unix::fs::MetadataExt;
        for kind in ["acquire", "register", "remove"] {
            for reason in [
                None,
                Some(UnadmittedReason::Declined),
                Some(UnadmittedReason::Cancelled),
            ] {
                let accept = reason.is_none();
                let (_temporary, layout, mut coordinator) = review_fixture(None);
                let directory = layout.state_directory.join("resources");
                fs::create_dir(&directory).unwrap();
                fs::set_permissions(&directory, fs::Permissions::from_mode(0o700)).unwrap();
                let metadata = fs::metadata(&directory).unwrap();
                let mut record = super::super::InstallationRecord::default();
                record.resources.parent = Some(ParentObservation {
                    device: metadata.dev(),
                    inode: metadata.ino(),
                    uid: metadata.uid(),
                });
                let pair = "/nix/store/00000000000000000000000000000000-pair".to_string();
                record.resources.pending = Some(match kind {
                    "acquire" => ResourceEffect::Acquire {
                        pair,
                        root: "candidate-a".into(),
                    },
                    "register" => ResourceEffect::Register {
                        pair,
                        parent: "candidate-a".into(),
                    },
                    "remove" => {
                        std::os::unix::fs::symlink(&pair, directory.join("candidate-a")).unwrap();
                        let observed = super::super::resource_io::ResourceDirectory::open_existing(
                            directory.clone(),
                        )
                        .unwrap()
                        .unwrap()
                        .inventory(2)
                        .unwrap()
                        .remove("candidate-a")
                        .unwrap();
                        record
                            .resources
                            .links
                            .insert("candidate-a".into(), observed.clone());
                        ResourceEffect::Remove {
                            name: "candidate-a".into(),
                            observed,
                        }
                    }
                    _ => unreachable!(),
                });
                let bytes = serde_json::to_vec(&record).unwrap();
                fs::write(layout.record(), &bytes).unwrap();
                fs::set_permissions(layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
                let offered = offer(&mut coordinator);
                assert_eq!(offered.summary.recovery, RecoveryPending::Resources);
                assert_eq!(fs::read(layout.record()).unwrap(), bytes);
                if reason == Some(UnadmittedReason::Cancelled) {
                    coordinator.request_cancel();
                } else {
                    coordinator.effects.answer(offered.token, reason).unwrap();
                }
                let result = finish(&mut coordinator).unwrap();
                if accept {
                    assert!(matches!(result, RunOutcome::Complete(None)));
                    let after: super::super::InstallationRecord =
                        serde_json::from_slice(&fs::read(layout.record()).unwrap()).unwrap();
                    assert!(after.resources.pending.is_none());
                } else {
                    assert!(matches!(result, RunOutcome::Unadmitted(_)));
                    assert_eq!(fs::read(layout.record()).unwrap(), bytes);
                }
                assert!(!layout.command_directory.exists());
                assert_eq!(
                    directory.join("candidate-a").symlink_metadata().is_ok(),
                    kind == "remove"
                );
            }
        }
    }

    #[test]
    fn review_projected_roles_drive_keep_and_rollback_without_metadata_effects() {
        let (_temporary, layout, _coordinator) = review_fixture(Some(true));
        let mut record: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(layout.record()).unwrap()).unwrap();
        let a = super::super::release::PinnedSelection {
            version: "1.0.0".into(),
            candidate_id: "a".repeat(64),
        };
        let b = super::super::release::PinnedSelection {
            version: "2.0.0".into(),
            candidate_id: "b".repeat(64),
        };
        record.committed.as_mut().unwrap().active.selection = Some(a.clone());
        let intent = record.activation.as_mut().unwrap();
        intent.before.as_mut().unwrap().active.selection = Some(a.clone());
        intent.after.active.selection = Some(b.clone());
        intent.after.previous.as_mut().unwrap().selection = Some(a.clone());
        fs::write(layout.record(), serde_json::to_vec(&record).unwrap()).unwrap();
        let owner = InstallationOwner::new(layout.clone());
        let lease = RootLease::acquire(&layout.lock()).unwrap();
        let prepared = test_with(&owner.effects, &lease, owner.inspect_recovery()).unwrap();
        assert_eq!(
            prepared
                .projected
                .ready_demand(&snapshot::Request::Install(None))
                .unwrap(),
            snapshot::Demand::Recorded(b)
        );
        assert_eq!(
            prepared
                .projected
                .ready_demand(&snapshot::Request::Rollback)
                .unwrap(),
            snapshot::Demand::Recorded(a)
        );
        assert!(!layout.command_directory.exists());
    }

    #[test]
    fn review_inflight_read_cancellation_waits_for_task_and_preserves_old_journal() {
        let (_temporary, layout, _coordinator) = review_fixture(Some(true));
        let before = fs::read(layout.record()).unwrap();
        let lease = RootLease::acquire(&layout.lock()).unwrap();
        let effects = Effects::for_tests();
        retain(&effects, &lease);
        drop(lease);
        effects.recovery_pending(RecoveryPending::Activation);
        let (entered, entered_rx) = std::sync::mpsc::sync_channel(1);
        let (release, release_rx) = std::sync::mpsc::sync_channel(1);
        let mut future = Box::pin(effects.filesystem(FilesystemJob::GatedRead {
            path: layout.record(),
            entered,
            release: release_rx,
        }));
        loop {
            assert!(poll(&effects, future.as_mut()).is_pending());
            if entered_rx.try_recv().is_ok() {
                break;
            }
            effects.wait();
        }
        effects.cancellation.request();
        assert!(unadmitted(&effects).is_err());
        assert!(poll(&effects, future.as_mut()).is_pending());
        assert!(matches!(
            RootLease::acquire(&layout.lock()),
            Err(TransactionError::Busy)
        ));
        release.send(()).unwrap();
        let result = loop {
            if let Poll::Ready(result) = poll(&effects, future.as_mut()) {
                break result;
            }
            effects.wait();
        };
        assert!(matches!(result, Err(TransactionError::StopRequested)));
        drop(future);
        assert!(matches!(
            unadmitted(&effects),
            Ok(RunOutcome::Unadmitted(_))
        ));
        assert_eq!(fs::read(layout.record()).unwrap(), before);
        assert!(!layout.command_directory.exists());
    }

    #[test]
    fn review_plain_driver_prints_before_accepting_and_reports_missing_consent() {
        for yes in [false, true] {
            let (_temporary, layout, mut coordinator) = review_fixture(Some(true));
            let arguments: Vec<std::ffi::OsString> = if yes {
                vec!["update".into(), "--rollback".into(), "--yes".into()]
            } else {
                vec!["rollback".into()]
            };
            let (request, consent) = crate::parse_invocation(&arguments).unwrap();
            assert!(matches!(request, snapshot::Request::Rollback));
            assert_eq!(consent.yes, yes);
            let before = fs::read(layout.record()).unwrap();
            let mut output = Vec::new();
            let result = drive_plain(&mut coordinator, consent.yes, &mut output).unwrap();
            let text = String::from_utf8(output).unwrap();
            assert!(text.contains(&crate::english().review_recover));
            assert!(text.contains(&crate::english().review_pending_recovery));
            assert!(!text.contains("candidate_id"));
            if yes {
                assert!(matches!(result, RunOutcome::Complete(None)));
                assert_ne!(fs::read(layout.record()).unwrap(), before);
            } else {
                assert!(matches!(
                    result,
                    RunOutcome::Unadmitted(UnadmittedOperation {
                        reason: UnadmittedReason::ConsentRequired,
                        ..
                    })
                ));
                assert_eq!(fs::read(layout.record()).unwrap(), before);
            }
            assert_eq!(
                crate::format_result(Ok(result)).code(),
                if yes { 0 } else { 2 }
            );
        }
    }

    #[test]
    fn review_plain_output_failure_never_consumes_acceptance() {
        struct Fails;
        impl std::io::Write for Fails {
            fn write(&mut self, _: &[u8]) -> std::io::Result<usize> {
                Err(std::io::Error::other("fixture output failure"))
            }
            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }
        let (_temporary, layout, mut coordinator) = review_fixture(Some(true));
        let before = fs::read(layout.record()).unwrap();
        assert!(matches!(
            drive_plain(&mut coordinator, true, &mut Fails),
            Err(TransactionError::Io)
        ));
        assert!(!coordinator.effects.mutation_admitted());
        assert!(matches!(
            coordinator.effects.state.borrow().review,
            ReviewGate::Awaiting { .. }
        ));
        assert_eq!(fs::read(layout.record()).unwrap(), before);
        assert!(!layout.command_directory.exists());
    }

    #[test]
    fn review_parser_requires_one_explicit_yes_and_rejects_conflicts() {
        for arguments in [
            vec!["install", "--yes", "--yes"],
            vec!["update", "--rollback", "--version", "1.0.0", "--yes"],
            vec!["rollback", "yes"],
            vec!["install", "--yes=true"],
        ] {
            assert!(
                crate::parse_invocation(&arguments.into_iter().map(Into::into).collect::<Vec<_>>())
                    .is_err()
            );
        }
        for arguments in [
            vec!["install"],
            vec!["update", "--version", "1.0.0"],
            vec!["rollback"],
        ] {
            assert!(
                !crate::parse_invocation(
                    &arguments.into_iter().map(Into::into).collect::<Vec<_>>()
                )
                .unwrap()
                .1
                .yes
            );
        }
    }

    #[test]
    fn review_cancel_during_recheck_leaves_old_pending_work_unadmitted() {
        let (_temporary, layout, mut coordinator) = review_fixture(Some(false));
        let before = fs::read(layout.record()).unwrap();
        let offered = offer(&mut coordinator);
        coordinator.effects.cancel_at("during_review_recheck");
        coordinator.effects.answer(offered.token, None).unwrap();
        assert!(matches!(
            finish(&mut coordinator),
            Ok(RunOutcome::Unadmitted(_))
        ));
        assert_eq!(fs::read(layout.record()).unwrap(), before);
        assert!(!layout.command_directory.exists());
        assert!(coordinator.effects.state.borrow().settlement.is_none());
    }

    #[test]
    fn review_resource_identity_drift_refuses_reconciliation() {
        use std::os::unix::fs::MetadataExt;
        let (_temporary, layout, mut coordinator) = review_fixture(None);
        let path = layout.state_directory.join("resources");
        fs::create_dir(&path).unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o700)).unwrap();
        let metadata = fs::metadata(&path).unwrap();
        let mut record = super::super::InstallationRecord::default();
        record.resources.parent = Some(super::super::resources::ParentObservation {
            device: metadata.dev(),
            inode: metadata.ino(),
            uid: metadata.uid(),
        });
        record.resources.pending = Some(super::super::resources::ResourceEffect::Register {
            pair: "/nix/store/00000000000000000000000000000000-pair".into(),
            parent: "candidate-a".into(),
        });
        let before = serde_json::to_vec(&record).unwrap();
        fs::write(layout.record(), &before).unwrap();
        fs::set_permissions(layout.record(), fs::Permissions::from_mode(0o600)).unwrap();
        let offered = offer(&mut coordinator);
        fs::rename(&path, layout.state_directory.join("old-resources")).unwrap();
        fs::create_dir(&path).unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o700)).unwrap();
        coordinator.effects.answer(offered.token, None).unwrap();
        assert!(matches!(
            finish(&mut coordinator),
            Ok(RunOutcome::Unadmitted(UnadmittedOperation {
                reason: UnadmittedReason::ReviewChanged,
                ..
            }))
        ));
        assert_eq!(fs::read(layout.record()).unwrap(), before);
        assert!(!layout.command_directory.exists());
    }
    #[test]
    fn review_every_closed_mutation_is_refused_during_inspection_and_recheck() {
        let (_temporary, layout, _coordinator) = review_fixture(None);
        let effects = Effects::for_tests();
        let lease = RootLease::acquire(&layout.lock()).unwrap();
        retain(&effects, &lease);
        let resource_path = layout.state_directory.join("fixture-resources");
        let directory =
            super::super::resource_io::ResourceDirectory::open(resource_path.clone()).unwrap();
        std::os::unix::fs::symlink("target", resource_path.join("owned")).unwrap();
        let observed = directory.inventory(2).unwrap().remove("owned").unwrap();
        drop(directory);
        for recheck in [false, true] {
            if recheck {
                let mut future = Box::pin(effects.review(ReviewSummary {
                    operation: ReviewOperation::Recover,
                    version: None,
                    recovery: RecoveryPending::None,
                    changes: vec![],
                }));
                assert!(poll(&effects, future.as_mut()).is_pending());
                let offered = effects.take_review().unwrap();
                effects.answer(offered.token, None).unwrap();
                drive(&effects, future).unwrap();
            }
            let (entered, _) = std::sync::mpsc::sync_channel(1);
            let (_, release) = std::sync::mpsc::sync_channel(1);
            let jobs = vec![
                FilesystemJob::EnsureCommandDirectory(layout.command_directory.clone()),
                FilesystemJob::PublishRecord {
                    path: layout.record(),
                    bytes: b"forbidden".to_vec(),
                    committing: false,
                },
                FilesystemJob::PublishReviewedRecord {
                    layout: layout.clone(),
                    expected: None,
                    active: None,
                    resources: None,
                    bytes: b"forbidden".to_vec(),
                    committing: false,
                },
                FilesystemJob::EnsureCommandLink {
                    layout: layout.clone(),
                    command: super::super::ManagedCommand::Neo,
                },
                FilesystemJob::RemoveCommandLink {
                    path: layout.command_directory.join("neo"),
                    target: layout.expected_command_target("neo"),
                },
                FilesystemJob::PrepareActiveLink {
                    layout: layout.clone(),
                    target: layout.state_directory.join("pair"),
                },
                FilesystemJob::RenameActiveLink {
                    layout: layout.clone(),
                    expected: None,
                    target: layout.state_directory.join("pair"),
                },
                FilesystemJob::RemoveNextLink {
                    layout: layout.clone(),
                    expected: layout.state_directory.join("pair"),
                },
                FilesystemJob::EnsureSnapshotDirectory(layout.state_directory.join("snapshots")),
                FilesystemJob::PublishSnapshot {
                    path: layout.state_directory.join("metadata"),
                    bytes: vec![1],
                },
                FilesystemJob::PruneSnapshot {
                    directory: layout.state_directory.join("snapshots"),
                    files: vec![],
                },
                FilesystemJob::OpenResourceDirectory(layout.state_directory.join("resources")),
                FilesystemJob::RemoveResource {
                    directory: super::super::resource_io::ResourceDirectory::open_existing(
                        resource_path.clone(),
                    )
                    .unwrap()
                    .unwrap(),
                    name: "owned".into(),
                    expected: observed.clone(),
                    limit: 2,
                },
                FilesystemJob::GatedWrite {
                    path: layout.state_directory.join("forbidden"),
                    entered,
                    release,
                },
            ];
            for job in jobs {
                assert!(matches!(
                    drive(&effects, effects.filesystem(job)),
                    Err(TransactionError::InvalidState(
                        "effect requires accepted review"
                    ))
                ));
            }
            for request in [
                process::WorkerRequest::ImportPolicyHelper {
                    client: layout.state_directory.join("nix"),
                    source: layout.state_directory.join("installer"),
                },
                process::WorkerRequest::AcquireRetained {
                    pair: "forbidden".into(),
                    candidate: "forbidden".into(),
                    offline: false,
                },
                process::WorkerRequest::RegisterExact {
                    pair: "forbidden".into(),
                    root: process::RootName::Candidate("forbidden".into()),
                },
                process::WorkerRequest::ProbeTool {
                    output: "forbidden".into(),
                    tool: process::Tool::Neo,
                    probe: process::Probe::Version,
                },
                process::WorkerRequest::BuildProfile {
                    output: layout.state_directory.join("pair"),
                },
                process::WorkerRequest::Fixture {
                    name: "success".into(),
                },
            ] {
                assert!(matches!(
                    drive(&effects, effects.process(request)),
                    Err(TransactionError::InvalidState(
                        "effect requires accepted review"
                    ))
                ));
            }
            assert!(!layout.record().exists());
            assert!(!layout.command_directory.exists());
            assert_eq!(
                fs::read_link(resource_path.join("owned")).unwrap(),
                std::path::Path::new("target")
            );
        }
    }
    #[test]
    fn review_settlement_requires_its_own_admitted_journal() {
        let (_temporary, owner, lease) = setup();
        test_with(&owner.effects, &lease, async {
            owner
                .effects
                .filesystem(FilesystemJob::EnsureCommandDirectory(
                    owner.layout.command_directory.clone(),
                ))
                .await
                .unwrap();
            for kind in [
                SettlementKind::Activation,
                SettlementKind::Resource,
                SettlementKind::Recovery,
            ] {
                assert!(owner.effects.settlement(kind).is_err());
            }
            let mut record = super::super::InstallationRecord::default();
            record.resources.pending = Some(super::super::resources::ResourceEffect::Register {
                pair: "/nix/store/00000000000000000000000000000000-pair".into(),
                parent: "candidate-a".into(),
            });
            owner.write_record(&record, false).await.unwrap();
            assert!(
                owner
                    .effects
                    .settlement(SettlementKind::Activation)
                    .is_err()
            );
            assert!(owner.effects.settlement(SettlementKind::Recovery).is_err());
            let settlement = owner.effects.settlement(SettlementKind::Resource).unwrap();
            settlement.finish_settlement().unwrap();
            assert!(owner.effects.settlement(SettlementKind::Resource).is_err());
        });
    }
    #[test]
    fn review_reported_journal_error_recovers_only_its_attempt_and_preserves_primary() {
        for foreign in [false, true] {
            let (_temporary, owner, lease) = setup();
            let before = owner.next_receipt(
                None,
                PairIdentity {
                    version: "1.0.0".into(),
                    output: owner.layout.state_directory.join("pair-a"),
                    selection: None,
                },
            );
            test_with(
                &owner.effects,
                &lease,
                owner.switch_pair(
                    None,
                    before.clone(),
                    Operation::Install(InstallSelection::ExactVersion),
                ),
            )
            .unwrap();
            owner.effects.cancel_at("after_journal_publication_error");
            retain(&owner.effects, &lease);
            let after = owner.next_receipt(
                Some(&before),
                PairIdentity {
                    version: "2.0.0".into(),
                    output: owner.layout.state_directory.join("pair-b"),
                    selection: None,
                },
            );
            let mut future = Box::pin(owner.switch_pair(
                Some(before.clone()),
                after,
                Operation::Update(InstallSelection::ExactVersion),
            ));
            let mut changed = false;
            let result = loop {
                if let Poll::Ready(result) = poll(&owner.effects, future.as_mut()) {
                    break result;
                }
                if foreign && !changed {
                    let record: super::super::InstallationRecord =
                        serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
                    if record.activation.is_some() {
                        let path = owner.layout.command_directory.join("neo");
                        fs::remove_file(&path).unwrap();
                        std::os::unix::fs::symlink("/foreign-target", path).unwrap();
                        changed = true;
                    }
                }
                owner.effects.wait();
            };
            drop(future);
            if foreign {
                assert!(changed);
                assert_eq!(
                    result,
                    Err(TransactionError::ActivationRecoveryFailed {
                        primary: Box::new(TransactionError::Io),
                        recovery: Box::new(TransactionError::CommandLinkConflict)
                    })
                );
                assert_eq!(
                    fs::read_link(owner.layout.command_directory.join("neo")).unwrap(),
                    std::path::Path::new("/foreign-target")
                );
            } else {
                assert_eq!(result, Err(TransactionError::Io));
                let record: super::super::InstallationRecord =
                    serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
                assert!(record.activation.is_none());
                assert_eq!(record.committed, Some(before.clone()));
                assert!(owner.effects.state.borrow().settlement.is_none());
            }
            assert_eq!(
                fs::read_link(owner.layout.active_link()).unwrap(),
                before.active.output
            );
            assert!(!owner.layout.active_next_link().exists());
            assert!(matches!(
                RootLease::acquire(&lease.path),
                Err(TransactionError::Busy)
            ));
            assert!(
                owner
                    .effects
                    .settlement(SettlementKind::Activation)
                    .is_err()
            );
        }
    }

    #[test]
    fn review_old_publication_cannot_authorize_later_settlement() {
        let (_temporary, owner, lease) = setup();
        test_with(&owner.effects, &lease, async {
            let mut record = super::super::InstallationRecord::default();
            record.resources.pending = Some(super::super::resources::ResourceEffect::Register {
                pair: "/nix/store/00000000000000000000000000000000-pair".into(),
                parent: "candidate-a".into(),
            });
            owner.write_record(&record, false).await.unwrap();
            owner.read_record().await.unwrap();
            assert!(owner.effects.settlement(SettlementKind::Resource).is_err());
        });
    }
}

#[cfg(test)]
pub(super) fn dashboard_gated_fixture(
    layout: Layout,
    entered: std::sync::mpsc::SyncSender<()>,
    release: std::sync::mpsc::Receiver<()>,
) -> Coordinator {
    let effects = Effects::new(Cancellation::default());
    let flow = effects.clone();
    let continuation = Box::pin(async move {
        let FilesystemResult::Startup { lease, .. } = flow
            .filesystem(FilesystemJob::Startup(Some(layout.clone())))
            .await?
        else {
            return Err(TransactionError::InvalidState("fixture startup type"));
        };
        flow.state.borrow_mut().lease = Some(lease);
        let identity = match flow
            .review(ReviewSummary {
                operation: ReviewOperation::Recover,
                version: None,
                recovery: RecoveryPending::None,
                changes: vec![ReviewedChange::Recovery],
            })
            .await
        {
            Ok(identity) => identity,
            Err(TransactionError::StopRequested) => return unadmitted(&flow),
            Err(error) => return Err(error),
        };
        flow.admit_reviewed(identity)?;
        flow.observe(super::observation::Activity::Recovering);
        flow.filesystem(FilesystemJob::GatedWrite {
            path: layout.state_directory.join("admitted-write"),
            entered,
            release,
        })
        .await?;
        if flow.stopped() {
            stopped(&flow, StopObservation::ObservedNoPair)
        } else {
            Ok(RunOutcome::Complete(None))
        }
    });
    Coordinator {
        effects,
        continuation,
    }
}

#[cfg(test)]
pub(super) fn dashboard_process_fixture(layout: Layout, resistant: bool) -> Coordinator {
    let effects = Effects::new(Cancellation::default());
    let flow = effects.clone();
    let continuation = Box::pin(async move {
        let FilesystemResult::Startup { lease, .. } = flow
            .filesystem(FilesystemJob::Startup(Some(layout.clone())))
            .await?
        else {
            return Err(TransactionError::InvalidState("fixture startup type"));
        };
        flow.state.borrow_mut().lease = Some(lease);
        let identity = match flow
            .review(ReviewSummary {
                operation: ReviewOperation::Recover,
                version: None,
                recovery: RecoveryPending::None,
                changes: vec![ReviewedChange::Recovery],
            })
            .await
        {
            Ok(identity) => identity,
            Err(TransactionError::StopRequested) => return unadmitted(&flow),
            Err(error) => return Err(error),
        };
        flow.admit_reviewed(identity)?;
        flow.observe(super::observation::Activity::Recovering);
        let output = flow
            .process(process::WorkerRequest::Fixture {
                name: if resistant {
                    "dashboard-private-resistant"
                } else {
                    "dashboard-private-success"
                }
                .into(),
            })
            .await?;
        assert!(String::from_utf8_lossy(&output.stdout).contains("PRIVATE-DASHBOARD-STDOUT"));
        assert!(String::from_utf8_lossy(&output.stderr).contains("PRIVATE-DASHBOARD-STDERR"));
        std::fs::write(layout.state_directory.join("private-capture.json"), serde_json::to_vec(&serde_json::json!({
            "stdout": String::from_utf8_lossy(&output.stdout), "stderr": String::from_utf8_lossy(&output.stderr),
        })).unwrap()).unwrap();
        Err(TransactionError::Verification(
            "PRIVATE-DASHBOARD-SEMANTIC-ERROR",
        ))
    });
    Coordinator {
        effects,
        continuation,
    }
}

#[cfg(test)]
impl Coordinator {
    pub(super) fn dashboard_kill_worker(&mut self) -> u32 {
        let mut state = self.effects.state.borrow_mut();
        let EffectSlot::Active(ActiveEffect::Process { owner, .. }) = &mut state.slot else {
            panic!("fixture requires active worker")
        };
        owner.dashboard_kill_worker()
    }
}

#[cfg(test)]
pub(super) fn dashboard_owned_fixture(layout: Layout, request: snapshot::Request) -> Coordinator {
    let effects = Effects::new(Cancellation::default());
    let flow = effects.clone();
    let continuation = Box::pin(async move {
        let FilesystemResult::Startup { lease, layout } = flow
            .filesystem(FilesystemJob::Startup(Some(layout)))
            .await?
        else {
            return Err(TransactionError::InvalidState("fixture startup type"));
        };
        flow.state.borrow_mut().lease = Some(lease);
        let owner = InstallationOwner {
            layout,
            effects: flow.clone(),
            fixture: true,
        };
        match owner
            .run_owned(request, runtime::Executor::NativeInstaller)
            .await
        {
            Ok(outcome) if !flow.stopped() => Ok(RunOutcome::Complete(outcome)),
            Ok(_) | Err(TransactionError::StopRequested) => {
                if !flow.mutation_admitted() {
                    return unadmitted(&flow);
                }
                let observation = owner.observe_stopped().await?;
                stopped(&flow, observation)
            }
            Err(error) => Err(error),
        }
    });
    Coordinator {
        effects,
        continuation,
    }
}
