use super::filesystem::{FilesystemJob, FilesystemResult, FilesystemTask};
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

pub(crate) enum ReviewOperation {
    Install,
    Update,
    Rollback,
    Recover,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ReviewedChange {
    Recovery,
    CommandDirectory,
    CommandLinks,
    Selection,
    Retention,
    Activation,
    Cleanup,
}

pub(super) struct ReviewOffer {
    pub(super) summary: ReviewSummary,
    token: ReviewToken,
}
pub(super) struct ReviewToken {
    owner: Weak<RefCell<EffectState>>,
    identity: u64,
}

enum ReviewGate {
    Inspecting,
    Awaiting {
        identity: u64,
        offer: Option<ReviewOffer>,
    },
    Rechecking(u64),
    Accepted {
        identity: u64,
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
}

struct Quiescent;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SettlementKind {
    Recovery,
    Activation,
    Resource,
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
    lease: Option<RootLease>,
    slot: EffectSlot,
    settlement: Option<SettlementPermit>,
    next_identity: u64,
    review: ReviewGate,
    recovery: RecoveryPending,
    journal_attempt: Option<JournalAttempt>,
}

struct JournalAttempt {
    review: u64,
    kind: SettlementKind,
    path: std::path::PathBuf,
    before: Option<Vec<u8>>,
    bytes: Vec<u8>,
    publication: Result<(), TransactionError>,
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
    Completed(Result<EffectResult, TransactionError>),
}

enum EffectRequest {
    Filesystem(FilesystemJob, Admission),
    Process(process::WorkerRequest),
}

impl EffectRequest {
    fn mutates(&self) -> bool {
        match self {
            Self::Filesystem(job, _) => job.mutates(),
            Self::Process(request) => request.mutates(),
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
                if let ReviewGate::Accepted { progress, .. } = &mut state.review {
                    *progress = AcceptedProgress::MutationAdmitted;
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
    pub(super) fn recovery_pending(&self, pending: RecoveryPending) {
        self.state.borrow_mut().recovery = pending;
    }

    pub(super) async fn review(&self, summary: ReviewSummary) -> Result<u64, TransactionError> {
        self.checkpoint()?;
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
        state.review = ReviewGate::Accepted {
            identity,
            progress: AcceptedProgress::NoMutationAdmitted,
        };
        Ok(())
    }

    pub(super) fn review_changed(&self) -> TransactionError {
        self.state.borrow_mut().review = ReviewGate::Stopped(UnadmittedReason::ReviewChanged);
        TransactionError::StopRequested
    }

    fn mutation_admitted(&self) -> bool {
        matches!(
            self.state.borrow().review,
            ReviewGate::Accepted {
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
                lease: None,
                slot: EffectSlot::Idle,
                settlement: None,
                next_identity: 0,
                review: ReviewGate::Inspecting,
                recovery: RecoveryPending::Unknown,
                journal_attempt: None,
            })),
            cancellation,
            admission: Admission::Requested,
        }
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
        match &self.admission {
            Admission::Requested if self.stopped() => Err(TransactionError::StopRequested),
            Admission::Requested => {
                let state = self.state.borrow();
                match state.review {
                    ReviewGate::Accepted { .. } => Ok(()),
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
                if before.activation.is_some() || before.resources.pending.is_some() {
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

    fn advance_effect(&self) {
        let slot = {
            let mut state = self.state.borrow_mut();
            std::mem::replace(&mut state.slot, EffectSlot::Idle)
        };
        let next = match slot {
            EffectSlot::Admitted(request) => {
                let permission = if request.mutates() && self.mutation_admitted() {
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
                    Err(error) => EffectSlot::Completed(Err(error)),
                }
            }
            EffectSlot::Active(mut active) => {
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
    fn new(
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
    }

    fn advance(&mut self) -> Poll<Result<RunOutcome, TransactionError>> {
        self.effects.advance_effect();
        let waker = Waker::from(Arc::new(CallerWake));
        let mut context = Context::from_waker(&waker);
        match self.continuation.as_mut().poll(&mut context) {
            Poll::Ready(result) => {
                if !matches!(self.effects.state.borrow().slot, EffectSlot::Idle) {
                    return Poll::Ready(Err(TransactionError::InvalidState(
                        "owner finished with an effect",
                    )));
                }
                Poll::Ready(result)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

pub(crate) fn run(
    request: snapshot::Request,
    executor: runtime::Executor,
    consent: bool,
) -> Result<RunOutcome, TransactionError> {
    let mut coordinator = Coordinator::new(request, executor, None);
    drive_plain(&mut coordinator, consent, &mut std::io::stdout().lock())
}

fn drive_plain(
    coordinator: &mut Coordinator,
    consent: bool,
    output: &mut impl std::io::Write,
) -> Result<RunOutcome, TransactionError> {
    loop {
        if let Poll::Ready(result) = coordinator.advance() {
            return result;
        }
        if let Some(offer) = coordinator.effects.take_review() {
            crate::write_review(output, &offer.summary)
                .and_then(|_| output.flush())
                .map_err(|_| TransactionError::Io)?;
            coordinator.effects.answer(
                offer.token,
                (!consent).then_some(UnadmittedReason::ConsentRequired),
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
    let state = effects.state.borrow();
    if !matches!(state.slot, EffectSlot::Idle) || state.settlement.is_some() {
        return Err(TransactionError::InvalidState(
            "stopping has unsettled work",
        ));
    }
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
        let temporary = tempfile::tempdir().unwrap();
        let state = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let layout = Layout::new(state.clone(), state.join("bin"));
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
                if reason == UnadmittedReason::Cancelled {
                    coordinator.request_cancel();
                } else {
                    coordinator
                        .effects
                        .answer(offered.token, Some(reason))
                        .unwrap();
                }
                let RunOutcome::Unadmitted(stopped) = finish(&mut coordinator).unwrap() else {
                    panic!("withheld review admitted work");
                };
                assert_eq!(stopped.reason, reason);
                assert_eq!(stopped.recovery, RecoveryPending::Activation);
                assert_eq!(fs::read(layout.record()).unwrap(), before);
                assert_eq!(fs::read_link(layout.active_link()).unwrap(), active);
                assert!(!layout.command_directory.exists());
            }
        }
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
            assert_eq!(consent, yes);
            let before = fs::read(layout.record()).unwrap();
            let mut output = Vec::new();
            let result = drive_plain(&mut coordinator, consent, &mut output).unwrap();
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
                    path: layout.command_directory.join("neo"),
                    target: layout.expected_command_target("neo"),
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
