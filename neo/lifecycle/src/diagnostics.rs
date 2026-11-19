mod store;

use crate::{Exit, identity::BuildIdentity, installation};
use serde::Serialize;
use std::io::{self, Write};
use std::time::Instant;

const MAX_PAYLOAD_BYTES: usize = 12 * 1024;

#[derive(Clone, Copy, Serialize)]
#[serde(tag = "executor", content = "route", rename_all = "snake_case")]
pub(crate) enum Invocation {
    Native(NativeRoute),
    Installed(RequestRoute),
}

#[derive(Clone, Copy, Serialize)]
#[serde(tag = "kind", content = "operation", rename_all = "snake_case")]
pub(crate) enum NativeRoute {
    NoArguments,
    Help,
    Version,
    Operation(RequestRoute),
    Unknown,
}

#[derive(Clone, Copy, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RequestRoute {
    Uninstall,
    Install,
    Update,
    Rollback,
}

pub(crate) struct InvocationResult {
    pub(crate) exit: Exit,
    pub(crate) outcome: DiagnosticOutcome,
}

#[derive(Serialize)]
pub(super) struct Report {
    schema_version: u8,
    invocation: Invocation,
    started_at_ms: u64,
    build: DiagnosticBuild,
    state: State,
}

#[derive(Serialize)]
struct DiagnosticBuild {
    product_version: Option<EarnedVersion>,
    source_revision: SourceRevision,
    target: Target,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum SourceRevision {
    Development,
    Revision(String),
    Unknown,
}

#[derive(Serialize)]
enum Target {
    #[serde(rename = "aarch64-apple-darwin")]
    AppleSilicon,
    #[serde(rename = "aarch64-unknown-linux-gnu")]
    LinuxArm64,
    #[serde(rename = "x86_64-unknown-linux-gnu")]
    LinuxX64,
    #[serde(rename = "unsupported")]
    Unsupported,
}

impl DiagnosticBuild {
    fn current() -> Self {
        let identity = BuildIdentity::current();
        let source_revision = match identity.source_revision {
            "development" => SourceRevision::Development,
            value
                if matches!(value.len(), 40 | 64)
                    && value.bytes().all(|byte| byte.is_ascii_hexdigit()) =>
            {
                SourceRevision::Revision(value.to_owned())
            }
            _ => SourceRevision::Unknown,
        };
        let target = match identity.target {
            "aarch64-apple-darwin" => Target::AppleSilicon,
            "aarch64-unknown-linux-gnu" => Target::LinuxArm64,
            "x86_64-unknown-linux-gnu" => Target::LinuxX64,
            _ => Target::Unsupported,
        };
        Self {
            product_version: EarnedVersion::from_observation(identity.product_version),
            source_revision,
            target,
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum State {
    Started,
    Finished {
        finished_at_ms: Option<u64>,
        duration_ms: u64,
        exit_code: i32,
        outcome: DiagnosticOutcome,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Phase {
    Inspecting,
    CheckingNix,
    Selecting,
    PreparingReview,
    Review,
    Rechecking,
    Recovering,
    PreparingPair,
    VerifyingPair,
    Activating,
    Finishing,
    Stopping,
    Unadmitted,
    Finished,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CompletedStep {
    Prerequisites,
    NixPolicyObservation,
    NixPolicyApplication,
    CandidateSelection,
    PairVerification,
    Recovery,
    FinalSettlement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum AppliedChange {
    NixPolicyApplied,
    ActivationCommitted,
    ActivationRecovered,
    ActivationReverted,
    RecoveryRepairsSettled,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ReviewAnswer {
    Accepted,
    Declined,
    ConsentRequired,
    Cancelled,
}

#[derive(Clone, Debug, Serialize)]
pub(crate) struct PhaseEvent {
    pub(crate) phase: Phase,
    pub(crate) elapsed_ms: u64,
}

#[derive(Clone, Debug, Serialize)]
pub(crate) struct ReviewFact {
    pub(crate) entered_ms: u64,
    pub(crate) wait_ms: Option<u64>,
    pub(crate) answer: Option<ReviewAnswer>,
    pub(crate) admitted_ms: Option<u64>,
}

#[derive(Clone, Debug, Serialize)]
pub(crate) struct InvocationFacts {
    pub(crate) elapsed_ms: u64,
    pub(crate) phase: Phase,
    pub(crate) events: Vec<PhaseEvent>,
    pub(crate) events_truncated: bool,
    pub(crate) review: Option<ReviewFact>,
    pub(crate) completed_steps: Vec<CompletedStep>,
    pub(crate) applied_changes: Vec<AppliedChange>,
    pub(crate) installer_retry_policy: RetryPolicy,
    pub(crate) upstream_attempts: UpstreamAttempts,
    pub(crate) other_applied_changes: ChangeObservation,
    pub(crate) cancelled_ms: Option<u64>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ChangeObservation {
    Unknown,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RetryPolicy {
    NotImplemented,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum UpstreamAttempts {
    Unknown,
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum DiagnosticOutcome {
    HelpDisplayed,
    VersionDisplayed,
    InvalidArguments,
    InvalidRequest,
    Lifecycle {
        request: RequestRoute,
        operation: OperationOutcome,
        presentation: Option<PresentationOutcome>,
        recovery_advice: RecoveryAdvice,
        observations: Option<InvocationFacts>,
    },
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum OperationOutcome {
    Complete {
        active_version: Option<EarnedVersion>,
        previous: PreviousPair,
    },
    Unadmitted {
        reason: UnadmittedReason,
        pending_recovery: PendingRecovery,
    },
    Cancelled {
        observation: StopObservation,
    },
    Failed {
        error: ErrorFact,
        activation_recovery: Option<ActivationRecovery>,
    },
}

#[derive(Serialize)]
#[serde(transparent)]
pub(crate) struct EarnedVersion(String);

impl EarnedVersion {
    fn from_observation(value: &str) -> Option<Self> {
        if value.len() > 64 {
            return None;
        }
        let version = semver::Version::parse(value).ok()?;
        (version.pre.is_empty() && version.build.is_empty() && version.to_string() == value)
            .then(|| Self(value.to_owned()))
    }
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PreviousPair {
    NotReported,
    None,
    Missing,
    Unchecked,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum UnadmittedReason {
    ConsentRequired,
    Declined,
    Cancelled,
    ReviewChanged,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PendingRecovery {
    Uninstall,
    Shell,
    Unknown,
    None,
    Activation,
    Resources,
    ActivationAndResources,
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum StopObservation {
    NotRequired,
    NoPair,
    Pair { version: Option<EarnedVersion> },
}

#[derive(Serialize)]
pub(crate) struct ActivationRecovery {
    primary: ErrorFact,
    recovery: ErrorFact,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "reason", rename_all = "snake_case")]
pub(crate) enum ErrorFact {
    Io,
    StopRequested,
    CleanupUnknown,
    ActivationRecoveryFailed,
    Busy,
    UnsafePath,
    InvalidPair,
    InvalidState,
    CommandLinkConflict,
    CommandPathConflict(installation::ManagedCommand),
    ForeignActiveTarget,
    NoInstallation,
    NoPreviousPair,
    Verification,
    NixBlocked(NixReason),
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum NixReason {
    MissingClient,
    UnprovedClient,
    UnprovedPlatform,
    InactiveService,
    UnprovedService,
    DaemonUnavailable,
    DaemonMismatch,
    ConfigurationOwnerUnknown,
}

#[derive(Serialize)]
pub(crate) struct PresentationOutcome {
    primary: PresentationReason,
    restored: bool,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum PresentationReason {
    Input,
    Output,
    Restore,
    Panic,
    Setup,
    DisplayData,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RecoveryAdvice {
    None,
    ReviewBeforeRetry,
    InspectInstallationState,
    CheckNixInstallation,
    WaitForOtherInvocation,
    CheckTerminal,
}

impl ErrorFact {
    fn project(error: &installation::TransactionError) -> Self {
        use installation::TransactionError as E;
        match error {
            E::Io => Self::Io,
            E::StopRequested => Self::StopRequested,
            E::CleanupUnknown => Self::CleanupUnknown,
            E::ActivationRecoveryFailed { .. } => Self::ActivationRecoveryFailed,
            E::Busy => Self::Busy,
            E::UnsafePath => Self::UnsafePath,
            E::InvalidPair => Self::InvalidPair,
            E::InvalidState(_) => Self::InvalidState,
            E::CommandLinkConflict => Self::CommandLinkConflict,
            E::CommandPathConflict { command, .. } => Self::CommandPathConflict(*command),
            E::ForeignActiveTarget => Self::ForeignActiveTarget,
            E::NoInstallation => Self::NoInstallation,
            E::NoPreviousPair => Self::NoPreviousPair,
            E::ShellRecoveryCompleted
            | E::ShellUnsupported
            | E::ShellZdotdirUnsupported
            | E::ShellVerificationFailed
            | E::Verification(_) => Self::Verification,
            E::NixBlocked(reason) => {
                use installation::nix_owner::NixBlock as N;
                Self::NixBlocked(match reason {
                    N::MissingClient => NixReason::MissingClient,
                    N::UnprovedClient => NixReason::UnprovedClient,
                    N::UnprovedPlatform => NixReason::UnprovedPlatform,
                    N::InactiveService => NixReason::InactiveService,
                    N::UnprovedService => NixReason::UnprovedService,
                    N::DaemonUnavailable => NixReason::DaemonUnavailable,
                    N::DaemonMismatch => NixReason::DaemonMismatch,
                    N::ConfigurationOwnerUnknown => NixReason::ConfigurationOwnerUnknown,
                })
            }
        }
    }
}

impl DiagnosticOutcome {
    pub(crate) fn from_drive_report(
        request: RequestRoute,
        report: &installation::DriveReport,
    ) -> Self {
        use installation::{RecoveryPending as R, RunOutcome as O, TransactionError as E};
        let operation = match &report.operation {
            Ok(O::Complete(outcome)) => OperationOutcome::Complete {
                active_version: outcome
                    .as_ref()
                    .and_then(|value| EarnedVersion::from_observation(&value.version)),
                previous: match outcome {
                    None => PreviousPair::NotReported,
                    Some(value) if !value.has_previous => PreviousPair::None,
                    Some(value) if value.previous_missing => PreviousPair::Missing,
                    Some(_) => PreviousPair::Unchecked,
                },
            },
            Ok(O::Unadmitted(value)) => OperationOutcome::Unadmitted {
                reason: match value.reason {
                    installation::UnadmittedReason::ConsentRequired => {
                        UnadmittedReason::ConsentRequired
                    }
                    installation::UnadmittedReason::Declined => UnadmittedReason::Declined,
                    installation::UnadmittedReason::Cancelled => UnadmittedReason::Cancelled,
                    installation::UnadmittedReason::ReviewChanged => {
                        UnadmittedReason::ReviewChanged
                    }
                },
                pending_recovery: match value.recovery {
                    R::Uninstall => PendingRecovery::Uninstall,
                    R::Unknown => PendingRecovery::Unknown,
                    R::None => PendingRecovery::None,
                    R::Activation => PendingRecovery::Activation,
                    R::Resources => PendingRecovery::Resources,
                    R::ActivationAndResources => PendingRecovery::ActivationAndResources,
                    R::Shell => PendingRecovery::Shell,
                },
            },
            Ok(O::Stopped(value)) => OperationOutcome::Cancelled {
                observation: match &value.observation {
                    installation::StopObservation::NotRequired => StopObservation::NotRequired,
                    installation::StopObservation::ObservedNoPair => StopObservation::NoPair,
                    installation::StopObservation::ObservedPair(pair) => StopObservation::Pair {
                        version: EarnedVersion::from_observation(&pair.version),
                    },
                },
            },
            Err(error) => OperationOutcome::Failed {
                error: ErrorFact::project(error),
                activation_recovery: match error {
                    E::ActivationRecoveryFailed { primary, recovery } => Some(ActivationRecovery {
                        primary: ErrorFact::project(primary),
                        recovery: ErrorFact::project(recovery),
                    }),
                    _ => None,
                },
            },
        };
        let presentation = report.presentation.map(|failure| PresentationOutcome {
            primary: match failure.primary {
                installation::PresentationFailureKind::Input => PresentationReason::Input,
                installation::PresentationFailureKind::Output => PresentationReason::Output,
                installation::PresentationFailureKind::Restore => PresentationReason::Restore,
                installation::PresentationFailureKind::Panic => PresentationReason::Panic,
                installation::PresentationFailureKind::Setup => PresentationReason::Setup,
                installation::PresentationFailureKind::DisplayData => {
                    PresentationReason::DisplayData
                }
            },
            restored: failure.restored,
        });
        let recovery_advice = match &report.operation {
            Err(E::NixBlocked(_)) => RecoveryAdvice::CheckNixInstallation,
            Err(E::Busy) => RecoveryAdvice::WaitForOtherInvocation,
            Err(_) => RecoveryAdvice::InspectInstallationState,
            _ if presentation.is_some() => RecoveryAdvice::CheckTerminal,
            Ok(O::Unadmitted(_) | O::Stopped(_)) => RecoveryAdvice::ReviewBeforeRetry,
            Ok(O::Complete(_)) => RecoveryAdvice::None,
        };
        Self::Lifecycle {
            request,
            operation,
            presentation,
            recovery_advice,
            observations: report.observations.clone(),
        }
    }
}

impl Report {
    pub(super) fn started(invocation: Invocation, now: u64) -> Self {
        Self {
            schema_version: 1,
            invocation,
            started_at_ms: now,
            build: DiagnosticBuild::current(),
            state: State::Started,
        }
    }

    pub(super) fn started_at_ms(&self) -> u64 {
        self.started_at_ms
    }

    pub(super) fn encode(&self) -> Result<Vec<u8>, ()> {
        struct Bounded(Vec<u8>);
        impl Write for Bounded {
            fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
                if bytes.len() > MAX_PAYLOAD_BYTES.saturating_sub(self.0.len()) {
                    return Err(io::Error::other("diagnostic payload limit"));
                }
                self.0.extend_from_slice(bytes);
                Ok(bytes.len())
            }
            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }
        let mut output = Bounded(Vec::new());
        serde_json::to_writer_pretty(&mut output, self).map_err(|_| ())?;
        Ok(output.0)
    }
}

pub(crate) fn record_public(
    invocation: Invocation,
    run: impl FnOnce() -> InvocationResult,
) -> Exit {
    let clock = Instant::now();
    let started = store::now_ms().map(|now| Report::started(invocation, now));
    let open = started.as_ref().map_err(|_| ()).and_then(store::begin);
    let mut result = run();
    if result.exit.code == 0
        && matches!(
            &result.outcome,
            DiagnosticOutcome::Lifecycle {
                request: RequestRoute::Uninstall,
                operation: OperationOutcome::Complete { .. },
                presentation: None,
                ..
            }
        )
    {
        if open
            .as_ref()
            .is_ok_and(|record| record.clear_retained().is_ok())
        {
            return result.exit;
        }
        result.exit.code = 1;
        let _ = writeln!(
            io::stderr().lock(),
            "{}",
            crate::english().diagnostic_uninstall_cleanup_failed
        );
    }
    let saved = match (started, open) {
        (Ok(mut report), Ok(record)) => {
            report.state = State::Finished {
                finished_at_ms: store::now_ms().ok(),
                duration_ms: u64::try_from(clock.elapsed().as_millis()).unwrap_or(u64::MAX),
                exit_code: result.exit.code,
                outcome: result.outcome,
            };
            record.finish(&report)
        }
        _ => Err(()),
    };
    if result.exit.code != 0 {
        let copy = crate::english();
        let message = match saved.as_deref().ok().and_then(std::path::Path::to_str) {
            Some(path) => copy.diagnostic_report_saved.replace("{path}", path),
            None => copy.diagnostic_report_unavailable.clone(),
        };
        let _ = writeln!(io::stderr().lock(), "{message}");
    }
    result.exit
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn report_with_a_full_timeline_fits_the_payload_bound() {
        let mut report = Report::started(Invocation::Installed(RequestRoute::Rollback), u64::MAX);
        report.state = State::Finished {
            finished_at_ms: Some(u64::MAX),
            duration_ms: u64::MAX,
            exit_code: 130,
            outcome: DiagnosticOutcome::Lifecycle {
                request: RequestRoute::Rollback,
                operation: OperationOutcome::Cancelled {
                    observation: StopObservation::Pair {
                        version: EarnedVersion::from_observation(
                            "18446744073709551615.18446744073709551615.18446744073709551615",
                        ),
                    },
                },
                presentation: Some(PresentationOutcome {
                    primary: PresentationReason::DisplayData,
                    restored: false,
                }),
                recovery_advice: RecoveryAdvice::InspectInstallationState,
                observations: Some(InvocationFacts {
                    elapsed_ms: u64::MAX,
                    phase: Phase::PreparingReview,
                    events: vec![
                        PhaseEvent {
                            phase: Phase::PreparingReview,
                            elapsed_ms: u64::MAX
                        };
                        64
                    ],
                    events_truncated: true,
                    review: Some(ReviewFact {
                        entered_ms: u64::MAX,
                        wait_ms: Some(u64::MAX),
                        answer: Some(ReviewAnswer::ConsentRequired),
                        admitted_ms: Some(u64::MAX),
                    }),
                    completed_steps: vec![
                        CompletedStep::Prerequisites,
                        CompletedStep::CandidateSelection,
                        CompletedStep::PairVerification,
                        CompletedStep::Recovery,
                        CompletedStep::FinalSettlement,
                    ],
                    applied_changes: vec![
                        AppliedChange::ActivationCommitted,
                        AppliedChange::ActivationRecovered,
                        AppliedChange::ActivationReverted,
                        AppliedChange::RecoveryRepairsSettled,
                    ],
                    installer_retry_policy: RetryPolicy::NotImplemented,
                    upstream_attempts: UpstreamAttempts::Unknown,
                    other_applied_changes: ChangeObservation::Unknown,
                    cancelled_ms: Some(u64::MAX),
                }),
            },
        };
        let bytes = report.encode().unwrap();
        let value: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(
            value["state"]["outcome"]["observations"]["events"]
                .as_array()
                .unwrap()
                .len(),
            64
        );
    }
}
