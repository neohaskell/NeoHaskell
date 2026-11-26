use super::{ReviewSummary, RunOutcome, TransactionError};
use std::collections::VecDeque;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct DisplayVersion {
    major: u64,
    minor: u64,
    patch: u64,
}

impl DisplayVersion {
    pub(super) fn parse(value: &str) -> Result<Self, TransactionError> {
        let parsed = semver::Version::parse(value).map_err(|_| TransactionError::InvalidPair)?;
        if !parsed.pre.is_empty() || !parsed.build.is_empty() || parsed.to_string() != value {
            return Err(TransactionError::InvalidPair);
        }
        Ok(Self {
            major: parsed.major,
            minor: parsed.minor,
            patch: parsed.patch,
        })
    }
}

impl std::fmt::Display for DisplayVersion {
    fn fmt(&self, output: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(output, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Activity {
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
    Declined,
    Finished,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Row {
    NotObserved,
    Existing,
    Unchanged,
    Working,
    Verified,
    Blocked,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Detail {
    Activity(Activity),
    Truncated,
}

pub(super) struct PresentationObservation {
    pub(super) activity: Activity,
    pub(super) rows: [Row; 6],
    details: VecDeque<Detail>,
}

impl PresentationObservation {
    pub(super) fn new() -> Self {
        Self {
            activity: Activity::Inspecting,
            rows: [Row::NotObserved; 6],
            details: VecDeque::new(),
        }
    }
    pub(super) fn activity(&mut self, activity: Activity) {
        if self.activity == activity {
            return;
        }
        self.activity = activity;
        if self.details.len() >= 127 {
            self.details.pop_front();
            if !self.details.contains(&Detail::Truncated) {
                self.details.pop_front();
                self.details.push_back(Detail::Truncated);
            }
        }
        self.details.push_back(Detail::Activity(activity));
    }
    pub(super) fn details(&self) -> impl DoubleEndedIterator<Item = &Detail> {
        self.details.iter()
    }
    pub(super) fn review(&mut self, summary: &ReviewSummary) -> Result<(), TransactionError> {
        summary
            .version
            .as_deref()
            .map(DisplayVersion::parse)
            .transpose()?;
        self.activity(Activity::Review);
        Ok(())
    }
    pub(super) fn finish(&mut self, result: &Result<RunOutcome, TransactionError>) {
        if let Err(TransactionError::NixBlocked(reason)) = result {
            let row = if *reason == super::nix::NixBlock::PairAcquisition {
                3
            } else {
                0
            };
            self.rows[row] = Row::Blocked;
        } else if matches!(result, Err(TransactionError::ReleaseAcquisition { .. })) {
            self.rows[3] = Row::Blocked;
        }
        self.activity(if result.is_err() {
            Activity::Failed
        } else {
            Activity::Finished
        });
    }
}

pub(super) struct InvocationObservation {
    started: std::time::Instant,
    facts: crate::diagnostics::InvocationFacts,
}

impl InvocationObservation {
    pub(super) fn new() -> Self {
        use crate::diagnostics::*;
        Self {
            started: std::time::Instant::now(),
            facts: InvocationFacts {
                elapsed_ms: 0,
                phase: Phase::Inspecting,
                events: vec![PhaseEvent {
                    phase: Phase::Inspecting,
                    elapsed_ms: 0,
                }],
                events_truncated: false,
                review: None,
                completed_steps: Vec::new(),
                applied_changes: Vec::new(),
                installer_retry_policy: RetryPolicy::NotImplemented,
                upstream_attempts: UpstreamAttempts::Unknown,
                other_applied_changes: ChangeObservation::Unknown,
                cancelled_ms: None,
            },
        }
    }
    fn elapsed(&self) -> u64 {
        self.started.elapsed().as_millis().min(u64::MAX as u128) as u64
    }
    pub(super) fn snapshot(&self) -> crate::diagnostics::InvocationFacts {
        let mut facts = self.facts.clone();
        facts.elapsed_ms = self.elapsed();
        facts
    }
    pub(super) fn activity(&mut self, activity: Activity) {
        use crate::diagnostics::{Phase, PhaseEvent};
        let phase = match activity {
            Activity::Inspecting => Phase::Inspecting,
            Activity::CheckingNix => Phase::CheckingNix,
            Activity::Selecting => Phase::Selecting,
            Activity::PreparingReview => Phase::PreparingReview,
            Activity::Review => Phase::Review,
            Activity::Rechecking => Phase::Rechecking,
            Activity::Recovering => Phase::Recovering,
            Activity::PreparingPair => Phase::PreparingPair,
            Activity::VerifyingPair => Phase::VerifyingPair,
            Activity::Activating => Phase::Activating,
            Activity::Finishing => Phase::Finishing,
            Activity::Stopping => Phase::Stopping,
            Activity::Declined => Phase::Unadmitted,
            Activity::Finished => Phase::Finished,
            Activity::Failed => Phase::Failed,
        };
        if self.facts.phase == phase {
            return;
        }
        self.facts.phase = phase;
        if self.facts.events.len() < 64 {
            self.facts.events.push(PhaseEvent {
                phase,
                elapsed_ms: self.elapsed(),
            });
        } else {
            self.facts.events_truncated = true;
        }
    }
    pub(super) fn review(&mut self) {
        self.activity(Activity::Review);
        self.facts.review = Some(crate::diagnostics::ReviewFact {
            entered_ms: self.elapsed(),
            wait_ms: None,
            answer: None,
            admitted_ms: None,
        });
    }
    pub(super) fn answer(&mut self, answer: crate::diagnostics::ReviewAnswer) {
        let elapsed = self.elapsed();
        if let Some(review) = &mut self.facts.review {
            if review.answer.is_none() {
                review.wait_ms = Some(elapsed.saturating_sub(review.entered_ms));
                review.answer = Some(answer);
            }
        }
    }
    pub(super) fn cancelled(&mut self) {
        if self.facts.cancelled_ms.is_none() {
            self.facts.cancelled_ms = Some(self.elapsed());
        }
        self.answer(crate::diagnostics::ReviewAnswer::Cancelled);
    }
    pub(super) fn admitted(&mut self) {
        let elapsed = self.elapsed();
        if let Some(review) = &mut self.facts.review {
            review.admitted_ms = Some(elapsed);
        }
    }
    pub(super) fn completed(&mut self, step: crate::diagnostics::CompletedStep) {
        if !self.facts.completed_steps.contains(&step) {
            self.facts.completed_steps.push(step);
        }
    }
    pub(super) fn applied(&mut self, change: crate::diagnostics::AppliedChange) {
        if !self.facts.applied_changes.contains(&change) {
            self.facts.applied_changes.push(change);
        }
    }
}
