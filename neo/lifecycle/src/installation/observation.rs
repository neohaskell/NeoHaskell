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
            let row = if matches!(
                reason,
                super::nix_owner::NixBlock::ConfigurationOwnerUnknown
            ) {
                1
            } else {
                0
            };
            self.rows[row] = Row::Blocked;
        }
        self.activity(if result.is_err() {
            Activity::Failed
        } else {
            Activity::Finished
        });
    }
}
