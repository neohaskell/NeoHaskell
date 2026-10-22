from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Mapping


class PlanKind(str, Enum):
    RELEASE = "release"
    RECOVERY = "recovery"
    OTHER = "other"
    UNKNOWN = "unknown"


class Disposition(str, Enum):
    OPEN = "open"
    CLOSED = "closed"
    MERGED = "merged"
    DRAFT = "draft"
    UNKNOWN = "unknown"


class ProofCheck(str, Enum):
    CANONICAL_REPOSITORY = "canonical_repository"
    MAIN_BASE = "main_base"
    SAME_REPOSITORY_HEAD = "same_repository_head"
    ACTIONS_AUTHOR = "actions_author"
    DETERMINISTIC_BRANCH = "deterministic_branch"
    SOURCE_MARKER = "source_marker"
    COMMIT_PARENT = "commit_parent"
    SINGLE_RELEASE_PLAN = "single_release_plan"
    PLAN_IDENTITY = "plan_identity"
    NON_BOOTSTRAP = "non_bootstrap"
    GENERATED_DIFF = "generated_diff"


REQUIRED_OWNERSHIP_PROOF = frozenset(ProofCheck)


class Ownership(str, Enum):
    VERIFIED = "verified"
    REJECTED = "rejected"
    UNKNOWN = "unknown"
    RECOVERY = "recovery"
    OTHER = "other"


class RunOperation(str, Enum):
    AUTO = "auto"
    RECOVER = "recover"
    RESUME = "resume"
    UNKNOWN = "unknown"


class RunEvent(str, Enum):
    WORKFLOW_DISPATCH = "workflow_dispatch"
    PULL_REQUEST = "pull_request"
    PUSH = "push"
    UNKNOWN = "unknown"


class RunStatus(str, Enum):
    QUEUED = "queued"
    IN_PROGRESS = "in_progress"
    WAITING = "waiting"
    REQUESTED = "requested"
    PENDING = "pending"
    COMPLETED = "completed"
    UNKNOWN = "unknown"


class RunConclusion(str, Enum):
    ACTION_REQUIRED = "action_required"
    SUCCESS = "success"
    FAILURE = "failure"
    CANCELLED = "cancelled"
    UNKNOWN = "unknown"


ACTIVE_RUN_STATUSES = frozenset(
    {
        RunStatus.QUEUED,
        RunStatus.IN_PROGRESS,
        RunStatus.WAITING,
        RunStatus.REQUESTED,
        RunStatus.PENDING,
    }
)


class PublicationStatus(str, Enum):
    IN_PROGRESS = "in_progress"
    PENDING = "pending"
    TRANSIENT_FAILURE = "transient_failure"
    PAUSED = "paused"
    CORRUPT = "corrupt"
    NEEDS_SOURCE_FIX = "needs_source_fix"
    COMPLETE = "complete"
    UNKNOWN = "unknown"


class CheckStatus(str, Enum):
    SUCCESS = "success"
    PENDING = "pending"
    ACTION_REQUIRED = "action_required"
    FAILURE = "failure"
    CANCELLED = "cancelled"
    SKIPPED = "skipped"
    NEUTRAL = "neutral"
    UNKNOWN = "unknown"


class VerdictStatus(str, Enum):
    MISSING = "missing"
    PASS = "pass"
    PASS_NOTES = "pass_notes"
    FAIL = "fail"


class ReviewDecision(str, Enum):
    APPROVED = "approved"
    CHANGES_REQUESTED = "changes_requested"
    REVIEW_REQUIRED = "review_required"
    NONE = "none"
    UNKNOWN = "unknown"


class DecisionKind(str, Enum):
    BLOCKED = "blocked"
    RESCAN = "rescan"
    WAIT_FOR_RUN = "wait_for_run"
    CARRY_REVIEW = "carry_review"
    APPROVE_ACTIONS = "approve_actions"
    WAIT_CHECKS = "wait_checks"
    WAIT_REVIEW = "wait_review"
    NEED_VERDICT = "need_verdict"
    WAIT_MERGEABILITY = "wait_mergeability"
    MERGE = "merge"
    DISPATCH_AUTO = "dispatch_auto"
    TRACK_PUBLICATION = "track_publication"
    RETRY_FROZEN = "retry_frozen"
    COMPLETE = "complete"
    LINK_STALE = "link_stale"
    CLOSE_STALE = "close_stale"


@dataclass(frozen=True, order=True)
class PRKey:
    host: str
    repository: str
    number: int


@dataclass(frozen=True)
class OwnershipProof:
    passed: frozenset[ProofCheck] = frozenset()
    failed: frozenset[ProofCheck] = frozenset()
    verified_head_sha: str | None = None
    verified_source_sha: str | None = None
    verified_plan_id: str | None = None


@dataclass(frozen=True)
class IndependentVerdict:
    status: VerdictStatus = VerdictStatus.MISSING
    tested_head_sha: str | None = None
    tested_main_sha: str | None = None
    verifier_id: int | None = None


@dataclass(frozen=True)
class CheckEvidence:
    context: str
    app_id: int | None
    head_sha: str
    status: CheckStatus
    run_id: int
    attempt: int
    authoritative: bool
    workflow_path: str


@dataclass(frozen=True)
class RequiredCheck:
    context: str
    app_id: int


@dataclass(frozen=True)
class ReviewFinding:
    source_pr: PRKey
    thread_id: str
    url: str
    summary: str


@dataclass(frozen=True)
class ReviewEvidence:
    complete: bool = False
    head_sha: str | None = None
    base_sha: str | None = None
    decision: ReviewDecision = ReviewDecision.UNKNOWN
    approvals: int = 0
    unresolved_threads: tuple[str, ...] = ()


@dataclass(frozen=True)
class PRRecord:
    key: PRKey
    url: str
    head_sha: str
    base_ref: str
    base_sha: str
    source_sha: str | None
    plan_id: str | None
    plan_kind: PlanKind
    ownership_proof: OwnershipProof
    disposition: Disposition
    preparation_like: bool
    author_id: int | None = None
    checks: tuple[CheckEvidence, ...] = ()
    verdict: IndependentVerdict = field(default_factory=IndependentVerdict)
    review: ReviewEvidence = field(default_factory=ReviewEvidence)
    carryover_findings: tuple[ReviewFinding, ...] = ()
    carryover_reconciled: bool = False
    mergeable: bool | None = None
    mergeable_head_sha: str | None = None
    merge_commit_sha: str | None = None
    merge_first_parent: str | None = None
    replacement_link_recorded: bool = False
    stale_source_is_ancestor_of_replacement: bool | None = None

    @property
    def ownership(self) -> Ownership:
        if self.plan_kind is PlanKind.RECOVERY:
            return Ownership.RECOVERY
        if self.plan_kind is PlanKind.OTHER:
            return Ownership.OTHER
        if self.ownership_proof.failed:
            return Ownership.REJECTED
        if not self.preparation_like:
            return Ownership.OTHER
        proof_is_complete = REQUIRED_OWNERSHIP_PROOF <= self.ownership_proof.passed
        proof_matches_record = (
            self.ownership_proof.verified_head_sha == self.head_sha
            and self.ownership_proof.verified_source_sha == self.source_sha
            and self.ownership_proof.verified_plan_id == self.plan_id
            and self.source_sha is not None
            and self.plan_id is not None
            and self.plan_kind is PlanKind.RELEASE
        )
        if proof_is_complete and proof_matches_record:
            return Ownership.VERIFIED
        return Ownership.UNKNOWN


@dataclass(frozen=True)
class ActiveRun:
    run_id: int
    event: RunEvent
    status: RunStatus
    head_sha: str | None
    operation: RunOperation = RunOperation.UNKNOWN
    pull_request: PRKey | None = None
    attempt: int = 1
    workflow_path: str | None = None
    actor_id: int | None = None
    conclusion: RunConclusion = RunConclusion.UNKNOWN
    authoritative: bool = False


@dataclass(frozen=True)
class PendingAttempt:
    plan_id: str
    source_sha: str
    publication: PublicationStatus
    assets_verified: bool = False
    published_evidence_verified: bool = False


@dataclass(frozen=True)
class BranchRules:
    complete: bool
    observed_main_sha: str | None
    required_checks: tuple[RequiredCheck, ...] = ()
    required_approvals: int | None = None
    squash_allowed: bool = False
    queue_required: bool = False
    required_workflow_paths: frozenset[str] = frozenset()
    workflow_mapping_complete: bool = False


@dataclass(frozen=True)
class SweepSnapshot:
    main_sha: str
    selection_main_sha: str | None
    inventory_complete: bool
    rules: BranchRules | None
    records: Mapping[PRKey, PRRecord] = field(default_factory=dict)
    blockers: tuple[str, ...] = ()
    active_runs: tuple[ActiveRun, ...] = ()
    pending_attempt: PendingAttempt | None = None
    verified_merge: PRRecord | None = None

    def __post_init__(self) -> None:
        if any(key != record.key for key, record in self.records.items()):
            raise ValueError("PR inventory key does not match its record")


@dataclass(frozen=True)
class Decision:
    kind: DecisionKind
    reason: str
    pr: PRKey | None = None
    plan_id: str | None = None
    expected_main_sha: str | None = None
    expected_head_sha: str | None = None
    details: tuple[tuple[str, str], ...] = ()


def _decision(
    snapshot: SweepSnapshot,
    kind: DecisionKind,
    reason: str,
    record: PRRecord | None = None,
    details: tuple[tuple[str, str], ...] = (),
    plan_id: str | None = None,
) -> Decision:
    return Decision(
        kind=kind,
        reason=reason,
        pr=record.key if record else None,
        plan_id=record.plan_id if record else plan_id,
        expected_main_sha=snapshot.main_sha,
        expected_head_sha=record.head_sha if record else None,
        details=details,
    )


def _active_runs(snapshot: SweepSnapshot) -> tuple[ActiveRun, ...]:
    return tuple(
        run for run in snapshot.active_runs if run.status in ACTIVE_RUN_STATUSES
    )


def _current_candidate(record: PRRecord, snapshot: SweepSnapshot) -> bool:
    return (
        record.ownership is Ownership.VERIFIED
        and record.disposition in {Disposition.OPEN, Disposition.DRAFT, Disposition.UNKNOWN}
        and record.plan_kind is PlanKind.RELEASE
        and record.source_sha == snapshot.main_sha
    )


def _current_verdict(record: PRRecord, main_sha: str) -> bool:
    verdict = record.verdict
    return (
        verdict.status in {VerdictStatus.PASS, VerdictStatus.PASS_NOTES}
        and verdict.tested_head_sha == record.head_sha
        and verdict.tested_main_sha == main_sha
        and verdict.verifier_id is not None
        and verdict.verifier_id != record.author_id
    )


def _action_required_runs(
    snapshot: SweepSnapshot,
    record: PRRecord,
    rules: BranchRules,
 ) -> tuple[ActiveRun, ...]:
    matched = tuple(
        run
        for run in snapshot.active_runs
        if run.authoritative
        and run.conclusion is RunConclusion.ACTION_REQUIRED
        and run.event is RunEvent.PULL_REQUEST
        and run.pull_request == record.key
        and run.head_sha == record.head_sha
        and run.actor_id == record.author_id
        and run.workflow_path in rules.required_workflow_paths
    )
    latest_by_path: dict[str, list[ActiveRun]] = {}
    for run in matched:
        latest_by_path.setdefault(run.workflow_path or "", []).append(run)
    if any(len(runs) > 1 for runs in latest_by_path.values()):
        return ()
    return matched


def _check_decision(
    snapshot: SweepSnapshot, record: PRRecord, rules: BranchRules
) -> Decision | None:
    evidence_by_context = {}
    for evidence in record.checks:
        if evidence.head_sha == record.head_sha:
            evidence_by_context.setdefault(evidence.context, []).append(evidence)

    pending, failed, wrong_app = [], [], []
    for required in rules.required_checks:
        evidence = evidence_by_context.get(required.context, [])
        matching_app = [item for item in evidence if item.app_id == required.app_id]
        if not matching_app:
            if evidence:
                wrong_app.append(required.context)
            else:
                pending.append(required.context)
            continue
        authoritative = [item for item in matching_app if item.authoritative]
        if not authoritative:
            pending.append(required.context)
            continue
        if len(authoritative) != 1:
            failed.append(required.context + " (ambiguous run evidence)")
            continue
        status = authoritative[0].status
        if status is CheckStatus.SUCCESS:
            continue
        if status in {CheckStatus.ACTION_REQUIRED, CheckStatus.PENDING}:
            pending.append(required.context)
        else:
            failed.append(required.context + " (" + status.value + ")")

    if wrong_app:
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "A required check ran under a different GitHub App",
            record,
            (("contexts", ", ".join(sorted(wrong_app))),),
        )
    if failed:
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "A required check failed, was skipped, or has ambiguous evidence",
            record,
            (("contexts", ", ".join(sorted(failed))),),
        )
    if pending:
        return _decision(
            snapshot,
            DecisionKind.WAIT_CHECKS,
            "Required checks have not all succeeded on the current head",
            record,
            (("contexts", ", ".join(sorted(pending))),),
        )
    return None


def decide(snapshot: SweepSnapshot) -> Decision:
    if not snapshot.inventory_complete:
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "The paginated live PR inventory is incomplete",
        )
    if snapshot.blockers:
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "; ".join(snapshot.blockers),
        )
    main_changed = (
        snapshot.selection_main_sha is not None
        and snapshot.selection_main_sha != snapshot.main_sha
    )
    if main_changed and snapshot.pending_attempt is None:
        return _decision(
            snapshot,
            DecisionKind.RESCAN,
            "Main changed during selection; refresh the inventory and all affected evidence",
        )

    if snapshot.verified_merge is not None:
        merged = snapshot.verified_merge
        if (
            merged.ownership is not Ownership.VERIFIED
            or merged.disposition is not Disposition.MERGED
            or merged.merge_commit_sha is None
            or merged.merge_first_parent is None
            or merged.source_sha is None
        ):
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "Merged preparation readback is incomplete",
                merged,
            )
        if merged.merge_first_parent != merged.source_sha:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "Squash merge first parent differs from the frozen verified source",
                merged,
            )
        if snapshot.pending_attempt is None or snapshot.pending_attempt.plan_id != merged.plan_id:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "The merged release plan is missing from the live pending-attempt ledger",
                merged,
            )
        stale = sorted(
            (
                record
                for record in snapshot.records.values()
                if record.key != merged.key
                and record.ownership is Ownership.VERIFIED
                and record.disposition is Disposition.OPEN
                and record.source_sha != merged.source_sha
                and record.stale_source_is_ancestor_of_replacement is True
            ),
            key=lambda record: record.key,
        )
        if stale:
            if main_changed:
                return _decision(
                    snapshot,
                    DecisionKind.RESCAN,
                    "Main changed before stale PR closure; refresh the inventory",
                )
            record = stale[0]
            if not record.replacement_link_recorded:
                return _decision(
                    snapshot,
                    DecisionKind.LINK_STALE,
                    "Link this proven stale preparation to the verified replacement before closing it",
                    record,
                    (("replacement", merged.url),),
                )
            return _decision(
                snapshot,
                DecisionKind.CLOSE_STALE,
                "Close one independently proven stale preparation after replacement merge readback",
                record,
                (("replacement", merged.url),),
            )

    if any(run.status is RunStatus.UNKNOWN for run in snapshot.active_runs):
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "A relevant workflow run has unknown status; reconcile it before acting",
        )

    if snapshot.pending_attempt is not None:
        attempt = snapshot.pending_attempt
        if attempt.publication in {
            PublicationStatus.PAUSED,
            PublicationStatus.CORRUPT,
            PublicationStatus.NEEDS_SOURCE_FIX,
            PublicationStatus.UNKNOWN,
        }:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "The frozen release attempt needs recovery or investigation",
                details=(("attempt", attempt.plan_id), ("state", attempt.publication.value)),
                plan_id=attempt.plan_id,
            )
        active = _active_runs(snapshot)
        if active:
            return _decision(
                snapshot,
                DecisionKind.WAIT_FOR_RUN,
                "A workflow run is active for the frozen attempt",
                details=(("runs", ", ".join(str(run.run_id) for run in active)),),
                plan_id=attempt.plan_id,
            )
        if attempt.publication is PublicationStatus.COMPLETE:
            if not attempt.published_evidence_verified:
                return _decision(
                    snapshot,
                    DecisionKind.TRACK_PUBLICATION,
                    "Publication is marked complete but public notes, tags, assets, and checksums are not verified",
                    details=(("attempt", attempt.plan_id),),
                    plan_id=attempt.plan_id,
                )
            return _decision(
                snapshot,
                DecisionKind.COMPLETE,
                "The immutable release attempt and its public assets are verified",
                details=(("attempt", attempt.plan_id),),
                plan_id=attempt.plan_id,
            )
        if attempt.publication is PublicationStatus.TRANSIENT_FAILURE:
            if main_changed:
                return _decision(
                    snapshot,
                    DecisionKind.RESCAN,
                    "Main changed before retry; refresh the frozen attempt and assets",
                    plan_id=attempt.plan_id,
                )
            if not attempt.assets_verified:
                return _decision(
                    snapshot,
                    DecisionKind.BLOCKED,
                    "Transient retry lacks verification of the same immutable attempt and assets",
                    details=(("attempt", attempt.plan_id),),
                    plan_id=attempt.plan_id,
                )
            return _decision(
                snapshot,
                DecisionKind.RETRY_FROZEN,
                "Retry auto on main for this same verified immutable attempt",
                details=(("attempt", attempt.plan_id),),
                plan_id=attempt.plan_id,
            )
        return _decision(
            snapshot,
            DecisionKind.TRACK_PUBLICATION,
            "Track the frozen attempt through native builds, consumer verification, and publication",
            details=(("attempt", attempt.plan_id),),
            plan_id=attempt.plan_id,
        )

    rules = snapshot.rules
    if rules is None or not rules.complete or not rules.workflow_mapping_complete:
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "Live effective branch rules or required-workflow mapping are unavailable or incomplete",
        )
    if rules.observed_main_sha != snapshot.main_sha:
        return _decision(
            snapshot,
            DecisionKind.RESCAN,
            "Branch rules were not read with the current main snapshot",
        )

    suspicious = sorted(
        (
            record
            for record in snapshot.records.values()
            if record.preparation_like
            and record.disposition is Disposition.OPEN
            and record.ownership in {Ownership.REJECTED, Ownership.UNKNOWN}
        ),
        key=lambda record: record.key,
    )
    current = sorted(
        (
            record
            for record in snapshot.records.values()
            if _current_candidate(record, snapshot)
        ),
        key=lambda record: record.key,
    )
    if len(current) > 1:
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "Multiple independently verified preparations target current main",
            details=(("prs", ", ".join(str(record.key.number) for record in current)),),
        )

    active = _active_runs(snapshot)
    if active:
        return _decision(
            snapshot,
            DecisionKind.WAIT_FOR_RUN,
            "A relevant workflow run is active; wait and rescan before dispatching or selecting a PR",
            details=(("runs", ", ".join(str(run.run_id) for run in active)),),
        )
    if current:
        record = current[0]
        if record.disposition is not Disposition.OPEN:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "The verified current preparation is not in an open, mergeable PR state",
                record,
            )
        if record.base_ref != "main" or record.base_sha != snapshot.main_sha:
            return _decision(
                snapshot,
                DecisionKind.RESCAN,
                "The candidate base does not match the current main snapshot",
                record,
            )
        if record.carryover_findings and not record.carryover_reconciled:
            return _decision(
                snapshot,
                DecisionKind.CARRY_REVIEW,
                "Carry forward actionable findings from older preparation PRs and recheck them against this diff",
                record,
                (("findings", str(len(record.carryover_findings))),),
            )
        if not record.review.complete:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "Current review state and threads are incomplete",
                record,
            )
        if record.review.head_sha != record.head_sha or record.review.base_sha != snapshot.main_sha:
            return _decision(
                snapshot,
                DecisionKind.RESCAN,
                "Review evidence no longer binds the current head and main",
                record,
            )
        if record.review.unresolved_threads:
            return _decision(
                snapshot,
                DecisionKind.WAIT_REVIEW,
                "Current review threads remain unresolved",
                record,
                (("threads", ", ".join(record.review.unresolved_threads)),),
            )
        if record.review.decision is ReviewDecision.CHANGES_REQUESTED:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "A current review requests changes",
                record,
            )
        if record.review.decision is ReviewDecision.UNKNOWN:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "The current review decision could not be established",
                record,
            )
        if rules.required_approvals is None:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "The live required-approval count is unknown",
                record,
            )
        if record.review.approvals < rules.required_approvals:
            return _decision(
                snapshot,
                DecisionKind.WAIT_REVIEW,
                "The current branch rules require more approvals",
                record,
            )

        action_runs = _action_required_runs(snapshot, record, rules)
        if action_runs:
            return _decision(
                snapshot,
                DecisionKind.APPROVE_ACTIONS,
                "Matched pull_request workflow runs need approval for this verified current candidate",
                record,
                (
                    ("runs", ", ".join(str(run.run_id) for run in action_runs)),
                    (
                        "attempts",
                        ", ".join(
                            f"{run.run_id}:{run.attempt}" for run in action_runs
                        ),
                    ),
                    (
                        "workflows",
                        ", ".join(sorted({run.workflow_path or "" for run in action_runs})),
                    ),
                ),
            )

        check_decision = _check_decision(snapshot, record, rules)
        if check_decision is not None:
            return check_decision

        if record.verdict.status is VerdictStatus.FAIL:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "The independent verifier returned FAIL",
                record,
            )
        if not _current_verdict(record, snapshot.main_sha):
            return _decision(
                snapshot,
                DecisionKind.NEED_VERDICT,
                "An independent PASS or PASS+NOTES must bind this head and current main",
                record,
            )
        if record.mergeable_head_sha != record.head_sha or record.mergeable is None:
            return _decision(
                snapshot,
                DecisionKind.WAIT_MERGEABILITY,
                "GitHub has not confirmed mergeability for this exact head",
                record,
            )
        if record.mergeable is False:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "GitHub reports that this exact head is not mergeable",
                record,
            )
        if not rules.squash_allowed:
            return _decision(
                snapshot,
                DecisionKind.BLOCKED,
                "Current branch rules do not allow the required squash merge",
                record,
            )
        return _decision(
            snapshot,
            DecisionKind.MERGE,
            "All live gates bind the verified current head and main",
            record,
            (("queue_required", str(rules.queue_required).lower()),),
        )

    same_source_unverified = [
        record for record in suspicious if record.source_sha == snapshot.main_sha
    ]
    if same_source_unverified:
        record = same_source_unverified[0]
        return _decision(
            snapshot,
            DecisionKind.BLOCKED,
            "A preparation-like PR for current main lacks ownership proof; leave it open and do not dispatch a duplicate",
            record,
        )

    return _decision(
        snapshot,
        DecisionKind.DISPATCH_AUTO,
        "No verified current preparation, pending attempt, or relevant workflow run exists",
    )
