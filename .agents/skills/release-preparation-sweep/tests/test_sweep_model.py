from dataclasses import replace
import os
from pathlib import Path
import subprocess
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))

from sweep_model import (
    ActiveRun,
    BranchRules,
    CheckEvidence,
    CheckStatus,
    DecisionKind,
    Disposition,
    IndependentVerdict,
    OwnershipProof,
    PendingAttempt,
    PlanKind,
    PRKey,
    PRRecord,
    ProofCheck,
    PublicationStatus,
    REQUIRED_OWNERSHIP_PROOF,
    RequiredCheck,
    ReviewDecision,
    ReviewEvidence,
    RunOperation,
    RunEvent,
    RunConclusion,
    RunStatus,
    SweepSnapshot,
    VerdictStatus,
    decide,
)


MAIN = "a" * 40
OLD_MAIN = "b" * 40
HEAD = "c" * 40
CHANGED_HEAD = "d" * 40
MERGE_SHA = "e" * 40
PLAN = "f" * 64
WORKFLOW_PATHS = frozenset(
    {
        ".github/workflows/test.yml",
        ".github/workflows/checks.yml",
        ".github/workflows/neo-ci.yml",
    }
)


def make_rules(main_sha=MAIN):
    return BranchRules(
        complete=True,
        observed_main_sha=main_sha,
        required_checks=(RequiredCheck("ci-gate", 15368),),
        required_approvals=0,
        squash_allowed=True,
        queue_required=False,
        required_workflow_paths=WORKFLOW_PATHS,
        workflow_mapping_complete=True,
    )


def make_proof(head_sha=HEAD, source_sha=MAIN, plan_id=PLAN, failed=frozenset()):
    return OwnershipProof(
        passed=REQUIRED_OWNERSHIP_PROOF,
        failed=failed,
        verified_head_sha=head_sha,
        verified_source_sha=source_sha,
        verified_plan_id=plan_id,
    )


def make_check(
    context,
    app_id,
    head_sha,
    status,
    run_id=500,
    attempt=1,
    authoritative=True,
    workflow_path=".github/workflows/ci.yml",
):
    return CheckEvidence(
        context,
        app_id,
        head_sha,
        status,
        run_id,
        attempt,
        authoritative,
        workflow_path,
    )


def make_pr(
    number=931,
    head_sha=HEAD,
    source_sha=MAIN,
    plan_id=PLAN,
    plan_kind=PlanKind.RELEASE,
    disposition=Disposition.OPEN,
    proof=None,
    **changes,
):
    key = PRKey("github.com", "neohaskell/NeoHaskell", number)
    values = dict(
        key=key,
        url=f"https://github.com/neohaskell/NeoHaskell/pull/{number}",
        head_sha=head_sha,
        base_ref="main",
        base_sha=MAIN,
        source_sha=source_sha,
        plan_id=plan_id,
        plan_kind=plan_kind,
        ownership_proof=proof or make_proof(head_sha, source_sha, plan_id),
        disposition=disposition,
        preparation_like=True,
        author_id=10,
        checks=(make_check("ci-gate", 15368, head_sha, CheckStatus.SUCCESS),),
        verdict=IndependentVerdict(
            VerdictStatus.PASS,
            tested_head_sha=head_sha,
            tested_main_sha=MAIN,
            verifier_id=20,
        ),
        review=ReviewEvidence(
            complete=True,
            head_sha=head_sha,
            base_sha=MAIN,
            decision=ReviewDecision.NONE,
            approvals=0,
        ),
        mergeable=True,
        mergeable_head_sha=head_sha,
    )
    values.update(changes)
    return PRRecord(**values)


def make_snapshot(
    records=(),
    active_runs=(),
    pending_attempt=None,
    verified_merge=None,
    main_sha=MAIN,
    selection_main_sha=MAIN,
    rules=None,
    inventory_complete=True,
):
    return SweepSnapshot(
        main_sha=main_sha,
        selection_main_sha=selection_main_sha,
        inventory_complete=inventory_complete,
        rules=rules or make_rules(main_sha),
        records={record.key: record for record in records},
        active_runs=tuple(active_runs),
        pending_attempt=pending_attempt,
        verified_merge=verified_merge,
    )


def make_merged(source_sha=OLD_MAIN, first_parent=None):
    return make_pr(
        number=929,
        source_sha=source_sha,
        disposition=Disposition.MERGED,
        merge_commit_sha=MERGE_SHA,
        merge_first_parent=first_parent or source_sha,
        base_sha=source_sha,
    )


class SweepDecisionTests(unittest.TestCase):
    def test_empty_sweep_dispatches_once_then_waits_for_the_visible_run(self):
        first = decide(make_snapshot())
        self.assertEqual(first.kind, DecisionKind.DISPATCH_AUTO)
        self.assertEqual(first.expected_main_sha, MAIN)

        rerun = decide(
            make_snapshot(
                active_runs=(
                    ActiveRun(
                        101,
                        RunEvent.WORKFLOW_DISPATCH,
                        RunStatus.IN_PROGRESS,
                        MAIN,
                        operation=RunOperation.AUTO,
                    ),
                )
            )
        )
        self.assertEqual(rerun.kind, DecisionKind.WAIT_FOR_RUN)

        with_candidate = decide(make_snapshot(records=(make_pr(),)))
        self.assertEqual(with_candidate.kind, DecisionKind.MERGE)

    def test_concurrent_auto_runs_wait_without_dispatch(self):
        decision = decide(
            make_snapshot(
                active_runs=(
                    ActiveRun(
                        101,
                        RunEvent.WORKFLOW_DISPATCH,
                        RunStatus.QUEUED,
                        MAIN,
                        operation=RunOperation.AUTO,
                    ),
                    ActiveRun(
                        102,
                        RunEvent.WORKFLOW_DISPATCH,
                        RunStatus.IN_PROGRESS,
                        MAIN,
                        operation=RunOperation.AUTO,
                    ),
                )
            )
        )
        self.assertEqual(decision.kind, DecisionKind.WAIT_FOR_RUN)
        self.assertIsNone(decision.pr)

    def test_main_change_after_selection_requires_a_fresh_scan(self):
        stale_candidate = make_pr(source_sha=OLD_MAIN)
        decision = decide(
            make_snapshot(
                records=(stale_candidate,),
                selection_main_sha=OLD_MAIN,
                main_sha=MAIN,
            )
        )
        self.assertEqual(decision.kind, DecisionKind.RESCAN)
        self.assertEqual(decision.expected_main_sha, MAIN)

    def test_changed_pr_head_is_left_open_without_a_merge_decision(self):
        changed = make_pr(
            head_sha=CHANGED_HEAD,
            proof=make_proof(head_sha=HEAD),
        )
        decision = decide(make_snapshot(records=(changed,)))
        self.assertEqual(decision.kind, DecisionKind.BLOCKED)
        self.assertEqual(decision.pr, changed.key)
        self.assertNotEqual(decision.kind, DecisionKind.CLOSE_STALE)

    def test_two_verified_current_candidates_stop(self):
        decision = decide(make_snapshot(records=(make_pr(), make_pr(number=932))))
        self.assertEqual(decision.kind, DecisionKind.BLOCKED)
        self.assertIn("Multiple", decision.reason)

    def test_verified_draft_or_unknown_state_never_dispatches_a_duplicate(self):
        for disposition in (Disposition.DRAFT, Disposition.UNKNOWN):
            candidate = make_pr(disposition=disposition)
            decision = decide(make_snapshot(records=(candidate,)))
            self.assertEqual(decision.kind, DecisionKind.BLOCKED)
            self.assertEqual(decision.pr, candidate.key)

    def test_unverified_same_source_pr_is_excluded_when_one_candidate_is_proven(self):
        ambiguous = make_pr(
            number=932,
            proof=make_proof(failed=frozenset({ProofCheck.GENERATED_DIFF})),
        )
        decision = decide(make_snapshot(records=(make_pr(), ambiguous)))
        self.assertEqual(decision.kind, DecisionKind.MERGE)
        self.assertEqual(decision.pr, make_pr().key)

    def test_unverified_current_source_pr_prevents_duplicate_dispatch(self):
        altered = make_pr(
            proof=make_proof(failed=frozenset()),
            head_sha=CHANGED_HEAD,
        )
        decision = decide(make_snapshot(records=(altered,)))
        self.assertEqual(decision.kind, DecisionKind.BLOCKED)
        self.assertEqual(decision.pr, altered.key)

    def test_generated_diff_failure_never_qualifies_for_merge_or_stale_closure(self):
        altered = make_pr(
            source_sha=OLD_MAIN,
            proof=make_proof(
                head_sha=HEAD,
                source_sha=OLD_MAIN,
                failed=frozenset({ProofCheck.GENERATED_DIFF}),
            ),
        )
        merged = make_merged()
        decision = decide(
            make_snapshot(
                records=(altered,),
                verified_merge=merged,
                pending_attempt=PendingAttempt(
                    PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS
                ),
            )
        )
        self.assertEqual(decision.kind, DecisionKind.TRACK_PUBLICATION)
        self.assertNotEqual(decision.pr, altered.key)

    def test_recovery_pr_is_outside_the_ordinary_sweep(self):
        recovery = make_pr(plan_kind=PlanKind.RECOVERY)
        decision = decide(make_snapshot(records=(recovery,)))
        self.assertEqual(decision.kind, DecisionKind.DISPATCH_AUTO)

    def test_pending_publication_prevents_a_new_preparation(self):
        decision = decide(
            make_snapshot(
                pending_attempt=PendingAttempt(
                    PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS
                )
            )
        )
        self.assertEqual(decision.kind, DecisionKind.TRACK_PUBLICATION)
        self.assertNotEqual(decision.kind, DecisionKind.DISPATCH_AUTO)

    def test_later_main_commit_does_not_interrupt_frozen_publication(self):
        decision = decide(
            make_snapshot(
                verified_merge=make_merged(),
                pending_attempt=PendingAttempt(
                    PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS
                ),
                selection_main_sha=OLD_MAIN,
                main_sha=MAIN,
            )
        )
        self.assertEqual(decision.kind, DecisionKind.TRACK_PUBLICATION)
        self.assertEqual(decision.plan_id, PLAN)

    def test_later_main_commit_requires_rescan_before_stale_closure(self):
        stale = make_pr(
            number=927,
            source_sha="1" * 40,
            stale_source_is_ancestor_of_replacement=True,
        )
        decision = decide(
            make_snapshot(
                records=(stale,),
                verified_merge=make_merged(),
                pending_attempt=PendingAttempt(
                    PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS
                ),
                selection_main_sha=OLD_MAIN,
                main_sha=MAIN,
            )
        )
        self.assertEqual(decision.kind, DecisionKind.RESCAN)

    def test_later_main_commit_requires_rescan_before_linking_or_closing_stale_pr(self):
        attempt = PendingAttempt(PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS)
        for linked in (False, True):
            stale = make_pr(
                number=927,
                source_sha="1" * 40,
                stale_source_is_ancestor_of_replacement=True,
                replacement_link_recorded=linked,
            )
            with self.subTest(linked=linked):
                decision = decide(
                    make_snapshot(
                        records=(stale,),
                        verified_merge=make_merged(),
                        pending_attempt=attempt,
                        selection_main_sha=OLD_MAIN,
                        main_sha=MAIN,
                    )
                )
                self.assertEqual(decision.kind, DecisionKind.RESCAN)

    def assert_stale_action_requires_write_preconditions(self, linked):
        stale = make_pr(
            number=927,
            source_sha="1" * 40,
            stale_source_is_ancestor_of_replacement=True,
            replacement_link_recorded=linked,
        )
        attempt = PendingAttempt(PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS)
        snapshot = make_snapshot(
            records=(stale,),
            verified_merge=make_merged(),
            pending_attempt=attempt,
        )
        cases = {
            "missing rules": (
                replace(snapshot, rules=None),
                DecisionKind.BLOCKED,
            ),
            "incomplete rules": (
                replace(snapshot, rules=replace(make_rules(), complete=False)),
                DecisionKind.BLOCKED,
            ),
            "incomplete workflow mapping": (
                replace(
                    snapshot,
                    rules=replace(make_rules(), workflow_mapping_complete=False),
                ),
                DecisionKind.BLOCKED,
            ),
            "stale rules": (
                replace(snapshot, rules=make_rules(OLD_MAIN)),
                DecisionKind.RESCAN,
            ),
            "unknown workflow run": (
                replace(
                    snapshot,
                    active_runs=(
                        ActiveRun(
                            700,
                            RunEvent.PUSH,
                            RunStatus.UNKNOWN,
                            HEAD,
                        ),
                    ),
                ),
                DecisionKind.BLOCKED,
            ),
        }
        for publication in (
            PublicationStatus.PAUSED,
            PublicationStatus.CORRUPT,
            PublicationStatus.NEEDS_SOURCE_FIX,
            PublicationStatus.UNKNOWN,
        ):
            cases[f"{publication.value} publication"] = (
                replace(
                    snapshot,
                    pending_attempt=PendingAttempt(PLAN, OLD_MAIN, publication),
                ),
                DecisionKind.BLOCKED,
            )

        stale_action = DecisionKind.CLOSE_STALE if linked else DecisionKind.LINK_STALE
        for label, (unsafe_snapshot, expected) in cases.items():
            with self.subTest(linked=linked, condition=label):
                decision = decide(unsafe_snapshot)
                self.assertEqual(decision.kind, expected)
                self.assertNotEqual(decision.kind, stale_action)

    def test_stale_link_waits_for_write_preconditions(self):
        self.assert_stale_action_requires_write_preconditions(linked=False)

    def test_stale_close_waits_for_write_preconditions(self):
        self.assert_stale_action_requires_write_preconditions(linked=True)

    def test_later_main_commit_requires_rescan_before_retry(self):
        decision = decide(
            make_snapshot(
                pending_attempt=PendingAttempt(
                    PLAN, OLD_MAIN, PublicationStatus.TRANSIENT_FAILURE,
                    assets_verified=True,
                ),
                selection_main_sha=OLD_MAIN,
                main_sha=MAIN,
            )
        )
        self.assertEqual(decision.kind, DecisionKind.RESCAN)

    def test_transient_retry_requires_the_same_attempt_assets(self):
        blocked = decide(
            make_snapshot(
                pending_attempt=PendingAttempt(
                    PLAN,
                    OLD_MAIN,
                    PublicationStatus.TRANSIENT_FAILURE,
                    assets_verified=False,
                )
            )
        )
        self.assertEqual(blocked.kind, DecisionKind.BLOCKED)

        retry = decide(
            make_snapshot(
                pending_attempt=PendingAttempt(
                    PLAN,
                    OLD_MAIN,
                    PublicationStatus.TRANSIENT_FAILURE,
                    assets_verified=True,
                )
            )
        )
        self.assertEqual(retry.kind, DecisionKind.RETRY_FROZEN)
        self.assertEqual(retry.plan_id, PLAN)

    def test_stale_merge_parent_mismatch_blocks_before_closing_old_pr(self):
        stale = make_pr(
            number=927,
            source_sha="1" * 40,
            stale_source_is_ancestor_of_replacement=True,
        )
        decision = decide(
            make_snapshot(
                records=(stale,),
                verified_merge=make_merged(first_parent=CHANGED_HEAD),
                pending_attempt=PendingAttempt(
                    PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS
                ),
            )
        )
        self.assertEqual(decision.kind, DecisionKind.BLOCKED)
        self.assertNotEqual(decision.pr, stale.key)

    def test_stale_pr_is_linked_then_closed_after_verified_replacement_merge(self):
        stale = make_pr(
            number=927,
            source_sha="1" * 40,
            stale_source_is_ancestor_of_replacement=True,
        )
        attempt = PendingAttempt(PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS)
        first = decide(
            make_snapshot(
                records=(stale,),
                verified_merge=make_merged(),
                pending_attempt=attempt,
                main_sha=OLD_MAIN,
                selection_main_sha=OLD_MAIN,
                rules=make_rules(OLD_MAIN),
            )
        )
        self.assertEqual(first.kind, DecisionKind.LINK_STALE)
        self.assertEqual(first.pr, stale.key)

        linked = replace(stale, replacement_link_recorded=True)
        second = decide(
            make_snapshot(
                records=(linked,),
                verified_merge=make_merged(),
                pending_attempt=attempt,
                main_sha=OLD_MAIN,
                selection_main_sha=OLD_MAIN,
                rules=make_rules(OLD_MAIN),
            )
        )
        self.assertEqual(second.kind, DecisionKind.CLOSE_STALE)
        self.assertEqual(second.pr, stale.key)

    def test_merge_requires_app_bound_checks_and_independent_current_verdict(self):
        ready = make_pr()
        self.assertEqual(decide(make_snapshot(records=(ready,))).kind, DecisionKind.MERGE)

        no_verdict = replace(
            ready,
            verdict=IndependentVerdict(status=VerdictStatus.MISSING),
        )
        self.assertEqual(
            decide(make_snapshot(records=(no_verdict,))).kind,
            DecisionKind.NEED_VERDICT,
        )

        wrong_app = replace(
            ready,
            checks=(make_check("ci-gate", 1, HEAD, CheckStatus.SUCCESS),),
        )
        self.assertEqual(
            decide(make_snapshot(records=(wrong_app,))).kind,
            DecisionKind.BLOCKED,
        )

        action_required = replace(
            ready,
            checks=(make_check("ci-gate", 15368, HEAD, CheckStatus.ACTION_REQUIRED),),
        )
        self.assertEqual(
            decide(make_snapshot(records=(action_required,))).kind,
            DecisionKind.WAIT_CHECKS,
        )

        approved = decide(
            make_snapshot(
                records=(replace(ready, checks=()),),
                active_runs=(
                    ActiveRun(
                        500,
                        RunEvent.PULL_REQUEST,
                        RunStatus.COMPLETED,
                        HEAD,
                        pull_request=ready.key,
                        workflow_path=sorted(WORKFLOW_PATHS)[0],
                        actor_id=ready.author_id,
                        conclusion=RunConclusion.ACTION_REQUIRED,
                        authoritative=True,
                    ),
                ),
            )
        )
        self.assertEqual(approved.kind, DecisionKind.APPROVE_ACTIONS)
        self.assertEqual(approved.pr, ready.key)
        self.assertIn(("runs", "500"), approved.details)

        unrelated_run = decide(
            make_snapshot(
                records=(replace(ready, checks=()),),
                active_runs=(
                    ActiveRun(
                        501,
                        RunEvent.PULL_REQUEST,
                        RunStatus.COMPLETED,
                        HEAD,
                        pull_request=ready.key,
                        workflow_path=".github/workflows/dependabot-auto-merge.yml",
                        actor_id=ready.author_id,
                        conclusion=RunConclusion.ACTION_REQUIRED,
                        authoritative=True,
                    ),
                ),
            )
        )
        self.assertEqual(unrelated_run.kind, DecisionKind.WAIT_CHECKS)

    def test_zero_check_runs_approves_only_required_action_required_workflows(self):
        candidate = replace(make_pr(), checks=())
        runs = tuple(
            ActiveRun(
                run_id,
                RunEvent.PULL_REQUEST,
                RunStatus.COMPLETED,
                HEAD,
                pull_request=candidate.key,
                attempt=1,
                workflow_path=path,
                actor_id=candidate.author_id,
                conclusion=RunConclusion.ACTION_REQUIRED,
                authoritative=True,
            )
            for run_id, path in zip((501, 502, 503), sorted(WORKFLOW_PATHS))
        ) + (
            ActiveRun(
                504,
                RunEvent.PULL_REQUEST,
                RunStatus.COMPLETED,
                HEAD,
                pull_request=candidate.key,
                workflow_path=".github/workflows/dependabot-auto-merge.yml",
                actor_id=candidate.author_id,
                conclusion=RunConclusion.ACTION_REQUIRED,
                authoritative=True,
            ),
        )
        decision = decide(make_snapshot(records=(candidate,), active_runs=runs))
        self.assertEqual(decision.kind, DecisionKind.APPROVE_ACTIONS)
        self.assertEqual(decision.pr, candidate.key)
        self.assertEqual(dict(decision.details)["runs"], "501, 502, 503")
        self.assertIn(".github/workflows/neo-ci.yml", dict(decision.details)["workflows"])
        self.assertEqual(
            dict(decision.details)["workflows"], ", ".join(sorted(WORKFLOW_PATHS))
        )

        after_approval = decide(make_snapshot(records=(candidate,)))
        self.assertEqual(after_approval.kind, DecisionKind.WAIT_CHECKS)

    def test_action_approval_requires_exact_pull_request_run_identity(self):
        candidate = make_pr(
            checks=(),
        )
        run = ActiveRun(
            500,
            RunEvent.PULL_REQUEST,
            RunStatus.COMPLETED,
            HEAD,
            pull_request=candidate.key,
            attempt=1,
            workflow_path=sorted(WORKFLOW_PATHS)[0],
            actor_id=candidate.author_id,
            conclusion=RunConclusion.ACTION_REQUIRED,
            authoritative=True,
        )
        wrong_runs = (
            replace(run, event=RunEvent.WORKFLOW_DISPATCH),
            replace(run, head_sha=MAIN),
            replace(run, pull_request=PRKey("github.com", "other/repo", 931)),
            replace(run, workflow_path=".github/workflows/other.yml"),
            replace(run, actor_id=999),
            replace(run, authoritative=False),
        )
        for wrong_run in wrong_runs:
            with self.subTest(run=wrong_run):
                decision = decide(
                    make_snapshot(records=(candidate,), active_runs=(wrong_run,))
                )
                self.assertNotEqual(decision.kind, DecisionKind.APPROVE_ACTIONS)

    def test_unresolved_thread_blocks_merge_even_with_other_current_evidence(self):
        record = make_pr(
            review=ReviewEvidence(
                complete=True,
                head_sha=HEAD,
                base_sha=MAIN,
                decision=ReviewDecision.NONE,
                approvals=0,
                unresolved_threads=("thread-1",),
            )
        )
        decision = decide(make_snapshot(records=(record,)))
        self.assertEqual(decision.kind, DecisionKind.WAIT_REVIEW)

    def test_unknown_review_decision_blocks_even_when_zero_approvals_are_required(self):
        record = make_pr(
            review=ReviewEvidence(
                complete=True,
                head_sha=HEAD,
                base_sha=MAIN,
                decision=ReviewDecision.UNKNOWN,
                approvals=0,
            )
        )
        self.assertEqual(decide(make_snapshot(records=(record,))).kind, DecisionKind.BLOCKED)

    def test_required_check_evidence_must_bind_the_candidate_head(self):
        record = make_pr(
            checks=(make_check("ci-gate", 15368, CHANGED_HEAD, CheckStatus.SUCCESS),)
        )
        decision = decide(make_snapshot(records=(record,)))
        self.assertEqual(decision.kind, DecisionKind.WAIT_CHECKS)

    def test_skipped_required_check_blocks(self):
        record = make_pr(
            checks=(make_check("ci-gate", 15368, HEAD, CheckStatus.SKIPPED),)
        )
        self.assertEqual(decide(make_snapshot(records=(record,))).kind, DecisionKind.BLOCKED)

    def test_current_success_supersedes_historical_failed_rerun_attempt(self):
        record = make_pr(
            checks=(
                make_check(
                    "ci-gate", 15368, HEAD, CheckStatus.FAILURE,
                    run_id=501, attempt=1, authoritative=False,
                ),
                make_check(
                    "ci-gate", 15368, HEAD, CheckStatus.SUCCESS,
                    run_id=501, attempt=2, authoritative=True,
                ),
            )
        )
        self.assertEqual(decide(make_snapshot(records=(record,))).kind, DecisionKind.MERGE)

    def test_multiple_authoritative_records_for_one_required_context_block(self):
        record = make_pr(
            checks=(
                make_check("ci-gate", 15368, HEAD, CheckStatus.SUCCESS, run_id=501),
                make_check("ci-gate", 15368, HEAD, CheckStatus.SUCCESS, run_id=502),
            )
        )
        self.assertEqual(decide(make_snapshot(records=(record,))).kind, DecisionKind.BLOCKED)

    def test_unknown_stale_preparation_remains_open_after_replacement_merge(self):
        unknown = make_pr(
            number=927,
            source_sha="1" * 40,
            proof=make_proof(failed=frozenset({ProofCheck.GENERATED_DIFF})),
            stale_source_is_ancestor_of_replacement=True,
        )
        decision = decide(
            make_snapshot(
                records=(unknown,),
                verified_merge=make_merged(),
                pending_attempt=PendingAttempt(PLAN, OLD_MAIN, PublicationStatus.IN_PROGRESS),
            )
        )
        self.assertEqual(decision.kind, DecisionKind.TRACK_PUBLICATION)
        self.assertNotEqual(decision.pr, unknown.key)

    def test_unverified_same_source_pr_does_not_block_a_proven_candidate(self):
        candidate = make_pr()
        unknown = make_pr(
            number=932,
            proof=make_proof(failed=frozenset({ProofCheck.GENERATED_DIFF})),
        )
        decision = decide(make_snapshot(records=(candidate, unknown)))
        self.assertEqual(decision.kind, DecisionKind.MERGE)
        self.assertEqual(decision.pr, candidate.key)


class TrustedVerifierTests(unittest.TestCase):
    def test_candidate_modified_checker_cannot_validate_its_generated_diff(self):
        root = Path(__file__).resolve().parents[4]
        release_scripts = root / ".agents/skills/neohaskell-release/scripts"
        sys.path.insert(0, str(release_scripts / "releases"))
        import test_release as fixture_module

        fixture = fixture_module.Provenance("test_generated_exception_checks_whole_diff")
        fixture.setUp()
        self.addCleanup(fixture.doCleanups)
        fixture.write(".github/workflows/checks.yml", "name: checks\n")
        fixture.write("neo/Cargo.toml", "[package]\nname = \"neo\"\nversion = \"0.4.1\"\n")
        fixture.write("flake.nix", "{}\n")
        fixture.commit("test: add checkout markers")
        fixture.note()
        plan = fixture_module.release.plan_release(
            fixture.repo,
            "HEAD",
            "platform",
            {},
            "2026-09-15",
            ("neo-v0.4.1", "0.4.2"),
        )
        fixture.prepare(plan)
        env = {**os.environ, "NEOHASKELL_ROOT": str(fixture.root)}
        trusted_cli = release_scripts / "semantic-release"
        args = ["check", "--base", plan["source_sha"]]
        before = subprocess.run(
            [sys.executable, str(trusted_cli), *args],
            env=env, capture_output=True, text=True,
        )
        self.assertEqual(before.returncode, 0, before.stderr)

        candidate_cli = fixture.root / ".agents/skills/neohaskell-release/scripts/semantic-release"
        fixture.write(
            ".agents/skills/neohaskell-release/scripts/semantic-release",
            'print("verified generated release operation")\n',
        )
        fixture.git("add", ".")
        fixture.git("commit", "-q", "--amend", "--no-edit")
        self.assertEqual(fixture.git("rev-parse", "HEAD^1"), plan["source_sha"])
        untrusted = subprocess.run(
            [sys.executable, str(candidate_cli), *args],
            env=env, capture_output=True, text=True,
        )
        trusted = subprocess.run(
            [sys.executable, str(trusted_cli), *args],
            env=env, capture_output=True, text=True,
        )
        self.assertEqual(untrusted.returncode, 0, untrusted.stderr)
        self.assertNotEqual(trusted.returncode, 0)
        self.assertIn("Unaccounted generated diff", trusted.stderr)


if __name__ == "__main__":
    unittest.main()
