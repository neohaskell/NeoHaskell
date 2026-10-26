# Operate a release preparation sweep

Use this procedure after the user explicitly asks to run the sweep. It applies
only to ordinary generated platform release preparations in
`github.com/neohaskell/NeoHaskell`. Do not use it for recovery PRs, release
source fixes, bootstrap, or another repository.

## Reconcile live state

1. Create a run receipt under
   `.agents-state/release-preparation-sweep/<UTC timestamp>/`. Keep the raw API
   responses, the decision record, each mutation request, and its readback.
2. Resolve the canonical repository with GitHub. Stop if the host, owner, or
   repository differs from `github.com/neohaskell/NeoHaskell`.
3. Read the current `main` SHA, the release manifest at that revision, every
   open PR page, all active runs of `.github/workflows/semantic-release.yml`,
   the effective branch rules, and public release state. Preserve pagination
   evidence. An incomplete or inaccessible inventory blocks every write.
4. Use `gh api --paginate --slurp` for paginated REST inventories. For review
   threads, paginate GraphQL `reviewThreads` through `pageInfo.hasNextPage`.
   Keep the raw response for every page in the receipt. Do not call an inventory
   complete because `gh pr list` returned one page.
5. Reconcile published attempts and pending attempts with the current manifest
   and public releases. Read the workflow's current operation choices before
   dispatching. Static required-check configuration is a cross-check only.
   Read the effective branch protection and rulesets for `main` from GitHub.
   If you cannot determine the effective required checks, GitHub App IDs,
   approval count, merge method, or queue requirement, stop.

Capture PRs as records keyed by `(host, repository, number)`. For each record,
store the observed head SHA, base ref and SHA, source SHA, plan ID and kind,
ownership proof, independent verdict, disposition, reviews, checks, and merge
readback. Capture the complete snapshot's main SHA, selection main SHA, PR
inventory completeness, rules, active runs, pending attempt, blockers, and any
verified merge. The [decision model](../scripts/sweep_model.py) defines these
typed values and the reducer outcomes. Do not treat its tests as live GitHub
evidence.

Candidate titles, labels, body text, and branch names are discovery hints. They
do not establish ownership. Recovery plans and PRs with a recovery marker stay
outside this sweep.

## Prove candidate ownership

Mark an open PR as a verified ordinary preparation only after all checks below
pass against the same observed head SHA and source SHA:

1. The PR is on `github.com/neohaskell/NeoHaskell`, its base repository is the
   canonical repository, and its base ref is `main`.
2. The head repository is the canonical repository. The head branch is exactly
   `release/platform-<first 16 characters of plan ID>`.
3. The author is `github-actions[bot]`. Resolve that account through GitHub and
   compare its immutable account ID and bot type with the PR API record. Do not
   trust the display login alone.
4. The body contains exactly one `Generated from main commit <source-sha>`
   marker, where `<source-sha>` is a 40-character SHA. The head commit has
   exactly one parent, and that parent equals the marked source SHA.
5. Compare the release manifest at the source and candidate head. The candidate
   adds exactly one operation across attempts and recoveries. That operation
   has `kind: release`, `group: platform`, and `bootstrap: false`. Its plan ID
   is 64 lowercase hexadecimal characters and matches the branch name.
6. Fetch the source and observed PR head into a disposable full-history
   checkout. Check out the candidate head there. Keep a separate trusted
   checkout at the marked source SHA, and verify that its revision is that
   exact SHA. Run the verifier from the trusted source checkout against the
   candidate checkout:

   ```sh
   NEOHASKELL_ROOT=<absolute-candidate-checkout> \
     <absolute-source-checkout>/.agents/skills/neohaskell-release/scripts/semantic-release \
     check --base <source-sha>
   ```

   The source checkout supplies the CLI, engine, and release configuration.
   `NEOHASKELL_ROOT` supplies the candidate's Git history and files. Never
   execute a verifier from the candidate before its complete generated diff
   passes. Require a successful generated release check. Confirm that
   `git rev-parse HEAD` in the candidate checkout equals the observed PR head
   both before and after the check. The check validates the complete diff and
   recomputes the deterministic plan identity. Do not derive the plan date
   from the current clock. Auto planning derives it from the source commit's
   UTC timestamp.

Any failed check rejects ownership. Missing evidence leaves ownership unknown.
Bind the proof to the observed head, source, and plan ID. A new head invalidates
the proof. A PR title, label, author name, source marker, or passing checks alone
never qualifies a candidate.

## Select one current preparation

1. Before merge, if the main SHA changed after selection began, discard
   evidence tied to the old main and repeat discovery, ownership checks, and
   review from the new main. After merge, keep tracking the frozen pending
   attempt through later main commits. Refresh the live inventory before
   closing an older PR or retrying a transient failure.
2. If more than one independently verified open preparation names current
   main as its source, stop and report every candidate.
3. If one verified current candidate exists, select it only when its base SHA
   also equals current main. Leave any separate
   unknown or manually changed candidate open with its evidence gap. Such a
   candidate does not block the selected verified PR.
4. If no verified current candidate exists and an unknown preparation-like PR
   names current main as source, leave it open and do not dispatch a duplicate.
   Report the missing ownership proof. Unknown stale PRs also remain open.
5. If a Semantic Release run is active, wait and rescan before dispatch or
   merge. Do not dispatch another run. Treat an unfamiliar run status as a
   blocker until you reconcile it. An Action-required `pull_request` run on a
   selected candidate follows the approval procedure below.
6. If there is no current candidate, pending attempt, or active run, re-read
   main, the manifest, PR inventory, and run inventory. Dispatch `auto` on
   `main` once with `gh workflow run semantic-release.yml --repo
   neohaskell/NeoHaskell --ref main -f operation=auto`. Record the run ID and
   confirm its source and generated PR before continuing. If dispatch times
   out, reconcile live state before retrying it.

The workflow reuses a deterministic branch only when its generated tree and
sole parent still match. Never rewrite a changed branch or combine generated
files by hand.

## Carry review findings forward

For every older preparation, inspect all current review threads and actionable
review comments. Link each actionable finding to its original PR and thread.
Check the finding against the selected PR's diff. Resolve it on the selected PR
only when its diff addresses the issue. Dismiss it there with a specific reason
when the new diff makes it irrelevant. Do not transfer approvals or verdicts
from an older PR.

## Approve only selected candidate workflow runs

An `action_required` workflow-run conclusion permits approval only when exactly
one preparation is verified for current main. This state is separate from
required check evidence. GitHub can report completed `pull_request` workflow
runs with conclusion `action_required` before any required CheckRun exists.
Do not require a check-suite join in that case. Missing required checks remain a
merge blocker after approval.

Read the [workflow runs API](https://docs.github.com/en/rest/actions/workflow-runs)
and preserve each run's ID, current `run_attempt`, event, status, conclusion,
head SHA, workflow path, actor ID, and `pull_requests` association. Match all
fields to the selected PR and its proven Actions author. Read the current
workflow definitions and effective branch rules to identify the workflow paths
that can produce required contexts. Approve only authoritative current
`pull_request` runs on the selected head from those paths. Do not approve the
   `auto` run on main or unrelated workflows such as Dependabot auto-merge. The
   observed blocked release candidate has Test (`test.yml`), checks
   (`checks.yml`), and Neo CLI (`neo-ci.yml`) runs that can produce required
   contexts; the Dependabot auto-merge run is unrelated.

Immediately re-read the PR and each selected run before approval. Approve each
matching run with
`gh api --method POST repos/neohaskell/NeoHaskell/actions/runs/<run-id>/approve`.
This is GitHub's [approve workflow run endpoint](https://docs.github.com/en/rest/actions/workflow-runs#approve-a-workflow-run-for-a-fork-pull-request).
Then read back the runs, rediscover required checks, and wait until every live
required context from its required App succeeds on the exact candidate head.
Do not approve a run for an unknown, stale, recovery, or otherwise unselected
PR. If event, workflow path, actor, PR, current attempt, or SHA does not match,
leave that run untouched and report the mismatch.

When GitHub lists prior attempts for a workflow or required check, keep each
run ID and attempt. Use the live current attempt as authoritative. An older
success cannot override a newer failure or pending attempt. An older failure
does not block a newer current success. If GitHub's current attempt is unclear,
wait for a fresh read instead of guessing.

## Verify the merge gate

Before merge, require every item below for the same candidate head and current
main:

1. An independent verifier returns `PASS` or `PASS+NOTES` and names the exact
   head SHA and main SHA. The verifier must differ from the PR author. Record
   the evidence using [Verify This](../../verify-this/SKILL.md). A green CI run
   is not an independent verdict.
2. Review the generated release notes and any breaking migration against the
   maintained API. Follow [Writing for Jess](../../neohaskell-release/references/jess-writing.md).
   Check copyable commands and the agent prompt when a migration is present.
3. Read all review threads and current review decisions. Require complete
   thread pagination, no unresolved blocking thread, no current changes
   requested, and at least the live required approval count.
4. Require every effective required check from its required GitHub App to
   report success on the exact candidate head. Use the live rules for required
   contexts and App IDs. Cross-check the current
   [required-check configuration](../../neohaskell-release/references/required-checks.json),
   but do not substitute it for live rules.
5. Require GitHub to report the exact head as mergeable. Follow the effective
   merge method and queue requirement. Never bypass a queue, review, check, or
   protection rule.

Before merging, re-read main, the PR head, base, source proof, required checks,
check run IDs and attempts, reviews, threads, branch rules, and mergeability.
If any input changed, invalidate the affected evidence and restart selection
and verification. Submit a squash merge with the verified head:

```sh
gh pr merge <number> --repo neohaskell/NeoHaskell --squash --match-head-commit <head-sha>
```

If branch rules require a merge queue, use the queue and wait for its result.
Read back the PR state, merge time, merge commit, and merge commit parents.
Require the merge commit's first parent to equal the verified source SHA. If it
does not, stop. Do not close older PRs or dispatch another preparation.

## Close only proven stale preparations

After the replacement merge passes readback, consider older open PRs one at a
time. Re-read each PR and prove that its ownership still matches its observed
head. Prove with full Git history that its source SHA is an ancestor of the
replacement's source SHA. A time comparison or title is not proof of staleness.

Link the replacement PR in the old PR's comments, unless that exact link is
already present. Read the comment back. Re-read the PR once more, then close it
and confirm the closed state. Leave any unverified, manually altered, unrelated,
or ancestry-uncertain PR open with a reason. Never close an unverified PR.

## Track publication

After a verified merge, track the same immutable plan through `platform_seed`,
every `platform_native` target in the frozen plan's target policy,
`platform_consumer`, and `platform_publish`. Use the plan's recorded target
policy. Current plans use `native-v2`; historical plans without that field keep
their frozen target set. The best-effort `platform_cache` job is not a release
gate.

Verify the public release against the frozen plan and merge revision. Require
the release to be stable, not draft or prerelease. Compare its body with the
rendered notes. Confirm that `neo-vX` and `vX` point to the same revision.
Download the assets and verify the exact asset set, `release-manifest.json`,
`SHA256SUMS`, every checksum, the recorded plan, and the published revision.
Use the release engine's remote verifier in
`../../neohaskell-release/scripts/releases/github.py` as the source for the
asset contract.

If the manifest has a pending attempt, create no new preparation. Track it to
completion. Retry a transient failure with `auto` on main only after verifying
that the same immutable plan, revision, and assets remain valid. A later main
commit does not change a merged attempt's frozen source. Stop and hand off to
[NeoHaskell release](../../neohaskell-release/SKILL.md) for a paused or corrupt
attempt, a required source fix, or a merge first-parent mismatch.

## Report the evidence

For each PR, report its URL, disposition, observed head, base, source, plan ID,
ownership proof, independent verdict, required check results, and any reason it
remains open. Report the current main SHA, selected PR, run and approval IDs,
merge commit and first parent, stale PR links and closure results, and the
publication URL and asset verification. State incomplete discovery or any
blocker. Do not report a release as complete until public publication
readback passes.
