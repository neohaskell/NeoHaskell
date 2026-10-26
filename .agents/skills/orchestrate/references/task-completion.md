# Complete an implementation task

For implementation and fix requests in this repository, the default delivery is
tested, independently verified work squash-merged into the default branch. The
user reviews after landing. Continue through review fixes and required checks
without asking for another human review or merge confirmation.

## Respect the requested boundary

| Request or existing constraint | Completion boundary |
| --- | --- |
| Build, implement, or fix, with no narrower boundary | Verification, review fixes, and squash landing through Shipping. |
| Explicit draft, no-merge, PR-only, or operator-lands instruction | The requested draft, open PR, or verified stack. Do not arm deferred merging. |
| Read-only status, review, investigation, design, or plan | The requested answer or artifact. This policy creates no mutation or merge grant. |
| Delegated branch or file ownership | Complete that assignment and report to its coordinator. Only its designated landing owner changes topology or merges. |
| Required external approval, safety gate, or explicit stop | Keep that gate pending and continue independent authorized work. Report the exact unmet condition. |

A later explicit instruction can change an earlier boundary. A status question
inside an ongoing implementation does not cancel its existing scope. Preserve
branch protections, required reviews, and the repository's Always-pause actions.
This policy does not authorize a force-push, deployment, deletion, or unrelated
follow-up merely because it would make completion easier.

## Represent dependencies as a real PR stack

When one task needs dependent PRs, publish an actual base-branch chain. The root
targets the discovered default branch. Each dependent PR targets its immediate
prerequisite branch and includes that prerequisite's code. A body saying
"depends on" is not a substitute for the real base relationship.

Keep one task owner and one exclusive topology owner. Record the root, tip,
ordered PR URLs, head branches and SHAs, actual base branches and SHAs, and the
lowest unmerged prerequisite. Update that map after every push, integration,
retarget, or merge. Keep parent/root/tip links in PR descriptions consistent with
the map. Independent work can remain separate when it has no code dependency.
Use the Orchestrate store only when the program warrants it. A small stack needs
only a current local map and accurate PR links.

Inspect current GitHub and CLI capabilities before choosing stack operations.
When native stack metadata is available, read its ordered entries and reconcile
them with actual PR bases and the local map. Paginate the entries. A null or
unavailable native stack does not remove an existing branch dependency. Inspect
the supported API or installed CLI help before any mutation. Read fields do not
prove that a stack creation or submission command exists. Use supported base
creation and editing operations when no applicable native mutation is available.

## Continue through verification and landing

After opening the in-scope PRs, the task owner runs
[Babysit](../../../workflow/playbooks/babysit.md), then
[Shipping](../../../workflow/playbooks/shipping.md) under the completion boundary
above. A worker's report transfers work back to that owner instead of asking the
user to supervise it. Readiness is an intermediate state when landing is in scope.

Keep Shipping's independent per-PR verdicts, exact head/base evidence, required
checks and reviews, and contiguous verified run. Merge bottom-up into the default
branch. Never merge a child into an unmerged feature branch as a substitute for
landing. Reconcile the next child through the topology owner after each confirmed
squash merge, preserving its own diff and rechecking affected evidence.

A verified-run ceiling limits what can land on existing proof. If the task still
has authorized work, obtain the next verdicts and continue. Stop only at the
requested completion boundary or a specific unresolved gate. Report actual
merged or pending state with an executive summary. Do not hand the user a review
or verification action that the agents can perform.
