# Keep resumable PR receipts

Store one record per forge host, repository, and PR number under repository-root `.agents-state/dependabot-sweep/`. Reuse records across invocations. Keep run inventories and detailed logs alongside them. Store repository-relative evidence paths or durable links, never credentials.

## PR record

Use a keyed record with these fields. Add detail only when the PR requires it.

| Field | Contents |
| --- | --- |
| Identity | Forge host, repository ID and name, PR number and URL, verified Dependabot account ID and bot type. |
| Candidate | Current state, head repository and ref, head SHA, base ref and SHA, and observation time. |
| Owner | Active writer identity, checkout or branch, last acknowledged remote head, and transfer status. |
| Assessment | Worthwhile, unsuitable, or unresolved, with dependency usage, upstream changes, benefit, adoption cost, and evidence. |
| Repair | Scoped plan, attempts, results, commits, and retry classification. |
| Verification | Verifier identity, verdict, tested head and base, relevant inputs, commands, results, limitations, and evidence paths. |
| Disposition | Pending, merged, closed as unsuitable, open and blocked, externally closed, unknown, or unprocessed. Include the observed forge state. |
| Blocker | Exact cause, evidence, attempted repairs and retries, and continuation condition. |
| Comment | Purpose, stable marker, forge comment ID, author ID, saved body, and last readback. |
| Actions | Intent, expected candidate, request result, readback result, and any merge commit. |

Keep assessment and disposition separate. `worthwhile` with failing CI means `open and blocked` once its comment is confirmed. It does not mean `unsuitable`. `merged` requires a confirmed merge commit. `closed as unsuitable` requires the saved reason and confirmed closure. `open and blocked` requires a confirmed open state and saved blocker comment. An unconfirmed action is pending or unknown.

## Reconcile ownership and comments

Before resuming, query the live PR, relevant evidence, and every page of its comments. A local receipt is a lookup aid, not proof of forge state. Stop a prior writer or establish that it is no longer active before transferring ownership. If that cannot be established, keep the PR blocked rather than run two writers.

Identify each sweep-owned comment with a stable marker, for example `<!-- dependabot-sweep:github.com:owner/repo:123:status -->`. Use the same status comment for blockers and an eventual suitability reason. Match the marker, PR identity, and authenticated comment author before editing. Another author's copied marker does not grant ownership.

If the receipt's comment ID still matches, update it only when the facts changed. If the receipt is missing, find the owned marker in paginated comments and recover the ID. If no owned comment exists, create it and read it back before recording success. Preserve unrelated comments. Resolve ambiguous multiple owned matches from current evidence before creating another.

After a failed or timed-out comment request, fetch the current comments before retrying. Recover a successfully saved comment even when the original response was lost. If writing still fails, preserve the error and report incomplete work. Never record a comment as saved based on request intent alone.

## Recover interrupted actions

Write each updated receipt to a temporary file and atomically replace its prior record. Record an action intent before calling the forge. Record the observed result afterward. On restart, reconcile each intent against the live PR and comment history before deciding whether another mutation is needed.

If the PR is already merged or closed, retain who performed the action when known. Do not repeat a merge, closure, or reason comment to make the local record look complete. If a blocked PR's head or relevant base changed, refresh assessment and verification before continuing. If it remains blocked for the same reason, reuse its comment.
