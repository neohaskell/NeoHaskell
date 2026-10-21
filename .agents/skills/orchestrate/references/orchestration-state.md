# Orchestration state

The coordinator keeps one program under repository-root `.agents-state/orchestrate/<program>/`. Run bookkeeping on one host; the PID lock does not coordinate separate machines. Workers publish reports through the available agent interface; the configured state writer records them. This helper never spawns agents, grants approval or merges PRs.

Use Node 24.2+ and the Commander dependency declared in the workflow's scripts package. Arrange dependency installation explicitly using the repository's package manager; startup, imports and help never install dependencies. Resolve `.agents/workflow` from `AGENTS.md`.

```sh
node .agents/skills/orchestrate/scripts/orch.ts --store <absolute-store-path> init
```

Below, `orch` stands for that same command prefix. Pass the explicit store on every invocation. All commands support `--json` before their subcommand. Canonical TSV/JSON remains readable without the helper. Unrecognized file headers fail rather than being silently migrated.

## Record verification

Keep a receipt with actual commands, expectations, observed results, tested repository/head/base, limitations and verifier provenance. Keep the relevant-input manifest alongside it and hash its contents. Include the compiler, dependencies, flags, generator sources, fixtures and runtime configuration that can affect the claim, per Shipping. The caller establishes the manifest's completeness and freshness; a supplied digest cannot discover an omitted dependency.

```sh
orch ledger record <pr> <tested-head> behavior-verified \
  --repo <owner/repo> --base <tested-base> --inputs <manifest-digest> \
  --claim behavior --method '<actual verification method>' \
  --evidence <receipt-path-relative-to-store> \
  --author <code-author> --verifier <independent-verifier> --independent
orch ledger check <pr> <tested-head> \
  --repo <checkout-directory> --base <tested-base> --inputs <current-manifest-digest> \
  --claim behavior
```

Use `compiler-verified --claim compiler` for a compiler/generated-API contract; `behavior-verified --claim behavior` for demanded runtime behavior; `artifact-verified --claim artifact` for a broader real-artifact acceptance contract described in the receipt. Labels match claims, not a quality ranking. Use `verifier-blocked` or `verifier-failed` for non-passes. Missing evidence is not a pass. Independent identities must differ from the code author's; those identities and authorship separation are supplied facts to audit, not authenticated by this helper.

A worker can record a self-check without `--independent`. It remains separate from independent proof. Records retain provenance and failures; repeating the current identical report is idempotent, while a fresh verdict after failure is a new event. Summary/status ledger counts include historical records and do not count landed or currently verified PRs.

`ledger check` reads the current GitHub repository and PR identity/head/base/state, requires an open matching candidate and a matching independent passing row with an extant receipt, and rejects changed inputs or claim kinds. It does not execute the receipt, discover relevant inputs, evaluate review threads/branch rules or authorize landing. Shipping and Babysit still govern those decisions. Reassess affected evidence after a candidate or input changes; independently explain any unaffected carry-forward before recording the new candidate.

## Refresh the frontier

```sh
orch frontier set --repo <checkout-directory> --prs <expected-root,...,expected-tip>
orch frontier show
```

`--prs` is an optional order pin. GitHub CLI supplies the real default branch, published head/base identities and PR states. The current checkout branch identifies the chain; local branch tips do not supply its PR SHAs. The helper checks observed parent/head coherence and rejects missing, ambiguous, unknown or non-linear observations without advancing the saved generation. A closed unmerged prerequisite with live descendants remains in the chain and blocks it. Abandoned historical siblings without live descendants are excluded. `lowestUnmerged` is the first link that is not actually merged, including a closed prerequisite.

Treat this as an observed generation, not an atomic or permanent GitHub snapshot. Refresh at landing boundaries, then follow Shipping/Babysit's current-candidate and merge readback rules. A verified child cannot bypass its unresolved parent.

## Drain and acknowledge completions

```sh
orch inbox push <agent> <unit> <status> --report <report-path>
orch inbox drain
```

The drain returns a batch id and pointers, each with a stable id. It moves that batch into `batches/<id>/`; new arrivals stay in `inbox/`. The next drain returns the same unacknowledged batch after interruption. `inbox peek` and `inbox count` include pending batches and new arrivals.

Classify every pointer and save the resulting unit or ledger rows first. Write an accounting JSON file covering each pointer exactly once:

```json
{
  "batch": "<batch-id>",
  "entries": [
    {"pointer": "<pointer-id>", "action": "unit", "state": "needs-verify"}
  ]
}
```

The pointer's unit must already exist in that saved state. For a verifier result, use an entry with `action: "ledger"`, `pointer`, `repo`, `pr`, `sha`, `base`, `inputs`, `claim` and `verifier`, identifying the saved row; its unit must already name that PR and head. A duplicate or irrelevant notification uses `action: "noise"` with `pointer` and a concrete `reason`. Include all pointers in the batch; partial or missing accounting fails.

```sh
orch inbox ack <batch-id> --receipt <accounting-json-path>
```

Acknowledgment saves the accounting receipt plus unit/ledger snapshots before moving the whole batch to `accounted/<id>/`. It retains the pointers for review. Repeating an acknowledgment is idempotent. On retry, reconcile existing unit ids and evidence keys, then finish the pending batch; do not count a notification twice. A crash before acknowledgment preserves the batch; a crash after the move leaves its saved accounting intact. Do not delete pending batches as a shortcut.
