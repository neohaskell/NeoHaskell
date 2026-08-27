# ADR-0076: Session-launched change process — amends ADR-0075

> **Amends, does not supersede, [ADR-0075](0075-change-process-v2.md).** The
> five steps, the model tiers, the V1–V9 verdict, the merge rule and the
> failure policy are all retained unchanged. What changes is the *substrate*:
> who launches a run and what advances it. Superseding 0075 would tell a
> future reader that its process was tried and failed — the record says it was
> never exercised at all (see Context), and erasing that would lose the most
> useful finding here.

## Status

Accepted

## Context

ADR-0075 replaced a 20-step formula with a 5-step molecule poured by a
dispatcher daemon. Between it landing (2026-08-20) and this amendment,
**zero changes shipped through it**. The reason turned out to be neither the
process design nor beads:

- **The formula never received its input.** `change.formula.toml` declared
  `[vars.request] required = true` but contained no `{{request}}` placeholder
  anywhere — `grep -c '{{'` returned 0. Every molecule poured reached its spec
  agent with no request text. Five molecules and their twenty-three step
  beads sat open. Filed as `nh-knk`, size `xs`.
- **Most remaining defects were not in this repo.** Of the recorded blockers,
  seven had their fix site in the external dispatcher repo; `nh-s3c` is
  titled "Operator queue: 7 dispatcher-repo defects NeoHaskell cannot fix".
- **"beads is broken" was a misdiagnosis, twice** (`nh-fuj`, then `nh-4s9`).
  A bare `bd` resolved to the host profile's 1.1.0 build, twelve schema
  migrations behind the project's Dolt DB. The `bd` pinned in `flake.nix` is
  1.2.2 and reads the same DB cleanly with no override. The bug was the
  binary resolution, not the tool.

That left one honest question: was the slowness the *queue* or the *store*?
Measured on this machine: a `bd` write costs ~0.9s, against per-step
time-boxes totalling 210 minutes. A run makes on the order of 12–25 state
operations. The store is ~0.1% of a run. The cost was the loop around it —
a whole agent session relaunched per step, queue latency, context rebuilt
from nothing each time, and the `pr` step's gate-and-exit dance.

A separate measurement, found while fixing the binary resolution, dominates
all of the above: on a **dirty working tree — the normal state during an
edit→verify loop — entering the flake shell costs ~24s per call**, and every
`./dev` verb was paying it. (An earlier draft attributed this specifically to
edits invalidating nix's eval cache, citing 1.5s on an unchanged tree. Audit
could not reproduce the fast half: on a dirty tree it is ~24s either way. The
narrower claim is the one that survives, and it makes the cost larger, not
smaller.)

## Decision

**1. Drop the loop, keep the store.** The queue, `bd mol pour`, the `change`
formula, the dispatcher daemon and the `neohaskell-enqueue` skill are retired.
Beads remains the run state store: **one bead per run**, carrying the binding
plan in `--design`, step/timing/verdict in `--set-metadata`, the failure label
in `--add-label`, the grill outcome and the GATE 1 approval in notes, and the
request in `--external-ref`.

**2. Runs are launched and driven from an interactive session** by the
`/change` skill, which dispatches each step to a subagent with a pinned model
(`.claude/agents/change-*.md`). Model tiers are unchanged from ADR-0075, with
one correction: the `pr` step is sonnet-only rather than haiku+sonnet. Watching
CI is shell polling, not agent work, and the same agent must judge whether a
bot finding is real — haiku is the tier least suited to that call.

**3. A mandatory grill step precedes spec.** The request is stress-tested with
Nick before any contract is written. It runs **in the session, not as a
subagent**: a subagent has no channel to a human, so a "grill" dispatched to
one would answer its own questions and return self-agreement — a fake stress
test, worse than none.

**4. The PR body is the merge authorization record.** The V1–V9 verdict is
written to both the bead and the PR body; the PR body is what makes an
unattended merge auditable by anyone later, rather than deniable state in a
local database.

**5. All tooling runs inside the pinned flake shell** (`./dev <verb>`,
`./dev bd`, `./dev exec`; long form `nix develop --command`). `./dev doctor`
fails on a bare `bd` in the executable wiring (`.claude/hooks/*`,
`.claude/settings.json`) — prose in a SKILL.md is not covered, so the rule
still needs reading as well as linting. This is the class fix for
`nh-fuj`/`nh-4s9`, which recurred because the rule existed only as habit.

**6. `scripts/with-toolchain` caches the resolved shell environment on disk**,
keyed by `scripts/toolchain-fp` and GC-rooted via
`nix print-dev-env --profile`. The fingerprint hashes only shell-*defining*
files, so it survives exactly the edits that invalidate nix's own cache.
Measured after: ~0.9s where it was ~24s. Every layer falls through to the next
on a stale, edited, empty, corrupt or garbage-collected cache rather than
running a host binary that merely shares a name — a 9-case `--self-test` in
`./dev doctor` holds that line, and its reject cases deliberately probe a name
that EXISTS ON THE HOST, because the first version probed a name present
nowhere and every reject passed for the wrong reason.

**7. `./dev process-check` is retargeted** from the deleted formula to the new
documents, and additionally fails if the formula or either retired skill
reappears — two live contradictory playbooks is the state this ADR ends.

## Consequences

**What gets better.** No queue latency, no per-step session relaunch, no
formula to pour. The maintainer starts a run when he wants one and sees each
gate in the conversation he is already in. The toolchain cache speeds up every
tool call in the repo, not only the pipeline.

**What gets worse, stated plainly:**

- **No unattended cross-session progress.** ADR-0075's premise was that the
  maintainer should not have to attend every agent block; this trades part of
  that back. A run advances while a session is driving it. Steps run as
  background agents so the session stays usable, but nothing relaunches a step
  after the session closes.
- **Two human touchpoints per run, not one.** ADR-0075's headline consequence
  ("one human wait per non-breaking change") is **explicitly retracted**: the
  mandatory grill adds a second. This is a deliberate trade of latency for a
  better-examined contract, and it should be revisited if grilling proves to
  add nothing on well-specified issues.
- **Time-boxes become judgment.** No daemon clocks a step. The orchestrator
  checks elapsed time from the bead's `t0_<step>` when an agent reports back.
- **The `pr` step loses its long-wait escape hatch.** Under the daemon it
  could create a `gh:run` gate and exit to be relaunched. Now the CI wait
  lives inside the agent's loop, and a wait that outgrows the time-box is
  reported for resumption rather than being resumed automatically.
- **Beads keeps enforcing nothing.** The retired `scripts/pipeline-state`
  refused invalid saves and mechanically blocked advancing past the spec gate.
  A bead is a filing cabinet. The gate precondition therefore lives in the
  `/change` orchestrator, and that is a weaker guarantee than a validator.

**Unchanged and deliberately so:** the five steps, the V1–V9 verdict including
the V9 sanity pass and its partition rule, the auto-merge conditions, the
failure taxonomy, the delta-si-repite class-fix rule, the scope fence, and the
beads Dolt sync to the private mirror.

**Reversibility.** The formula and the enqueue skill are recoverable from git
history, and the beads DB is untouched. Restoring the daemon path means
re-adding the formula with its `{{request}}` placeholder fixed — the one-line
bug that made this whole comparison necessary.
