---
name: neohaskell-change
description: Stage playbook for executing steps of the `change` formula v2 (ADR-0075) — the 5-step molecule that takes a request from spec to merged PR. Use when claiming or executing any bead poured from the change formula (spec, spec-approval, build, verify, pr), when parking a stuck step, or when recording a spec approval. Covers the failure policy (time-box, retry, model escalation, park), the V1-V9 verify verdict, and the auto-merge rule. Do not use the retired neohaskell-pipeline skill (docs/legacy/) or re-create per-step micro-beads.
---

# The change process v2 (ADR-0075)

One molecule per request, five coarse beads:

```text
spec ══ GATE 1 (Nick+Fable) ══▶ build ─ verify ─▶ pr ─ (auto-merge | GATE 2)
```

Authoritative step definitions live in `.beads/formulas/change.formula.toml`
— read your step's description there before starting. This skill adds the
operational detail shared across steps.

## Model assignment (fixed, ADR-0075)

| Step | Model | Notes |
|---|---|---|
| spec | opus | intake + binding localization + spec, one pass |
| spec-approval | — | human gate; Fable converses with Nick, edits spec, records approval; Fable never executes pipeline work |
| build | sonnet | escalate to opus after 2 failed repair rounds on the same error |
| verify | opus | FRESH agent — never the build agent; spawns a sonnet subagent for V9 |
| pr | haiku + sonnet | haiku watches CI/mechanics; sonnet triages bot findings |

## Failure policy (every step except gates)

Time-boxes (wall-clock, excluding `waiting_on_human`): spec 45m · build 90m
· verify 30m · pr 45m. On breach:

1. **Retry once** — fresh attempt, same binding plan.
2. **Escalate model** one tier (sonnet→opus); note the escalation on the bead.
3. **Park**: `bd defer <id>` + failure label + a structured report in the
   bead notes: step, elapsed, label, last error verbatim, what was tried.
   A parked report beats a wrong PR — parking is the process succeeding at
   honesty. Never park a gate: waiting on Nick is `waiting_on_human`.

**Class-fix on repeat (delta-si-repite):** before closing a parked/failed
bead, look for the same failure label on prior beads:
`bd list --label <label>` plus `bd list --label <label> --status=closed`
(`bd search` matches titles/IDs only — it will NOT find labels and always
reports zero). Second occurrence of the same label → closing
REQUIRES shipping a class fix alongside the retry (a doc line, an alias, a
lint rule, a hook — whatever kills the category). First occurrence → no
tax, just the report.

## Telemetry

Free via beads — do not add ritual: stage timings come from bead status
transitions; token cost per stage comes from the `bd-token-tracking` hook
(`tokens`, `tokens_out`, `tokens_scope` in metadata). Do not create
runs.jsonl, golden archives, or consult logs (retired with ADR-0075).

## Gate mechanics

- **GATE 1 (spec-approval):** on entering the step, flag it:
  `bd update <id> --add-label human` — the `human` label is what
  `bd human list` reads, and the dispatcher notifies Nick from there
  (`bd human <id>` is NOT a command; it prints a help menu and exits 0
  without flagging anything). Approval happens in a Nick+Fable
  conversation; Fable records it: resolve the gate and add a bead note
  `approved-by: Nick · via: fable-session · <timestamp> · spec-rev: <sha>`.
  Advancing without the recorded approval is refused. If the conversation
  changed the contract delta, re-run `./dev spec-check` before resolving.
- **GATE 2 (merge, conditional):** only exists when the approved spec says
  `breaking: true` or carries an ADR. Create it from the pr step —
  `bd gate create --type=human --blocks <pr-bead-id> --reason="GATE 2:
  breaking change, Nick merges"` (`--blocks` is required: it names the bead
  held back) — and flag with `bd update <pr-bead-id> --add-label human`.
  Only Nick merges through this gate.
- **Auto-merge (the non-breaking path):** requires ALL of: CI matrix green,
  review bots settled, V1–V9 verdict complete on the verify bead, spec
  `breaking: false`, no public-API surface beyond the declared delta.
  Record `merged-by: auto` on the pr bead. Any doubt about eligibility →
  treat as GATE 2; a wrong auto-merge costs more than a wait.

## The V1–V9 verify verdict

Written to the verify bead as a table, each row PASS/FAIL + one line of
evidence (command + result, or file:line). All nine PASS = merge
authorization. Any FAIL → bounce to build (max 2 bounces, then park).

| # | Check | How |
|---|---|---|
| V1 | Compiles clean | `./dev check`, no new warnings |
| V2 | Tests prove the change | each criterion test RED on base, GREEN on head |
| V3 | No regressions | impact globs + `./dev test-all` (once, here only) |
| V4 | Exact contract | `./dev spec-drift` clean vs approved delta |
| V5 | Dialect/composability | new API conforms to `neohaskell-dialect-rules` |
| V6 | Expectations intact | no existing test weakened; edits justified |
| V7 | Scope fence | diff within binding `touches:`/`files:` |
| V8 | Lint | `./dev lint` green |
| V9 | Security/perf sanity | sonnet subagent, checklist below, max 2 iterations |

V2 is the anti-tautology lock: a test that passes on both branches proves
nothing — run the criterion tests against the base branch and demand red.
V5 is where NeoHaskell's bet lives: correctness comes from the design of
composable primitives, so dialect conformance is the one judgment check
that can never be skipped.

### The V9 checklist (concrete, no ISO ceremony)

**Partition rule (ADR-0075): whatever is deterministic runs as a
deterministic tool, fast, in CI — agents only check what needs judgment.**
Secret scanning is CI's job (the `secrets` gitleaks job in `checks.yml`),
not V9's; never add an agent check for something a linter/scanner already
enforces, and when a V9 item becomes mechanizable, move it to CI and
delete it here.

Run by a **sonnet subagent** spawned from verify, reading only the diff,
the spec, and the tests. Max 2 iterations; each finding is one line
(file:line + what + why it bites). Anything beyond this list is out of
scope — V9 is a sanity pass, not an audit:

1. External input crossing a trust boundary is validated (parse, bounds).
2. No shell or SQL built by concatenating external input.
3. Resources (handles, threads, connections) released on error paths.
4. If the contract involves shared state or parallelism: the criteria
   tests actually STRESS concurrency — parallel invocations, interleaved
   operations, race exposure. A concurrent contract with only sequential
   tests is a FAIL.
5. No unbounded accumulation (state that only grows) and no
   obviously-quadratic loop over user-sized input on a hot path.

## What is deliberately NOT here (ADR-0075, reversible)

- Heavyweight security/perf design-review stages — downscoped to the V9
  sanity pass; restore the full reviews when there is real user data to
  protect or production load to serve.
- Docs sub-pipeline (draft/widgets/reconcile/fragments) — on maintainer
  hold until the docs system exists.
- Manual telemetry (runs.jsonl, golden, consult-logging, weekly miner).
- Per-step micro-beads: never split these 5 steps into finer beads.
