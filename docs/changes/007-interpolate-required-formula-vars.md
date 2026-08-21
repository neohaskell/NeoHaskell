# Change 007: Interpolate every required formula var, and gate it mechanically

The `change` formula declares `request` as a required variable but no step
ever references `{{request}}`, so `bd mol pour` accepts the value and throws
it away. Every molecule poured since ADR-0075 reaches the spec agent carrying
only generic boilerplate, with the actual ask nowhere in the ledger. Fix the
formula so the request lands in the beads, and add a check so no formula can
ever drop a required var again.

```yaml spec
issue: adhoc:nh-knk
kind: bug
touches: [dev-pipeline]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No Haskell public API changes — this is pipeline plumbing. The surface that
does change is one `./dev` verb, which `codemap/signatures/` does not track:

- **new verb** `./dev formula-check [files]` — validates
  `.beads/formulas/*.formula.toml`; `--self-test` (deterministic, doctor/CI),
  `--pour-check` (round-trip against the real `bd` binary, SKIPs without it).

```diff signatures
```

## What was measured, not assumed

Established empirically this session against `bd 1.2.2` (probe formula, then
deleted):

| Question | Answer |
|---|---|
| Does `{{request}}` in a **step description** interpolate? | Yes — `bd cook --mode=runtime --var request=…` substitutes it. |
| Does `{{request}}` in the **formula root description** interpolate? | Yes — same mechanism. |
| Does bd enforce `required = true`? | **Only when the var is referenced.** With `{{request}}` present, `bd mol pour probe --dry-run` (no `--var`) fails with `missing required variables: request`. With it absent, `bd mol pour change --dry-run` succeeds silently. |

That last row answers the open question in nh-knk: **interpolating
`{{request}}` fixes both halves of the bug at once.** No bd-side change is
needed, and none is proposed — the enforcement was always there, gated on a
reference the formula never made.

## Criteria

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | **The repro, committed RED**: `./dev formula-check` exits 1 on today's `change.formula.toml`, naming `request` as required-but-never-referenced. After the formula fix it exits 0. | `scripts/formula-check` over `.beads/formulas/` | unit |
| C2 | The validator distinguishes the real cases: referenced in a step → pass; referenced only in the root description → pass; referenced nowhere → fail; referenced *only inside its own* `[vars.*]` block → still fail; not required and unreferenced → pass; `{{var}}` with no `[vars.var]` → fail; malformed TOML → reported, not crashed. | `scripts/formula-check --self-test` (embedded fixtures + mutation coverage, mirroring `spec-check --self-test`) | unit |
| C3 | The request text actually reaches the ledger end-to-end: pouring **the formula under review** (`--dry-run`) **fails** with `missing required variables: request` when `--var request=…` is omitted, and **succeeds** when it is supplied. | `scripts/formula-check --pour-check` (shells out to the real `bd`; SKIP-with-notice when `bd` is absent) | integration |
| C4 | An adversarial request value is inert data, not a second interpolation pass or a shell: pouring with `request={{request}} "quoted" $(id) \`whoami\`` yields a bead whose description contains that text literally — no re-substitution, no command execution, no TOML corruption. | `scripts/formula-check --pour-check`, adversarial case | integration |
| C5 | The gate is wired so a future formula cannot regress: `scripts/doctor` runs both `scripts/formula-check --self-test` and the real `scripts/formula-check`, and `./dev doctor` is green — which also means CI covers it through the existing `doctor` job, with no new workflow. | `./dev doctor` | unit |

C1 is committed red in this draft PR: `scripts/formula-check` and the
`./dev formula-check` verb ship with the spec, and the verb fails. It is
deliberately **not** wired into `scripts/doctor` yet (C5 does that) so the
draft PR's `doctor` CI job stays green while the repro stays red.

## Edge cases and failure modes

- **F1 — the dispatcher must pass the var (the one real risk).** After this
  fix, `bd mol pour change` *hard-fails* without `--var request=…`. No
  in-repo caller pours (`neohaskell-enqueue` only creates the request bead);
  the pour happens in the external dispatcher daemon (`launch.ts`). If the
  dispatcher does not pass the request, **the queue halts** — loudly, with an
  actionable `Hint: Provide them with: --var request=<value>`, instead of
  today's silent data loss. That is the correct trade: a halted queue is
  visible, a queue that runs on empty requests is not. Tracked separately for
  the dispatcher side; see the linked follow-up bead on nh-knk.
- **F2 — a placeholder inside the var's own declaration must not count.**
  Writing `description = "Referenced as {{request}}"` inside `[vars.request]`
  documents the var without interpolating it anywhere a reader sees. The
  scanner ignores `[vars.*]` blocks for exactly this reason (C2).
- **F3 — the inverse rot.** A `{{var}}` with no `[vars.var]` declaration
  renders literally into the bead. Also caught (C2).
- **F4 — malformed TOML.** Reported as a formula-level failure, never a
  traceback (C2).
- **F5 — no formulas present.** `formula-check` reports and exits 0; a repo
  without formulas is not a failure.
- **F6 — the worktree trap (verified, and designed around).** `bd` resolves
  formulas from the *resolved beads dir*, which is the **main checkout**
  (`/Users/nick/repos/NeoHaskell/.beads/formulas/`) even when invoked from a
  git worktree. A naive `bd mol pour change --dry-run` therefore tests
  `main`'s formula, not the one under review — it would have passed C3 before
  the fix ever landed. `--pour-check` sidesteps this by copying the
  repo-local formula to the user search path (`~/.beads/formulas/`) under a
  throwaway name, pouring *that*, and removing it in a `finally` (refusing to
  run if the name is already taken, so it can never clobber a real formula).
  Confirmed both directions from this worktree: red before the fix, green
  after — the anti-tautology lock V2 will demand.
- **Concurrency: not applicable.** The contract involves no shared mutable
  state and no parallelism — `formula-check` is a pure read of files on disk,
  and `--pour-check` runs `bd … --dry-run`, which writes nothing. No
  concurrency stress criterion is warranted, and none is claimed.
- **Trust boundary.** The `request` value is external input (Nick's words, or
  a GitHub issue reference that may be attacker-influenceable on a public
  repo) that is substituted into a bead description which agents later read
  as instructions. It is validated as *inert* by C4: it must survive
  verbatim, with no second interpolation pass and no shell evaluation.
  Semantic trust in the request's *content* is the human gate's job
  (GATE 1), not the validator's, and is deliberately out of scope here.

## User impact

**Breaking:** no public API is removed, so `breaking: false`. But note one
behavior change for the maintainer: pouring the `change` formula now
**requires** `--var request=…` and fails without it (F1). That is the point
of the fix.

**Testbed effect:** none — no Haskell code, no build, no runtime surface.

**Migration note:** the dispatcher's pour call must supply
`--var request=<the request>`. Molecules already poured (`nh-mol-i4a`,
`nh-mol-84g`, `nh-mol-rte`) keep their empty request text; their request has
to be recovered from the branch/worktree name, as this molecule's was.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). ADR-0075 already decides *that* the formula
carries a request; this change only makes the formula honor its own
declaration.
