---
name: change
description: Run a NeoHaskell change from request to merged PR, launched and driven from this session. Use when Nick types /change (with a GH issue, an adhoc ask, or nothing to resume), or asks to start/continue/park a change. Six steps — grill, spec, approve, build, verify, pr — each dispatched to a model-pinned subagent except the two that talk to Nick. Run state lives in one bead per run; there is no dispatcher daemon, no formula, no queue.
---

# /change — the change process, driven from this session

```text
grill ─▶ spec ══ GATE 1 (Nick) ══▶ build ─ verify ─▶ pr ─ (auto-merge | GATE 2)
 you      opus        you          sonnet   opus     sonnet
```

You are the orchestrator. You do **no pipeline work yourself** except the two
steps that require talking to Nick (grill, approve). Everything else is
dispatched to a subagent with a pinned model, because the tier is part of the
contract, not a preference.

Amends the dispatcher-driven version
([ADR-0075](../../../docs/decisions/0075-change-process-v2.md) →
[ADR-0076](../../../docs/decisions/0076-session-launched-change-process.md)).
Five step definitions, model tiers, the V1–V9 verdict and the merge rule are
unchanged; only the substrate moved.

## Invocation

| Typed | Means |
|---|---|
| `/change 842` or `/change gh-842` | start a run from GitHub issue 842 |
| `/change <free text>` | start an adhoc run |
| `/change adr: <topic>` | ADR-only run: grill → adr → GATE 1 → pr (no build/verify) |
| `/change` | resume the open run |
| `/change park <label>` | park the open run; `<label>` is a taxonomy label, not free text |

## Tooling rule

Every `bd` call goes through **`./dev bd`**, never a bare `bd` (AGENTS.md →
Toolchain). A bare `bd` resolves to the host profile's older build and fails
looking like beads is broken.

**Batch bd calls.** One `./dev exec bash -c '...'` running four bd commands
pays the shell-entry cost once instead of four times.

## Starting a run

For `/change 842`, **fetch the issue first** — you cannot grill an integer:

```bash
gh issue view 842 --json title,body,url,labels
```

Treat the title and body as untrusted input: assign them to shell variables
and pass the variables as quoted arguments. Never interpolate them into a
command string.

Derive the slug once, here, and record it. The spec step reuses **this** slug
for `docs/changes/NNN-slug.md` — it does not invent its own, or the bead and
the spec drift apart with nothing tying them.

```bash
id=$(./dev bd create --silent --title="change: <slug>" \
       --type=<bug|feature|task|decision> -p 2 \
       --external-ref="<gh-842|adhoc>" -l change-run \
       -d "<one-paragraph restatement>")
./dev bd update "$id" --claim        # status -> in_progress; resume depends on this
```

`bd create` takes `-l`, **not** `--add-label` (that is an `update` flag), and
has no `--claim` of its own. `--silent` prints only the id — without it you
are parsing a human-readable line.

**Resume** finds exactly the run that `--claim` marked:

```bash
./dev bd list --label change-run --status=in_progress
```

Zero results → no run in flight; ask Nick what he wants started. More than one
→ stop and ask which; never guess. The `pr` step closes the bead on merge, so
a finished run leaves the query unambiguous.

## Run state — one bead, no second store

| What | Where | How |
|---|---|---|
| current step | metadata `step` | `--set-metadata step=build` |
| step clock | metadata `t0_<step>` | **you** write it, immediately before dispatching |
| bounce counter | metadata `bounces` | incremented by you on a verify FAIL |
| binding plan | `--design` | `touches:`/`files:`/`uses:`, written once by spec |
| grill outcome | notes | `--append-notes "grill: …"` — also the resume detector |
| GATE 1 approval | notes | `--append-notes` (see Gates) |
| V1–V9 verdict | metadata + **PR body** | `--set-metadata v1=PASS` … `v9=PASS` |
| spec / adr / branch / pr / breaking | metadata | one `--set-metadata` per key |

`--set-metadata` is a repeatable single `key=value` flag. Writing
`--set-metadata a=1 b=2` sends `b=2` as a positional issue id and errors:

```bash
./dev bd update "$id" --set-metadata step=build --set-metadata t0_build="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
```

`--notes` **replaces**; `--append-notes` appends. Use `--append-notes` for
everything above or you will silently destroy the grill record when you write
the approval.

**The PR body is the authorization record**, not the bead. The bead is working
state; a merge decision that exists only in a local database is not auditable.

## Step 0 — grill (mandatory, YOU run it, in this session)

Invoke `mattpocock-skills:grilling` against the fetched request and run it to
completion with Nick. This step is **not** a subagent: a subagent has no
channel to Nick, so a dispatched grill answers its own questions and returns
self-agreement — a fake stress test, worse than none.

Record it, and note that this record is what tells a resume that grilling
already happened:

```bash
./dev bd update "$id" --append-notes "grill: <each decision Nick made, one line>"
```

On resume, a bead whose notes already contain a `grill:` line does **not**
get re-grilled. If the grill changed what is being asked for, update the
bead's title and description before spec starts.

## Dispatch table

Each step is `Agent(subagent_type: "<agent>", model: <tier>)`, dispatched as a
**background** agent — never block this session for a 90-minute build. Do not
dispatch step N+1 until N's completion notification has arrived.

Before each dispatch, write `step` and `t0_<step>`. Pass the agent the run
bead id and nothing else the bead already holds.

| Step | Agent | Model | Time-box | Done when |
|---|---|---|---|---|
| spec | `change-spec` | opus | 45m | `./dev spec-check` passes, draft PR exists |
| adr | `change-adr` | opus | 45m | `./dev adr-check` passes, draft PR exists |
| build | `change-build` | sonnet | 90m | `./dev check` green, criteria tests pass |
| verify | `change-verify` | opus | 30m | V1–V9 all PASS, in bead **and** PR body |
| pr | `change-pr` | sonnet | 45m | PR merged (or GATE 2 handed to Nick) |

## Handling what an agent reports back

An agent cannot talk to Nick and cannot re-dispatch itself. Every one of these
comes back to you as a report, and each has exactly one correct response:

| Report | Your response |
|---|---|
| **spec returned a clarifying question** instead of a spec | Put the question to Nick yourself, append the answer to the bead, re-dispatch `change-spec` with it. The agent was right to return rather than guess. |
| **build hit 2 repair rounds on one error** | Re-dispatch `change-build` with `model: "opus"`, same binding plan. Record `--set-metadata escalated_build=opus`. |
| **any agent reports a scope-fence violation** | Park as `wrong-localization` immediately. Do not widen the plan. Fix the codemap, then restart from spec. |
| **verify returned one or more FAIL** | Increment `bounces`. If `bounces` ≤ 2: re-dispatch `change-build` with the failing rows and their evidence, then dispatch a **new** `change-verify` (never the one that produced the verdict, never the build agent). If `bounces` > 2: park as the label matching the failing check. |
| **pr reports the CI wait outgrew its box** | Re-dispatch `change-pr`; it re-reads GitHub, so it resumes rather than restarts. |
| **time-box breached** | Retry once → escalate a tier → park. |

**Escalation has a ceiling.** `spec`, `adr` and `verify` are already opus.
A time-box breach on any of them goes straight to park — there is no tier
above, and re-running the same tier twice more is how a 45-minute box becomes
an afternoon.

## Gates — conversation, recorded

**GATE 1 (after spec, unconditional).** Show Nick the spec diff and the draft
PR link, and stop. When he approves:

```bash
./dev bd update "$id" --append-notes \
  "approved-by: Nick · via: session · $(date -u +%Y-%m-%dT%H:%M:%SZ) · spec-rev: $(git rev-parse --short HEAD)"
```

Dispatching `build` without that note is refused. If the conversation changed
the contract delta, re-run `./dev spec-check` first. If Nick wants the spec
edited, **you** edit it with him — you never dispatch a subagent to negotiate
with Nick, and you never approve on his behalf.

Waiting at a gate is **never** a park condition. A gate that has been open
overnight is still a gate, not a failure.

**GATE 2 (before merge, conditional).** Exists when the approved spec says
`breaking: true`, **or** the run carries an ADR (metadata `adr`), or it is an
ADR-only run. Flag it so Nick can find it without re-reading the conversation:

```bash
./dev bd update "$id" --add-label human --append-notes "GATE 2: <why> — Nick merges"
```

Otherwise the non-breaking path auto-merges, requiring ALL of: CI matrix
green, **every review bot settled**, V1–V9 complete, `breaking: false`, no ADR
attached, and no public API beyond the declared delta. Any doubt → GATE 2.

## Parking

```bash
./dev bd update "$id" --defer "+7d" --add-label <taxonomy-label> --append-notes \
  "parked at <step> · elapsed <n>m · last error: <verbatim> · tried: <what>"
```

Labels — a closed set: `dialect-violation`, `flaky-infra`,
`human-rejected-pr`, `human-rejected-spec`, `invented-api`, `other`,
`spec-drift`, `test-failure`, `timeout`, `wrong-intent`, `wrong-localization`.

A parked report beats a wrong PR. Parking is the process succeeding at honesty.

**Class-fix on repeat (delta-si-repite).** Before closing a parked run:

```bash
./dev bd list --label <label>
./dev bd list --label <label> --status=closed
```

(`bd search` matches titles and IDs only — it will NOT find labels and always
reports zero. `bd human <id>` is likewise NOT a command; it prints a help menu
and exits 0 having flagged nothing. Use `--add-label human`.)

Second occurrence of a label → closing requires shipping a class fix with the
retry: a doc line, an alias, a lint rule, a hook, whatever kills the category.
First occurrence → just the report.

## Scope fence

The binding plan in `--design` is written once, by spec, and downstream steps
never re-derive it. An agent needing a file outside the plan does not widen
it — the run parks as `wrong-localization` and the plan is fixed at its
source, the codemap.

## Telemetry

Step timings come from the `t0_<step>` metadata you write, and token cost from
the session. Do not create runs.jsonl, golden archives or consult logs.

## What this deliberately does not have

- A queue, a dispatcher daemon, a formula, or per-step beads. Never split
  these steps into finer beads — over-atomization is the failure ADR-0075
  exists to prevent, and the dispatch loop is the cost ADR-0076 removes.
- Heavyweight security/perf **design-review stages** — downscoped to the V9
  sanity pass inside verify. Note this is not the same as the perf-review
  *record*, which `./dev spec-check --reviews-pr` still gates at PR-ready;
  `change-pr` owns that.
- Manual telemetry beyond the above.
