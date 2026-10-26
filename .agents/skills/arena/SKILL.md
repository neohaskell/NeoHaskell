---
name: arena
description: "Spawn N parallel candidates at the same task, pick a base, graft the strongest parts of the losers into it. Use for /arena, 'arena this', 'throw it in the arena', or when one attempt at a non-trivial artifact would lock in the wrong shape."
---

# Arena

Open a todolist with one entry per phase (Frame, Fan out, Cross-judge, Pick, Graft, Verify) before launching anything.

## Phase A: Frame

Every candidate gets the same prompt, so the prompt is the contract.

1. State the artifact each candidate is producing.
2. Derive the rubric, 3-6 concrete gradeable criteria for what success looks like on *this* task. It is the picker's tool in Phase D. Candidates only see the task.
3. Follow the delegation instructions in `AGENTS.md`. Pick the runners through the current environment's configured candidate roles and available delegation interface. Use independent perspectives for judgment-sensitive work; the same supported execution configuration may run several candidates when appropriate. Inherit supported runtime defaults when no override is configured; report unsupported overrides rather than inventing fallback identifiers. Select only reasoning and speed settings supported by the environment. Centralized role configuration is managed separately. Preserve any candidate count or structural-diversity requirement supplied by the caller. When neither the caller nor configuration specifies a count, start with two candidates. Add runners when multiple design directions warrant them.
4. Give each candidate an isolated working area and output location under the project's intermediate-work directory (`.agents-state/` in this repository). Use a git worktree when available and suitable; otherwise use a per-candidate subdirectory that keeps candidate writes independent, per **separate-before-serializing-shared-state**. Candidate writes stay within their assigned working areas; do not overwrite another candidate's artifacts.

## Phase B: Fan out

Launch the N candidates concurrently through the available delegation interface, within its capacity. Give each the task, the path to the shared grounding, its isolated working area and output path, and instructions to produce the artifact and a short rationale naming the alternatives it considered and what it rejected.

If a candidate produces no usable output, note the dropout. Proceed with the remaining candidates only when they satisfy the caller's minimum count and structural-diversity requirements. Otherwise obtain replacement candidates before synthesis, or report the unmet requirement if the environment cannot support it. Do not select a base when no usable candidate exists.

## Phase C: Cross-judge

After all Phase B candidates complete, spawn one read-only judge using the current environment's configured judging role. Prefer an independently configured judging role when supported and available; otherwise inherit supported defaults and keep the evaluation independent. Report unsupported overrides rather than inventing fallback identifiers. Use read-only enforcement when available; otherwise state the no-write constraint explicitly. The judge sees the rubric and the candidates by path label, scores each criterion, and recommends a base with rationale. It runs concurrently with your reading in Phase D. Don't spawn it while candidates are still writing.

## Phase D: Pick a base

Read every candidate end to end before picking. Score each against the rubric criterion by criterion, not on holistic feel, and compare with the cross-judge. Agreement on the base confirms the pick. Disagreement means one of you is biased or the rubric was ambiguous. Read both rationales before deciding.

Pick the base a future maintainer can extend most easily without breaking invariants. When two feel tied, prefer the cleaner boundary or smaller API, per the Minimal change.

## Phase E: Graft

Walk each losing candidate once more for what is worth porting into the base, usually one or two things per candidate, not most of it. Fold each graft in by hand, per the **redesign-from-first-principles** principle skill, so the result stays coherent under one mental model. Don't paste mechanically.

If the candidates converge on one shape, note the agreement. Ship the consensus shape without grafting when the caller's structural-diversity requirements are satisfied; otherwise obtain the required distinct alternatives before final synthesis. If they wildly diverge, Phase A was under-specified. Reframe and re-run rather than averaging the divergence.

## Phase F: Verify

Verify the synthesized artifact like any other output, per the **prove-it-works** principle skill. If verification finds a problem the arena missed, either Phase A was wrong (re-frame and re-run) or a candidate caught it and you missed the graft (go back to Phase E). Don't paper over it.

## Outputs

One synthesized artifact, with a short synthesis note alongside naming the base and why, the cross-judge's verdict, each graft with its source candidate, what was rejected and why, any convergence or dropouts, and the verification result.
