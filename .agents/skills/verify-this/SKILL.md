---
name: verify-this
description: "Verify a claim with fresh local evidence: restate it falsifiably, capture baseline and treatment, compare artifacts, and return VERIFIED, NOT VERIFIED, or INCONCLUSIVE."
---

# Verify This

Verification is not a recap. It proves or disproves a specific claim with repeatable evidence.

## When To Use

- The user asks "verify this", "prove it works", "did this fix it", or "show me the evidence".
- A bug fix needs a before/after repro.
- A UI, CLI, API, performance, or memory claim needs measurement.
- A test passes but the user-visible behavior still needs confirmation.

Do not use this for vague claims like "the code is cleaner". Ask for a measurable claim first.

## Workflow

1. Restate the claim in falsifiable form: condition and exact observable acceptance predicate. For numeric claims, include the metric, unit, and threshold.
2. Pick the smallest local surface that can disprove it.
3. Capture a baseline from the old state: merge base, parent commit, failing branch, or current broken repro.
4. Capture treatment from the changed state with the same command, data, warmup, and environment.
5. Compare raw artifacts: numbers, screenshots, terminal transcripts, HTTP responses, profiles, heap snapshots, or test output.
6. Return exactly one verdict: `VERIFIED`, `NOT VERIFIED`, or `INCONCLUSIVE`.

## Local Surfaces

- Code behavior: focused unit/integration tests, meaningful property checks, or a minimal repro script with independently specified expected outcomes. Demand the part of a pure result the contract requires, execute deferred Task effects, and consume relevant stream output.
- Compiler and generated APIs: fresh positive/negative consumer compilation for the claimed type or diagnostic contract. For Template Haskell or Generic-derived APIs, process the actual declarations and exercise the generated interfaces relevant to the claim. Compilation proves compiler claims; runtime behavior needs runtime observations.
- Framework flows: drive the relevant command, decider, storage, or downstream integration/projection path and observe its claimed completion boundary.
- CLI/TUI behavior: [control-cli](../control-cli/SKILL.md), terminal transcript, or demo recording.
- UI behavior: [control-ui](../control-ui/SKILL.md), screenshots, accessibility snapshots, or browser traces.
- API behavior: local HTTP/RPC request and response diff.
- Performance: same-machine baseline/treatment timings or CPU profiles.
- Memory: runtime-appropriate allocation, residency, or heap evidence before and after the suspected operation.

Follow [Prove It Works](../principle-prove-it-works/SKILL.md) for proof selection and [Test Behavior, Not Implementation](../principle-test-behavior-not-implementation/SKILL.md) for independent examples and meaningful properties; avoid tautological tests. Apply the [benchmark checklist](../benchmark-checklist/SKILL.md) to performance claims, including full framework flows and consumer compilation costs when relevant.

Choose an existing executable owner before building a new driver. Use [Regression evidence](../regression-evidence/SKILL.md) for runtime assertions or positive and negative compiler fixtures, [NeoHaskell tests](../neohaskell-test/SKILL.md) for maintained suites and doctests, and [Event-store regression](../event-store-regression/SKILL.md) for controlled-fault sensitivity. Use [Consumer verification](../neohaskell-consumer-verification/SKILL.md) for generated projects and the embedded IDE bundle. Use [Framework benchmark](../framework-benchmark/SKILL.md) for matched measurements.

## Artifact Layout

When safe to write artifacts, use paths relative to the repository root:

```text
.agents-state/verify-this/<claim-slug>/
├── claim.md
├── timeline.md
├── baseline/
├── treatment/
├── diff/
└── verdict.md
```

If artifacts may contain sensitive code, prompts, screenshots, HTTP bodies, or heap data, keep only the minimal inline evidence unless the user agrees to disk storage.

## Verdict Rules

- `VERIFIED`: baseline and treatment satisfy the predicted relationship and acceptance predicate, including any claimed numeric threshold, with no obvious confound. Unchanged behavior can satisfy a preservation claim.
- `NOT VERIFIED`: the comparison falsifies the acceptance predicate; for example, behavior changes when preservation was claimed, stays unchanged when a change was claimed, moves the wrong way, or misses the threshold.
- `INCONCLUSIVE`: no valid baseline, noisy signal, failed measurement, or an environment difference invalidates the comparison.

## Output

Use this shape:

```text
VERIFIED | NOT VERIFIED | INCONCLUSIVE
Claim: <falsifiable claim>

Evidence:
<artifact/observation>: baseline=<...>, treatment=<...>, expected=<predicate or threshold>, result=<...>

Reasoning:
<one tight paragraph naming the evidence and any confounds>
```

For numeric claims, include the unit, delta, and threshold in the evidence.

Do not soften a negative result. A clear `NOT VERIFIED` is useful.
