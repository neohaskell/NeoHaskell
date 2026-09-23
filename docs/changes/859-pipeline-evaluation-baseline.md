# Change 859: Measure pipeline behavior before optimizing it

Give contributors a repeatable evaluation suite for improving the NeoHaskell change process with tests first. Begin with today's intake and localization behavior, preserve an honest baseline, and compare later implementations on correctness, uncertainty handling, reproducibility, speed, and resource use. Use Jev for semantic evaluation, with deterministic reference checks to measure the evaluator itself. Keep the runner extensible to later pipeline stages without changing their production behavior in this change.

```yaml spec
issue: adhoc:pipeline-evaluation-baseline
kind: feature
touches: [dev-pipeline, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No Haskell API change. This adds the internal `./dev pipeline-eval` command and versioned evaluation data. It does not replace the pipeline state machine, existing benchmark, routing assets, or production model decisions.

```diff signatures
```

### Evaluation surface

- `./dev pipeline-eval validate --phase intake` validates versioned cases, decision schemas, scoring policies, and source references without model calls.
- `./dev pipeline-eval run --phase intake --adapter <name> --judge jev --output <directory> --repeat <n>` records every attempted sample and produces JSON plus a readable report. Initial adapters exercise current deterministic intake behavior in isolated fixtures and the existing routing-smoke protocol through fresh Codex CLI contexts. Jev evaluates the resulting semantic decisions through the TypeSafe API; its overhead is recorded separately from the process under test. An explicit command adapter permits later pipeline implementations to consume the same sanitized input contract.
- `./dev pipeline-eval compare <baseline-directory> <candidate-directory>` produces paired per-case differences and an exit status based on explicit policy. It refuses incomplete or incompatible evidence instead of silently comparing different case sets or graders.
- `./dev pipeline-eval --self-test` is the fast offline runner/grader suite wired into `./dev doctor`. `--self-test-subprocess` additionally proves the real subprocess boundary using controlled fixture processes. Hosted CI does not call a paid model or enforce noisy wall-clock improvements.

Phase identifiers are data, initially `intake` (covering intake and localization substeps). Future stages plug in their cases, adapters, and graders. The runner supports typed yes/no, choice (including explicit abstention and set-valued answers where declared), and rubric-score decisions. A case declares the expected decision or accepted alternatives, scoring rule, observable constraints, and whether it characterizes current behavior or requires an improvement. Schema-invalid, missing, extra, and non-finite values fail explicitly; missing observations are never reported as measured zero.

### Jev evaluation contract

Use TypeSafe Jev as the semantic judge. The process under test remains separately identified: first measure the existing intake implementation and routing protocol, then evaluate future replacements on the same cases. Jev judgments supplement deterministic reference checks; they cannot override a wrong gold answer or a violated invariant. The runner must never silently substitute a chat model for Jev. Missing credentials fail a requested live evaluation clearly; fixture-only runs carry a distinct label.

- Send narrow questions with explicit evidence: Noul for whether a selected route is supported or clarification is required; Choice for a declared outcome category; Score for an ordered evidence-fit rubric (0: contradicted, 1: weak, 2: partial, 3: sufficient). Each judgment names its observable inputs and applicable cases. Independent questions can share a request; dependent judgments require a subsequent call or code composition.
- Call `POST https://api.typesafe.ai/v1/systemone` using `TYPESAFE_API_KEY` from the environment. Use the Python standard library HTTP client, avoiding a new package dependency. Initially pin `jev-1.13.0`, recording both requested and returned model IDs; a version mismatch invalidates a comparable run. Credentials and authorization headers never enter artifacts.
- Preserve native Noul probabilities and Choice/Score distributions, fractional scores and confidence. Noul has no separate confidence field. Validate keys, types, finite ranges, probability normalization, score levels and model identity. Thresholds and rubrics are versioned configuration, never tuned silently against evaluation outcomes.
- Keep reference labels out of both the process under test and the semantic judge's input. The judge sees the request, relevant source evidence, and observed decision, but not adapter identity or a preferred outcome. The offline grader compares both the observed decision and Jev judgments with reviewed labels. Score rubrics are public instructions; per-case gold scores remain private to the grader.
- Report judge/reference agreement and false acceptance of known-wrong outputs. Where reviewed labels exist, compute Noul/Choice Brier scores and Score absolute error. Retain the returned confidence separately; do not interpret it as measured correctness. Threshold selection uses development cases, with evaluation results on separately identified cases; the ten known smoke examples do not establish calibration or generalization.
- Record workload latency, Jev judge latency, and total harness latency separately, including retry attempts and backoff. Bounded timeouts and retries retain authentication errors, rate limits, overloads and malformed responses as failures. Offline tests exercise transport responses without credentials; live-provider measurements require an authenticated run and cannot be replaced by fixture results.

API shapes and model pin are based on the official [API reference](https://docs.typesafe.ai/api), [models](https://docs.typesafe.ai/models), and [confidence semantics](https://docs.typesafe.ai/confidence), checked 2026-09-23.

### Initial coverage and baseline

1. Preserve the existing ten routing-smoke cases and expected answers as an unchanged compatibility set, with source digests. They are known development examples, not an unseen holdout.
2. Characterize the actual intake state implementation: issue versus ad hoc requests, assignment failure, base/ancestry/branch/repository mismatch, change-ID collision, active-run handling, and plan-write lifecycle. Controlled dependencies isolate these checks from real issue assignments, pushes, or live pipeline state. Label fixture timing as local tool timing, not real GitHub/network latency.
3. Add a separately identified challenge set for ambiguous, unsupported, mixed, paraphrased, and misleading requests, including preservation of explicit user constraints. Cases support reviewed alternative answers and unresolved labels; unlabeled cases cannot silently enter a correctness denominator.
4. Record baseline measurements before optimizing production routing; running the Jev judge does not change the process under test. Publish a coverage matrix: actually exercised behavior, fixture-only behavior, and unmeasured behavior. A routing-only result must not be described as end-to-end intake performance.
5. Record the model/CLI/configuration, repetitions and concurrency, source/corpus/prompt/grader digests, repository SHA plus relevant working-tree content hashes, environment/cache declarations, raw model output and execution events. Per-case expected answers and acceptance policies stay in the grader process, outside model inputs and accessible evaluation workspaces; task instructions and semantic rubrics are explicitly versioned inputs. Existing skill examples are identified as known examples, not claimed to be hidden from the model.

### Measurements and comparison policy

Report separate dimensions rather than an opaque overall score:

- Decision correctness and route-set precision/recall where applicable; incorrect accepted routes and abstention/coverage reported together so refusing everything cannot win.
- Required clarification recall and unnecessary clarification rate where reference labels exist; unsupported-request handling and explicit-constraint preservation.
- Output-schema validity, evidence/reference validity, fabricated identifiers, tool-policy violations, crashes, and timeouts; failures remain in attempted-sample denominators and raw records.
- Repeat agreement and per-case variation, with sample counts and uncertainty intervals for rates. Small-sample percentiles are explicitly labeled descriptive.
- Measured elapsed time (median and nearest-rank p95), separately for successful and all completed/terminated attempts, plus timeout count. Human waits are separate; uncontrolled provider caches are disclosed.
- Observed input, cached-input and output tokens, model calls, tool calls, retries, and interaction turns when the adapter can measure them. Unsupported fields and actual monetary cost are null with a reason; dollar estimates require an explicit dated price schedule and remain estimates.

Correctness and critical invariants dominate speed. Offline acceptance assertions can fail the command deterministically. Baseline comparison rejects new critical failures and reports paired correctness changes; timing/resource thresholds are explicit local policies rather than silently inferred from a noisy first sample. Comparisons bind the same case IDs, inputs, expectations and scoring version, retain missing/error attempts, disclose intentional adapter/model changes, and refuse changed gold labels disguised as improvements. No automatic baseline overwrite or automatic expectation weakening is allowed.

The TDD loop is: add a reviewed failing case or acceptance assertion, confirm the current implementation fails for the expected reason, change the implementation, rerun that case, then run the full deterministic set and a repeated model comparison. Desired future behavior remains distinguishable from characterization of a known current limitation. Controlled wrong-answer and protocol mutations must demonstrate that the evaluator can detect regressions.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Versioned phase/case contracts validate yes/no, choice/set, score, abstention, labels and provenance, rejecting duplicate IDs, malformed inputs and unsupported grader configuration | `script:scripts/pipeline-eval#--self-test --case contracts` | unit | none |
| C2 | Deterministic graders discriminate known-correct and mutated-wrong answers, reject invented identifiers and non-finite scores, and separate characterization from required improvements without changing existing expectations | `script:scripts/pipeline-eval#--self-test --case grading` | unit | none |
| C3 | Metrics retain every attempt, distinguish null from zero, report correctness with coverage, distinguish clarification errors, and compute latency/repeat statistics with explicit denominators and small-sample limitations | `script:scripts/pipeline-eval#--self-test --case metrics` | unit | none |
| C4 | Paired comparisons reject missing, duplicate, tampered, differently labeled or incompatible samples and fail critical regressions even when the candidate is faster | `script:scripts/pipeline-eval#--self-test --case comparison` | unit | none |
| C5 | Model input construction excludes expected answers and grader policies; source/output digests, relevant dirty-file fingerprints and adapter/configuration provenance prevent stale or mislabeled baseline evidence | `script:scripts/pipeline-eval#--self-test --case provenance` | unit | none |
| C6 | Initial intake cases exercise the unchanged pipeline implementation with isolated fake external dependencies and preserve all ten legacy routing expectations; unavailable observations remain explicitly unmeasured | `script:scripts/pipeline-eval#--self-test --case current-intake` | unit | none |
| C7 | Phase registration and a command-adapter contract admit another synthetic phase without special-casing intake; offline checks require neither credentials nor model inference | `script:scripts/pipeline-eval#--self-test --case extensibility` | unit | none |
| C8 | Real fixture subprocesses prove successful execution, wrong-answer grading, malformed output, timeout cleanup, nonzero exit, missing-usage handling, identity mismatch and workspace-mutation detection | `script:scripts/pipeline_eval/test_subprocess.py#--self-test` | integration | subprocess:real |
| C9 | Jev request/response fixtures prove all three native primitives, independent batching, pinned identity, probability validation, rubric/label separation, credential redaction, bounded retry accounting and distinct workload/judge timing without substituting another model | `script:scripts/pipeline-eval#--self-test --case jev` | unit | none |
| C10 | Known-right and known-wrong labeled judgments prove judge agreement, false-acceptance, Brier and score-error calculations; confidence never overrides gold correctness and tuning/evaluation case identities remain distinct | `script:scripts/pipeline-eval#--self-test --case judge-quality` | unit | none |

## User impact

Contributors can measure today's process, add a failing evaluation before an optimization, and see whether the candidate is more correct, faster, or cheaper on the same cases. Application behavior is unchanged. Python's standard library is sufficient for offline evaluation; live routing measurements additionally use an already installed, authenticated Codex CLI and record the explicitly resolved model configuration. Live semantic evaluation additionally requires `TYPESAFE_API_KEY` for Jev. Raw runs remain local under a gitignored output directory; reviewed aggregate baseline evidence may be committed without transcripts or credentials.

Initial model measurements are local evidence, not proof of unseen-request generalization or full intake correctness. Ground-truth review, broader cases and later-stage adapters extend the suite over time. Existing `pipeline-benchmark` and telemetry protocols retain their contracts.

## ADR

Not required — no breaking API, dependency, capability, or extension-point trigger. This extends the existing pipeline evaluation and telemetry approach with a separate versioned evaluation protocol.
