# Intake experiments

The paired classifier experiment compares **Jev against Codex Luna Max** directly.
The older route-judging experiment is documented below; it measures a different
task and is not the control arm of this comparison.

## Paired task classification

```sh
./dev pipeline-experiment --self-test
./dev pipeline-experiment --split dev --repeat 1 --output /tmp/intake-dev-v1
./dev pipeline-experiment --split evaluation --repeat 2 --output /tmp/intake-eval-v1
```

Both models receive the same request, task definitions, policy and questions.
Jev `jev-1.13.0` answers two Choices, six independent yes/no Noul questions and
one Score in a single batch. `codex exec` uses **`gpt-5.6-luna` with
`model_reasoning_effort="max"`**, producing corresponding schema-constrained JSON.
Code compares both outputs against frozen reference labels. Neither model judges
the other. No pipeline state or implementation workflow is executed.

The first experimental unit is the proposed **task-classification step**. It
splits the 22 requested task names into 12 work types, five special execution
modes and five lifecycle actions, with explicit default, mixed and unknown
outcomes. Actions are a set; their execution order is outside this experiment.
It does not measure localization, the old two-route intake, or whole-task success.
Later experiments can add those stages with their own inputs and proving labels.

The versioned [contract](intake_contract.json) defines categories, tie-breaks,
clarification policy and an anchored 0–3 readiness rubric. The
[corpus](intake_cases.json) has six development and 30 evaluation requests,
including every proposed type, mixed tasks, explicit exclusions, missing context
and a quoted instruction. References and rationales are **author-draft synthetic
labels**, not independently human-reviewed ground truth. Requests are disjoint
across splits; their concepts overlap. Readiness is a provisional ordinal target,
not a measured probability of success or permission to act.

Protocol fixed before evaluation:

- Randomize case order with seed 1729; alternate model order within pairs; one
  outstanding inference at a time. Repetitions measure consistency and do not
  become additional independent tasks.
- Pin model identifiers and Luna effort. Each Codex call starts a fresh ephemeral
  process in an empty directory, ignores user configuration, disables repository
  instructions, uses a read-only sandbox and disables tools and discovered skills.
  File and directory skill-disable identities are both included because this CLI
  build still injected skills with only the discovery flag or folder identities.
  Retain raw warnings and reject a reported shortened skill catalog or tool use.
- References, rationales, case IDs and split labels are never included in model
  inputs. The model's temporary directory contains only the output schema; the
  input arrives over stdin. Auth stays in the existing Codex login; Jev reads
  `TYPESAFE_API_KEY` or `~/.jevenv` without logging it.
- Use Jev Choice's selected option and Noul ≥0.5 for raw predictions. Save all
  native probabilities/confidences; do not fit an abstention gate on evaluation
  outcomes or ask Luna to invent comparable confidence values.
- Include every attempted call: failures count against exact and per-field
  accuracy. Application retries are zero, with a 45-second Jev request timeout
  and a 180-second Codex process timeout. Provider/CLI transport retries are not
  controlled. A timeout kills the Codex process group.
- Preserve the exact contract, labels, source snapshots/hashes, CLI version,
  command, schema, inputs, raw responses/events, usage and execution order in a
  new output directory. Never overwrite a previous run.

The report measures exact profile accuracy (work + mode + action set +
clarification), each dimension separately, action micro/macro F1, unrequested
actions, missed/unneeded clarifications, numeric readiness MAE, errors, median
and descriptive p95 latency, token usage with observation counts, and repeated
categorical agreement. It includes paired wins/losses and an exploratory
case-cluster bootstrap interval for the accuracy difference. Score errors use
valid responses only, with their denominator shown; no response is a success.

Timing measures the deployment paths: API round trip for Jev and complete
`codex exec` process time for Luna, including startup. Provider system context
and tokenizers differ. Caching, service load and network conditions are not
controlled, and there is no inference-only timing claim. Reported Luna model and
effort are requested settings; the CLI event stream does not attest the resolved
server model. Token counts are resource observations, not a dollar estimate or
directly comparable units of computation.

## Regression workflow

1. Add a failing development case for the behavior being changed and freeze its
   intended label before changing the classifier. Keep evaluation labels stable.
2. Run the development split, change questions or composition, and rerun it.
3. Freeze the candidate. Run the evaluation split once using the same protocol.
   Once inspected, that set is a known regression set; reserve a new reviewed set
   for any later generalization claim.
4. Compare saved runs offline (no API calls):

```sh
./dev pipeline-experiment --compare /tmp/intake-eval-v1 /tmp/intake-eval-v2
```

The provisional gate exits nonzero for lower exact-profile accuracy, more service
errors, more unrequested actions, more missed clarifications, or median latency
over 1.25× baseline, for either provider. Override the timing budget with
`--max-latency-ratio` **before** a candidate run when appropriate. These are
development budgets, not statistical promotion criteria; p95 remains descriptive
on this small set. The comparator rejects incomplete or duplicated pairs,
changed saved grades/inputs, and incompatible contracts, labels, repeats, split,
seed, models, CLI or skill inventory. Prompt/adaptor code can change while the
task contract stays fixed. A changed taxonomy needs a new baseline on that task.

Offline protocol and subprocess tests run through `./dev doctor`. Live model
calls are opt-in and are never part of doctor/CI by default. Before adopting
automatic routing, review the reference labels and collect real intake requests.

## Earlier route-judging experiment

This is a local experiment, not the full evaluation suite proposed in change
859. It assesses recorded routing decisions; it does not run or modify the
NeoHaskell change pipeline. Use the TypeSafe skill when changing the questions.

Run offline regression checks:

```sh
./dev pipeline-judge --self-test
```

Evaluate recorded routes with a credential in `TYPESAFE_API_KEY` or `~/.jevenv`:

```sh
./dev pipeline-judge --input /path/to/samples.jsonl \
  --output /tmp/jev-new-run --repeat 2 --controls
```

Each input line contains `id`, `request`, `expected`, and `response.route`.
`expected` is used only by the local grader and control-case generator. It is
never sent to Jev. `--controls` adds a reference-correct proposal and a different
reference route for each request. These mutations are development probes, not
independent held-out requests; some alternate routes may merit human review.

To compare with the original three questions, supply `--legacy-metadata` pointing
to the original experiment's metadata JSON (which contains `questions`). Both
versions run on the same cases with alternating execution order and fixed
provisional thresholds. The retained questions currently match the legacy
questions: three attempted rewrites failed to improve the comparison and were
not promoted. Future changes can replace the versioned `questions.json` and
compare against a saved earlier metadata file. Raw model classifications and
automatic accept/reject/review outcomes are separate metrics.

The judge asks Choice for support, Noul for the probability that the route is
supported, and Score for evidence fit. These semantic dimensions overlap. Their
probabilities are never multiplied or treated as independent corroboration.
Following the TypeSafe verification pattern, code performs identifier checks and
response validation, then routes uncertain or conflicting answers to review.
Choice confidence, probability margin, Noul and evidence thresholds are explicit
in `POLICY`. The model's raw answers remain available for later policy experiments.

Every sample also has an independent `reference_verdict`: a Jev acceptance cannot
override a known reference mismatch. Choice support probability is measured
against route labels with a Brier score. Numeric evidence-fit and clarification
accuracy remain unmeasured without reviewed labels for those dimensions.

The model is pinned, responses are validated, and unknown route identifiers are
rejected before inference. Thresholds remain provisional development policy.
Model confidence is not ground truth. Exact route labels remain the correctness
reference; a Jev acceptance cannot override a mismatch.

Output directories must be new. They contain metadata, exact requests, raw
responses, every attempt and summaries by case group. Timing includes the API
round trip, not the intake workload. Service/validation failures remain in the
attempt denominator, with zero retries and a 45-second request timeout. Raw API
usage is preserved; reported token totals cover only samples with usage, whose
count is also reported. No monetary cost is inferred. Artifacts omit credentials.

The original routing outputs came from an exploratory Codex run with injected
global skill descriptions. They are usable for judging experiments, but do not
establish a strictly isolated routing baseline. Repetitions and generated controls
share the same ten known requests; results cannot establish generalization or
probability calibration. No reviewed numeric score labels exist yet.

Sources: [TypeSafe skill](../../.agents/skills/typesafe-ai/SKILL.md),
[state](https://docs.typesafe.ai/concepts/state),
[verification cookbook](https://docs.typesafe.ai/cookbooks/citation_check),
[API](https://docs.typesafe.ai/api). Offline checks run through `./dev doctor`.
