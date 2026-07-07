> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 01-produce
description: Produces the test specification covering happy paths, edges, errors, and properties.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Produce test spec

Reads the architecture doc and writes `.integration-pipeline/integration-tests.md` with every test case the implementation must satisfy.

## Inputs

- `.integration-pipeline/integration-architecture.md` — produced by phase 7.
- `.integration-pipeline/classification.json` — provides the complexity `tier` and `rationale`.
- `../../references/test-spec-rubric.md` — the rubric the spec will be judged against; the producer must satisfy it.

## Plan

1. Load all three inputs → verify: every file exists.
2. Enumerate every public function and type from the architecture doc → verify: list non-empty.
3. For each public function, draft test cases in the five buckets named below; tally happy vs non-happy counts → verify: every function has ≥3 cases and the aggregate non-happy-to-happy ratio is ≥1:1.
4. Write the spec to `.integration-pipeline/integration-tests.md` → verify: file exists with the sections in the Output contract.

Assumptions:
- Minimum 3 cases per public function (1 happy + 2 non-happy).
- Target aggregate ratio of non-happy to happy ≥ 1:1 (3:1 is the stretch goal noted in the skill).
- Layout matches the existing exemplars (`describe "Module" > describe "Function" > it "specific case"`).

If any assumption fails, refuse — do not guess.

## Steps

1. Load the architecture doc and classification. Refuse on missing inputs.
2. Enumerate every public function, every public type, every error ADT constructor.
3. For each function, draft cases in five buckets:
   - **Happy paths** — typical inputs producing typical outputs.
   - **Edge cases** — zero, empty, unicode, maximum, max+1, negative where representable.
   - **Error conditions** — one case per error ADT constructor the function can produce.
   - **Serialization round-trips** — for every type the architecture doc marks as `ToJSON` / `FromJSON`.
   - **Property invariants** — for operations the architecture doc identifies as commutative, associative, idempotent, or round-tripping. If the function satisfies no law, the spec records `properties: none — <reason>`.
4. Tally aggregate happy vs non-happy. If non-happy / happy < 1, expand non-happy cases until the floor is met. If a function has fewer than 3 cases total, expand.
5. Write the spec with these sections:
   - `## Coverage summary` — table: function × case count × non-happy ratio.
   - `## Per-function cases` — one subsection per public function, organised as `describe`/`describe`/`it` lines with the expected outcome stated as a concrete value or predicate.
   - `## Round-trip suite` — one entry per serializable type.
   - `## Property suite` — one entry per law-satisfying operation; explicit `none` entries for operations that satisfy no law.
6. Print the path to the written file. Do not call `pipeline.py complete` — the review-quality step owns that.

## Output

`.integration-pipeline/integration-tests.md` exists with the four sections above. Stdout is a single line: `wrote .integration-pipeline/integration-tests.md`.

## Refusals

- Architecture doc missing → refuse: "no architecture doc; run phase 07 first".
- A public function has fewer than 3 cases at write time → refuse with the offending function name.
- Aggregate non-happy / happy ratio < 1 at write time → refuse with the current ratio.
