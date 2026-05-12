---
name: 01-produce
description: Produces the implementation-ready architecture document for the feature.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Produce architecture doc

Reads the ADR + DevEx review and writes `docs/architecture/<adr-number>-<slug>.md` with every design decision concretely resolved.

## Inputs

- `docs/decisions/<adr-number>-<slug>.md` — the ADR from phase 3.
- `.pipeline/devex-review.md` — the DevEx review from phase 6.
- `.pipeline/classification.json` — provides `adr-number` and the slug.
- `../../references/architecture-rubric.md` — the rubric the doc will be judged against; the producer must satisfy it.
- `../../references/nhcore-context.md` — framework conventions the doc references.

## Plan (Karpathy 1 + 4)

1. Load all five inputs → verify: every file exists.
2. Compute the target path `docs/architecture/<adr-number>-<slug>.md` → verify: parent directory exists; refuse to overwrite if the file already exists with different content.
3. For each ADR decision, resolve module placement, full type signatures, exact imports, exact nhcore utilities, error ADTs, persistence statements, concurrency notes, and visibility decisions → verify: no `TBD`, `?`, `TODO`, or placeholder remains.
4. Write the doc with the sections listed in the Output contract → verify: every section is non-empty.

Assumptions:
- The architecture doc must leave no design decision open.
- All type signatures use the NeoHaskell custom Prelude (`Text`, `Array`, `Result`, `Task`).
- Every existing utility reused must be named with its exact module + symbol per `../../references/nhcore-context.md`.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the inputs. Refuse on missing files.
2. Enumerate the ADR's decisions. For each, write the corresponding section:
   - `## Module map` — every new file path; integrate with existing layout (`core/<area>/<Module>.hs`).
   - `## Public types` — full signatures, doc-by-example, visibility (`exported` / `internal`).
   - `## Public functions` — full signatures, doc-by-example, every type parameter named (no single letters).
   - `## Internal helpers` — full signatures plus the call sites that justify them (no single-use helpers — see Karpathy "simplicity first").
   - `## Imports` — qualified, with the exact symbol set imported per upstream module.
   - `## nhcore utilities used` — exact module + symbol per row.
   - `## Errors` — every error ADT constructor, the condition that produces it, and the user-facing message.
   - `## Concurrency & persistence` — Hasql statements, event-stream IDs, `RequestContext` threading, or an explicit "pure type, no persistence" statement.
3. Cross-check every section against the rubric's eight checks. If any section is missing a required element, fix it before writing.
4. Write the doc atomically.
5. Print the path to the written file. Do not call `pipeline.py complete` — the review-quality step owns that.

## Output

`docs/architecture/<adr-number>-<slug>.md` exists with the eight sections above. Stdout is a single line: `wrote docs/architecture/<adr-number>-<slug>.md`.

## Refusals

- Any prerequisite input missing → refuse with the missing path.
- The ADR contains an unresolved decision the producer cannot fill in from the ADR + DevEx review → refuse: "ADR decision <X> requires maintainer input"; do not invent the decision.
- Any section would contain `TBD` / `TODO` / `?` at write time → refuse with the list of unresolved decisions.
