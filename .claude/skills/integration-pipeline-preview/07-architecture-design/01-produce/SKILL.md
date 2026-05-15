---
name: 01-produce
description: Produces the implementation-ready architecture document for the integration.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Produce architecture doc

Reads the design draft + DevEx review and writes `.integration-pipeline/integration-architecture.md` with every design decision concretely resolved.

## Inputs

- `.integration-pipeline/integration-design.md` — the design draft from phase 3.
- `.integration-pipeline/devex-review.md` — the DevEx review from phase 6.
- `.integration-pipeline/classification.json` — provides the complexity `tier` and `rationale`.
- `../../references/architecture-rubric.md` — the rubric the doc will be judged against; the producer must satisfy it.
- `../../references/nhcore-context.md` — framework conventions the doc references.

## Plan

1. Load all five inputs → verify: every file exists.
2. Compute the target path `.integration-pipeline/integration-architecture.md` → verify: parent directory exists. Overwrite behaviour is idempotent — if the file already exists with byte-identical content, skip the write and continue; if the file exists with different content, refuse so a re-run after rubric failure does not silently clobber an in-flight design.
3. For each design decision, resolve module placement, full type signatures, exact imports, exact nhcore utilities, error ADTs, persistence statements, concurrency notes, and visibility decisions → verify: no `TBD`, `?`, `TODO`, or placeholder remains.
4. Write the doc with the sections listed in the Output contract → verify: every section is non-empty.

Assumptions:
- The architecture doc must leave no design decision open.
- All type signatures use the NeoHaskell custom Prelude (`Text`, `Array`, `Result`, `Task`).
- Every existing utility reused must be named with its exact module + symbol per `../../references/nhcore-context.md`.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the inputs. Refuse on missing files.
2. Enumerate the design draft's decisions. For each, write the corresponding section:
   - `## Module map` — every new file path under `integrations/Integration/<module_name>/`, with `integrations/Integration/<module_name>.hs` as the top-level entry. Never route integration code under `core/`. **If the design draft chose to wrap `Integration.Http` (the default for HTTP request/response integrations — see `../../references/nhcore-context.md` "Framework-provided defaults"), the module map MUST mirror the canonical four-file layout used by `Integration.Brevo`, `Integration.OpenRouter`, `Integration.Oura`: a re-export shell at `<module_name>.hs`, a Jess-facing `<module_name>/Request.hs`, a `<module_name>/Response.hs`, and a `<module_name>/Internal.hs` exposing `toHttpRequest :: ... -> Http.Request command` and the `ToAction (Request command)` instance.** Departures from this layout are allowed only when the design draft's `## Decision drivers` names a concrete blocker (streaming response, persistent connection, scheduled poller, non-HTTP wire); if no such blocker is named, refuse and ask the design-draft phase to be re-run with the wrap-Http option.
   - `## Public types` — full signatures, doc-by-example, visibility (`exported` / `internal`).
   - `## Public functions` — full signatures, doc-by-example, every type parameter named (no single letters).
   - `## Internal helpers` — full signatures plus the call sites that justify them (no single-use helpers — single-use abstractions are rejected).
   - `## Imports` — qualified, with the exact symbol set imported per upstream module.
   - `## nhcore utilities used` — exact module + symbol per row.
   - `## Errors` — every error ADT constructor, the condition that produces it, and the user-facing message.
   - `## Concurrency & persistence` — Hasql statements, event-stream IDs, `RequestContext` threading, or an explicit "pure type, no persistence" statement.
3. Cross-check every section against the rubric's eight checks. If any section is missing a required element, fix it before writing.
4. Write the doc atomically.
5. Print the path to the written file. Do not call `pipeline.py complete` — the review-quality step owns that.

## Output

`.integration-pipeline/integration-architecture.md` exists with the eight sections above. Stdout is a single line: `wrote .integration-pipeline/integration-architecture.md`.

## Refusals

- Any prerequisite input missing → refuse with the missing path.
- The design draft contains an unresolved decision the producer cannot fill in from the design draft + DevEx review → refuse: "design decision <X> requires maintainer input"; do not invent the decision.
- Any section would contain `TBD` / `TODO` / `?` at write time → refuse with the list of unresolved decisions.
