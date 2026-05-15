# Architecture design quality rubric

The acceptance bar for a phase-7 architecture document. Eight yes/no questions, grounded in merged ADRs (0048, 0049, 0052). The review-quality agent applies every question to the architecture doc produced by phase 7.1.

The architecture doc passes when every question lands `yes`. The standard is "implementation-ready" — phase 9 (test writing) and phase 10 (implementation) must be able to proceed from this document with **no further design decisions**.

## Contents

- [How to use this rubric](#how-to-use-this-rubric)
- [The eight checks](#the-eight-checks)
- [Carrier rules](#carrier-rules)
- [Exemplar references](#exemplar-references)

## How to use this rubric

The review-quality agent reads the produced architecture doc at `.integration-pipeline/integration-architecture.md` plus the source design draft at `.integration-pipeline/integration-design.md`. For each question:

- `verdict`: `pass`, `fail`, or `n/a` (with a one-line justification for `n/a`).
- `evidence`: file path and line range supporting the verdict.

If every question is `pass` or `n/a`, the producer step is approved and the agent runs `pipeline.py complete 7`. Any `fail` causes the agent to surface the failures verbatim and stop the pipeline.

## The eight checks

1. **Module paths are concrete.** Every new module declared in the doc has an absolute file path matching the NeoHaskell layout convention (`core/<area>/<Module>.hs`, `core/service/Service/<Subsystem>/Core.hs`). No `<TBD>`, `<choose later>`, or directory placeholders (ADR-0049 pattern: `core/service/Service/OutboundIntegration/Core.hs` + `TH.hs`).
2. **Type signatures are complete.** Every public function declared in the doc has its full type signature spelled out, using `Text` / `Array` / `Result` / `Task` (no `String`, no `IO`, no `Either`). No `?`, `TODO`, `TBD`, or `Generic`-stand-in placeholders.
3. **Every type is constructed and consumed somewhere.** For each new type, the doc names at least one construction site (smart constructor, generator) and at least one consumption site (function that takes it, decider, projection). Dead types fail this check.
4. **Integration points name exact upstream names.** Any reuse of an existing utility names the exact symbol and module (e.g. "uses `EventVariantOf.toVariant` from `Service.Entity.Core`", not "the event variant helper"). ADR-0049 shows the pattern; ADR-0052 shows the cross-module reuse case.
5. **Imports are listed qualified.** The doc names every nhcore module the implementation will import, with the expected alias and the set of symbols pulled in. No `import Whatever`-style ambiguity.
6. **Errors are enumerated.** Every error case the feature can produce has a named ADT constructor in the doc, with the conditions that produce it (per ADR-0048 `ContentHashError`). Generic `Text` error returns fail this check unless the doc explicitly justifies them.
7. **Concurrency & persistence decisions are stated.** If the feature touches event-store / command / query / persistence layers, the doc names the specific Hasql statements (or "no persistence — pure type"), the specific event-stream IDs the feature emits or consumes, and the specific RequestContext threading the commands require. If it does not touch these layers, the doc says so explicitly.
8. **Visibility decisions are explicit.** For each new type and function, the doc states whether it is exported from the module's public interface or internal-only. Smart-constructor types name whether the constructor itself is exported (typically `no`, with the smart constructor as the only public construction path — Redacted, Decimal, ContentHash).

## Carrier rules

- **Think before coding.** Every design decision in the doc cites the ADR section that motivated it. A decision without an ADR cite is a `fail` — the decision is either speculative or undocumented.
- **Simplicity first.** No abstraction, type class, or helper module is introduced unless the doc names two or more concrete call sites that need it. Single-use abstractions fail this check.
- **Surgical changes.** The doc lists every existing file it modifies and every existing module it touches. Lists that grow beyond the ADR's stated scope fail this check.
- **Goal-driven.** Each section ends with the observable success criterion that the implementer can check against (e.g. "the `Decimal.divide` test for zero divisor returns `Nothing` and emits no log").

A doc that passes the eight checks but violates a carrier rule is still a `fail`.

## Exemplar references

- `docs/decisions/0048-file-upload-content-deduplication.md:55-73, 62-73` — type-table + smart-constructor pattern.
- `docs/decisions/0049-outboundintegration-typeclass-dispatch-generation.md:75-87, 116-127, 133-148` — full module path + class decl + integration sequencing.
- `docs/decisions/0052-mcp-stdio-transport.md:73-87` — `Service/<Subsystem>/` placement with sub-modules.
