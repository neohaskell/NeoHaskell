# Human documentation review

## Scope and method

The approved method lives in [DOCUMENTATION_PLAN.md](DOCUMENTATION_PLAN.md).
The page inventory, public source fingerprints, exact excerpts, and editorial
review records live in [documentation-manifest.json](documentation-manifest.json).

Author review covers accessible openings, progressive depth, business decisions,
implementation evidence, independent consultation, and increasing reader responsibility.
Parallel cross-review checks instructions and capability claims against public
implementation. These are editorial reviews, not an observed human reader trial.

General concepts lead each topic. Ecommerce connects the examples through one
reader-created application without assuming an existing company. The reader uses
`neo new`, `neo build`, `neo run`, `neo test`, and `neo ide`; framework checkout and
contributor tools are confined to the contribution branch. Checkout, payment,
fulfilment, and other policies remain explicit design extensions, not a claimed
complete commerce product. Agent-facing documentation is separate.

## Progressive examples and complete checkpoints

Teaching blocks show relevant declarations and behaviour without module headers
or import catalogues. Each step identifies its destination and role in the same
project. Small explanations precede the code they motivate. Complete application
files live in five downloadable checkpoints: first Cart, Cart additions, end of
Build, Connect overlays, and the Postgres overlay. They preserve generated project
settings and split entity state, event definitions, individual payloads, commands,
queries, integrations, service registration, and application composition.

The downloads are generated from `examples/` by `scripts/generate-examples.mjs`.
The content check compares their bytes with deterministic regenerated archives;
CI fails on a stale download. Four tests cover extraction with standard tar,
binary fidelity, deterministic ordering, unsafe/duplicate paths, and checkpoint
boundaries. Complete source retains the imports today's APIs require; fragments
do not claim that `Core` alone currently re-exports every framework operation.
Application modules contain no `LANGUAGE` pragmas.

Concept declaration examples show their canonical marker and omit deriving clauses.
The event marker generates Show, Generic, FromJSON, and ToJSON, but not Eq; complete
event fixtures retain equality for full-payload assertions. Entity files now use `deriveEntity` after `initialState`, `update`, and the
imported event-routing companion. Ordinary value instances remain in complete
source; entity derivation deliberately does not impose Show.

## CLI and application verification

### Current derive-helper migration

The current examples and downloads use the five canonical helpers from `Core`,
including the new `deriveEntity`. They require the upcoming framework exports;
the released revision below is insufficient. Entity business functions and event
routing were preserved while generated instances and direct TH imports were
removed. Checker coverage now rejects legacy markers and direct TH imports in
both teaching fragments and downloadable source.

The compilation and runtime evidence below is **historical, before this marker
migration**. It must not be read as verification of the changed downloads.
Framework verification now passes: all 253 library modules typecheck;
70 service TH/auth examples and 28 outbound integration examples pass. The new
13 entity tests cover replay, routing, defaults, JSON, fields without Show,
custom instances and names, custom identifiers, missing companions, conflicting
families, aliases, and repeated derivation. The service fixtures first failed on
missing Core exports, then passed with the implementation. One new fixture needed
an explicit record constructor to satisfy the existing duplicate-field warning.
Portable lint and the expectation guard pass.

The correct outbound test registration is `nhcore-test-core`. An initial run in
`nhcore-test-integration` selected zero tests and was rejected by the runner; the
old aggregate `nhcore-test` suite has an unrelated missing `ArraySpec` registration.
Neither attempt is counted as a pass. The declared criterion now names the
registered core suite.

The revised first-Cart checkpoint compiled against framework revision
`45e6fcc251de413d198be5c13cd583a82d3b2d57`; its two unit examples passed.
Its HTTP readiness check was interrupted before a result. On 2026-09-16 Nick
explicitly requested stopping compilation and prioritizing delivery of the docs.
The checkpoint runner, API-catalog regeneration, and watcher were stopped.
The other four checkpoints were not recompiled after this migration; no passing
HTTP run is claimed for the revised downloads. API-catalog refresh and full
framework acceptance remain unfinished; the PR stays a draft.

The only production delta after the consumer pin extracts the unchanged
entity-family comparison into a private helper; the local 253-module check and
98 targeted tests cover that final source. The cold consumer build reported
1410.2 seconds. Build optimization and the requested retrospective are deferred
so they do not delay documentation delivery.

### Historical verification before the derive-helper migration

An official Neo 0.10.0 Apple-silicon release asset was downloaded into an isolated
temporary directory. Its SHA256 matched the release asset:
`69323f581ef80f8a9dbe6bc2d02e1311f8dae82b7bbf3b93edea94b846a955d8`.
`neo new` selected framework revision
`25bd7027a85b8f2999602f66b46b4b6b133c22e8`.
The user's installed CLI was not replaced.

That released CLI lacked `DerivingStrategies` in its generated preset. Framework
concept markers emit strategy-qualified deriving clauses, so this change adds the
missing extension to the shared application/library/test configuration. A real
rendering regression failed before the correction and passed afterward. All 629
CLI unit tests, the targeted generated-project integration test, and the fresh
packaged-binary new-project end-to-end test passed. The local corrected binary
still reports 0.10.0; it is **unreleased**, and the setup guide explicitly records
that prerequisite. No release was published as part of this work.

The corrected packaged binary's SHA256 is
`b1dfa23946bb79a19e527633fc7d01855de72414ea1daa4f999fb8a6e02b70c3`.
Those checkpoint builds used this CLI against the released framework pin, with no
`cabal.project.local` override and no language pragmas.

- First Cart: `neo build` and `neo test` passed, including two decision examples
  and one HTTP scenario. The first cold HTTP startup exceeded Neo's 60-second
  readiness timeout. Diagnostic `neo run` started normally, and the warmed
  `neo test` retry passed; the original failure is retained in the evidence. Connect encountered the same
  cold-start timeout; its diagnostic run also started normally.
- Cart additions: the then-current downloadable archive passed `neo build` and
  `neo test`, including eight decision/replay examples and two HTTP scenarios.
- End of Build: all 21 application modules and test modules compiled; `neo test`
  passed 12 decision/replay examples and three HTTP scenarios. A separate
  cross-domain smoke confirmed a positive selection changes the cart while stock
  remains untouched before integration, and a zero-quantity refusal changes neither.
- Connect: all 25 application modules and test modules compiled; `neo test`
  passed 12 decision/replay examples and four HTTP scenarios. A separate
  observation on temporary port 8082 verified an immediate empty cart and a
  second distinct cart after 32 seconds, plus byte-identical upload/download of
  a 23-byte text file with application configuration enabled. The timer and
  temporary port were then removed; ordinary HTTP tests passed with the original
  App restored byte-for-byte.
- Postgres overlay: the then-current source compiled all 22 application modules,
  its executable, and the test executable. No database was accessed.

The repository-wide Rust formatting check reports pre-existing formatting drift;
unrelated formatting was not rewritten. The full framework Haskell suite was
attempted earlier, but stopped before runtime tests because the default database
port belongs to an unrelated application. No passing full-framework suite is
claimed. Complete that check in isolation before final PR review.

## Content and website checks

- 49 human pages cover 24 required public capability areas, grounded in 222
  fingerprinted public evidence files and 51 checked literal excerpts.
- 73 documentation checker assertions cover missing content, source/excerpt drift,
  review coverage, links, diagrams, binary screenshots, image zoom controls,
  reader-owned workflows, pragma/import/module/deriving scaffolding, canonical
  Core helper usage, entity boilerplate rejection, and code disclosure.
- Four archive tests passed; all five downloads are reproducible from source.
- All 46 shell blocks pass `bash -n`; syntax checks do not imply execution.
- Public source hyperlinks were checked against existing repository paths.
- Astro diagnostics passed with zero errors, warnings, or hints. The production
  build/search indexed 769 pages; all 49 human routes passed rendered-link,
  anchor, and image-zoom checks.
- The portable dialect lint passed with no hints. Spec criteria and
  the expectation guard passed; current API drift validation awaits catalog regeneration. No existing test expectations were changed.

These checks do not certify pedagogy, historical-data compatibility, provider
acceptance, or the correctness of a reader's business policies.

## Introduction and diagrams

The supplied 13-page explainer was read in full and inspected visually. Its
progression from meaning to history, correction, shared models, bounded feature
growth, and strategic evaluation informed the longer introduction. Technical
claims were checked against public implementation rather than inferred from the
explainer. Early chapters remain code-free.

Nine original diagram pairs are maintained as editable Draw.io files and SVGs.
Draft renders were inspected for labels, routing, and clipping; structural
validation passed with zero errors or warnings. Canonical notation was researched
from Adam Dymitruk's [Event Modeling introduction](https://eventmodeling.org/posts/what-is-event-modeling/),
Martin Dilger's [documentation article](https://eventmodelers.ai/docs/blog/documenting-software-with-event-modeling/),
and the [Event Modelers cheat sheet](https://eventmodelers.ai/cheatsheet/).
Commands are blue, events orange, and read models green. Worked timelines follow
cart creation and an addition through screens, requests, facts, and views. Two
units are explicitly distinguished from one cart entry.

The first model's footer now describes the feature the reader builds. Its updated
Draw.io source passed validation and visual review before the SVG was re-exported.
Sources, exports, screenshots, and alternatives are checked by the content gate.

## IDE continuity and image enlargement

Both IDE images were recaptured from the same actual reader-created project with
Neo 0.10.0. The overview shows CreateCart, CartCreated, and CartSummary. The detail
shows the later Cart checkpoint with AddItem selected and its ItemAdded connection
highlighted. `neo inspect sync` and `neo validate` passed for both saved models.
The AddItem view belongs to a later return after implementing that command.
A model relationship is not presented
as proof that an operation ran. Capture provenance stays here, outside the lesson.

Starlight Image Zoom 0.15.0 and the supported Unified Markdown renderer enlarge
SVGs and PNGs in a dialog without changing pages. [ADR-0078](../docs/decisions/0078-documentation-image-zoom.md)
records the dependency choice. Earlier Safari verification confirmed Escape,
close controls, restored focus, Enter to reopen, and preserved URL/scroll position.
Built-page checks require accessible zoom triggers on all registered illustrations.

Final Safari review verified the short first-cart examples and download link at
normal reading size. The recaptured AddItem image opened in the dialog at the same
guide URL; Escape returned focus to its zoom trigger and retained the reading position.

## Remaining validation boundaries

- A reader matching Jess's profile has not completed the reader trial.
- An evaluator has not walked only the accessible openings.
- No real orders, payments, email, provider AI, or paid services are triggered to
  verify prose. Provider sandbox/live checks remain application responsibilities.
- Postgres compilation, database startup, restart durability, and backup/restore
  are separate checks; a build is not a durability result.
- Partial provider examples describe named APIs and application-defined outcomes;
  they are not claimed as independently runnable provider integrations.
- New English content uses the existing translation workflow and fallback; no
  reviewed translations of the new corpus are claimed.

## Reader trial protocol

Give the reader the shop-on-paper, first-cart, and visual-IDE pages. Without
coaching, ask them to explain a rule, identify it in the graph, catch the
quantity-limit misunderstanding, introduce an unseen boundary change, and find
the relevant answer again. Record hesitation, incorrect predictions, and recovery.
Ask an evaluator to describe benefits, tradeoffs, and operational work from the
accessible sections. Revise from observations; agent review does not replace them.
