# Human documentation review

## Scope and method

The approved method lives in [DOCUMENTATION_PLAN.md](DOCUMENTATION_PLAN.md).
The page inventory, public sources, source fingerprints, exact excerpts, and
per-page editorial review records live in [documentation-manifest.json](documentation-manifest.json).

Author review covers accessible openings, progressive depth, business decisions,
implementation evidence, independent consultation, and increasing reader responsibility.
Parallel cross-review checks executable instructions and capability claims against
public implementation. Review records represent agent editorial review, not an
observed human reader trial.

The site teaches NeoHaskell concepts and capabilities for the reader's application.
An ecommerce practice project connects the worked examples without assuming an
existing company or requiring its story to understand general topic pages. The
public Counter starter and Cart/Stock testbed are explicitly identified as different
runnable anchors. Checkout, payment, fulfilment, and other application-specific
policies are design extensions, not a claimed complete commerce product.

## Verification results

- 47 authored/reviewed human pages, with 24 required public capability areas covered.
- 150 inspected public source files fingerprinted; 14 literal code excerpts checked.
- `pnpm test:docs`: 13 positive, negative, and boundary fixtures passed, including a missing domain-transfer review.
- `pnpm check:docs`: complete inventory, review records, source evidence, excerpts,
  internal links, and reachability from the home page passed.
- `pnpm check`: zero errors, warnings, or hints from Astro diagnostics.
- `pnpm build`: production build and search index completed, including locale fallback.
- `pnpm check:links`: all 47 authored routes and rendered local links/anchors passed.
- All 40 shell code blocks passed `bash -n`; this checks syntax, not execution.
- Public GitHub source links were checked against existing repository paths.
- Safari review verified the small-window lesson layout, menu ordering, exercise
  disclosure, and search results for stock. This is UI review, not a human reader trial.
- `./dev lint`: passed with no hints.
- `./dev doctor`, `./dev workflow-check`, `./dev codemap-check`,
  `./dev adr-website --check`, `./dev spec-check --criteria-tests origin/main`,
  `./dev spec-drift docs/changes/013-human-documentation.md`, local/committed
  design-review presence checks, release-fragment checks, and the expectation guard passed.

The full Haskell suite was attempted with `./dev exec ./dev test-all --require-all`.
Its pinned toolchain began compiling after cache delays. It was stopped before
runtime tests because the required default database port is occupied by an unrelated
application. No passing full-suite or acceptance-runtime result is claimed. Complete
that check in an isolated environment before promoting the draft for final review.

These structural checks do not certify pedagogy, historical-data compatibility,
provider acceptance, or the correctness of a reader's shop-specific policies.

## Cross-review changes

Following reader feedback on the initial framing, the home page, navigation,
section introductions, and topic pages were reviewed for applicability beyond
ecommerce. General problems now lead the explanations; cart, stock, and order
examples remain concrete exercises in the continuing practice project. This is an
editorial review of domain transfer, not evidence from a human reader trial.

Independent readers of the manuscript corrected the query derivation declaration
order against the compiling public fixture, clarified testbed versus generated-project
IDE workspaces, exposed the Git side effects of domain locking, and distinguished
secret-store persistence from transaction-store lifetime. A transfer milestone now
assembles Cart/Stock in the reader's own application. Source inspection also bounded
claims about retries, upload cleanup/downloads, query checkpoints, AI tools, and
provider callbacks. No framework source was changed as part of this documentation work.

## Explicit remaining validation boundaries

- A human matching Jess's profile has not yet completed the reader trial.
- An evaluator has not yet walked only the accessible openings.
- No real customer orders, payments, emails, provider AI calls, or paid services are
  executed merely to verify prose. Provider sandbox/live checks remain documented
  application responsibilities.
- Partial Haskell examples explain named APIs in context; they are not independently
  compiled complete applications. Exact excerpts are checked against repository
  source, separately from runtime validation of the owning project.
- The translated editions use the existing translation workflow and English fallback;
  this change does not claim reviewed translations of the new English corpus.

## Reader trial protocol

Give the reader the shop-on-paper, first-cart, and visual-IDE pages. Without coaching,
ask them to explain a rule, identify it in the graph, catch the quantity-limit
misunderstanding, introduce an unseen boundary change, and find the relevant answer
again. Record observed hesitation, incorrect predictions, and recovery. Ask a second
reader evaluating adoption to describe benefits, tradeoffs, and remaining operational
work from the accessible sections. Revise based on observations; do not replace
these trials with readability scores or an agent's self-assessment.
