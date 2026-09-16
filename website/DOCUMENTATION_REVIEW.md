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

- 49 authored/reviewed human pages, with 24 required public capability areas covered.
- 156 public evidence files fingerprinted, including the screenshot model fixture; 14 literal code excerpts checked.
- `pnpm test:docs`: 31 positive, negative, and boundary fixtures passed, including missing domain-transfer review, diagram source/export drift, binary screenshot drift, invalid asset paths, and missing image alternatives.
- `pnpm check:docs`: complete inventory, review records, source evidence, excerpts,
  internal links, and reachability from the home page passed.
- `pnpm check`: zero errors, warnings, or hints from Astro diagnostics.
- `pnpm build`: production build and search index completed, including locale fallback.
- `pnpm check:links`: all 49 authored routes and rendered local links/anchors passed.
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


## Expanded introduction and visual review

The supplied 13-page `neohaskell-explainer-v5.pdf` was read in full and all pages
were inspected visually. Its progression from meaning to history, correction,
shared models, bounded feature growth, and strategic evaluation informed the
expanded introduction. Technical claims were checked against the public sources;
the PDF was not treated as evidence of runtime guarantees.

The home page and start section grew from approximately 3,300 to 9,500 words,
including two new chapters on history/change and growth by slices. Independent
cross-review checked natural depth, domain transfer, and the boundary between
compiler checks and policy correctness. Early chapters remain code-free.

Seven original diagrams were authored as editable Draw.io files and exported as
editable SVGs. All passed the Draw.io skill's structural validation with zero
errors or warnings after correction of an overlapping annotation. Every draft
render was inspected for readable labels, routing, and clipping. The final SVGs
were checked in the site through the Tailscale preview: Safari showed the expanded
chapter, accessible diagram link names, and a working full-size SVG link. A narrow
window preserved readable labels and allowed horizontal scrolling within the image.

The current content gate, Astro diagnostics, production build/search, rendered
links and image resources, spec checks, release fragments, expectation guard, and
whitespace checks passed. This update changes documentation and its presentation;
no additional application runtime result is claimed.

## Event models and actual IDE captures

Notation was researched from Adam Dymitruk's [Event Modeling introduction](https://eventmodeling.org/posts/what-is-event-modeling/),
Martin Dilger's [documentation article](https://eventmodelers.ai/docs/blog/documenting-software-with-event-modeling/),
and the [Event Modelers cheat sheet](https://eventmodelers.ai/cheatsheet/).
Two original Draw.io models trace cart creation and a subsequent addition through
screens, blue commands, orange events, and green read models. Example values and
field names were checked against the public Cart commands, events, and query.
The text explicitly distinguishes two units from one cart entry. Five earlier
conceptual diagrams were recoloured to use the same semantic mapping.

Two PNGs were captured from a running Neo CLI 0.1.4 IDE through Safari: an overview
and a selected AddItem command highlighting its ItemAdded relationship. The model
was produced by `neo inspect sync` against an isolated copy of public testbed source,
then reduced to five existing nodes and their original edges/fields for legibility.
Chapter grouping, slice ordering, and layout were curated; screenshots themselves
are unaltered captures. `website/fixtures/cart-event-model.json` preserves that
model and passed `neo validate`. The IDE server connected to the isolated workspace;
no application HTTP server, database, or external provider was needed for capture.

Independent visual review checked both screenshots against the guide: visible
labels, fields, colours, selected connection, public-only content, and the distinction
between a model relationship and runtime evidence. The guide states the version
and curated scope, explains each capture, and links to the full-size images.
Registered PNGs are hashed as binary data; fixtures verify that decoding them as
text fails the hash check. The checker also validates enclosing full-size image
links independently from their embedded images.

Both new diagram drafts and the five recoloured drafts passed structural validation
with zero errors or warnings and were inspected visually. The nine registered
diagram pairs and two PNG captures pass the content gate. All 31 checker fixtures,
Astro diagnostics, the 763-route production build/search index, and rendered links
passed for this revision. Safari verified the event-model lesson, its full-size
SVG, and the screenshot guide through the hostname-allowed Tailscale preview.
Spec criteria/drift checks, release fragments, expectation guard, and whitespace
checks also passed. No additional application-runtime test result is claimed.

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
