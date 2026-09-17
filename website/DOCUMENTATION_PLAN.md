# Human documentation: progressive depth

Status: methodology approved by Nick in the authoring conversation; manuscript implemented and editorially reviewed. Verification results and remaining reader/runtime trials are recorded in DOCUMENTATION_REVIEW.md.

## Purpose and reader

Jess has written some code, understands business processes, has an application idea,
and works alongside a coding agent. She learns to delegate implementation confidently
while owning business decisions and accepting the resulting behaviour. NeoHaskell's
constraints, event models, tests, and visual IDE make that confidence inspectable.
Compilation does not establish that the chosen business rules are correct.

These are human docs. Worked human–agent conversations belong here; instructions
intended for consumption by agents belong in a separate documentation project.

## The paint swatch principle

Depth increases across the whole journey, within each section, and within each page.
Each page opens with an understandable situation and earns the complexity it adds.
Early pages have a low technical ceiling. Later pages can explain sophisticated
problems in accessible language before developing their implementation.

An accessible introduction still needs depth. Give the reader time to understand
why the approach exists, how preserving history changes an application, how people
and agents share a model, and how explicit feature boundaries support change.
Develop the reasoning through examples and consequences before introducing code.
Length should serve those explanations; a short outline is not the introduction.

The rhythm is situation → decisions → model → implementation → evidence → deeper
explanation. This is an editorial tool, not a mandatory set of repeated headings.
An evaluator reading only accessible openings must still encounter a coherent
account of business benefits, choices, costs, and limitations. Do not make those
openings depend on previously skipped code sections.

## Visual explanations

Use diagrams to teach relationships, comparisons, sequences, and decisions that
are harder to follow in prose alone. The author-supplied strategic explainer
informs the introductory arc and visual pacing. Its claims still need the same
implementation review as any other technical explanation.

Keep one central idea per diagram, readable labels, a consistent vocabulary and
palette, and a textual explanation beside it. Colour must not be the only way to
distinguish concepts. Provide descriptive alt text and in-page enlargement through Starlight Image Zoom. Show
refusals, asynchronous work, and design responsibilities where their absence
would create a misleading impression.

Author graphical resources with the Draw.io skill. Keep editable `.drawio` sources
and exported SVGs in `public/diagrams/`. Validate and visually inspect the export,
then register source/export fingerprints in the documentation manifest. The CI
gate checks image references, alt text, review records, and artifact drift; it
does not certify whether a diagram teaches the right idea.

## Three ways to read

1. Learn along a recommended beginning-to-end journey.
2. Evaluate the philosophy, commercial usefulness, and tradeoffs without building.
3. Consult an individual page with local context, prerequisites, and related topics.

Permit deliberate repetition for direct-entry readers. Provide descriptive links,
clear next steps, a glossary, and a capability reference. Do not force sequential
reading to answer a specific question.

## Teaching decisions

- State the central promise early; reveal its consequences through familiar problems.
- Give readers a useful illustrated experience before installation.
- Teach NeoHaskell's concepts and capabilities as tools for the reader's own
  application, whatever its domain. The documentation is about the language and
  framework; ecommerce supplies a continuing example and an optional practice project.
- Grow that practice project from one product and one order/cart. Introduce it as
  something the reader builds, with no existing company, brand, or business history
  they must adopt or remember.
- Lead general topic pages with the reusable problem and concept. Bring in the
  ecommerce example to make the idea concrete, then help the reader apply it to their
  own domain. Tutorial milestones can open directly with the practice task.
- Create one reader-owned project with the released `neo` CLI. Every main-journey
  implementation, test, IDE session, integration, and deployment continues that
  project. Teach `neo new`, `neo build`, `neo run`, and `neo test`; never require
  a framework checkout, reference application, or contributor build commands.
- Use public framework source and tests as author evidence. Keep source provenance
  in maintainer records or optional notes; it must not become the reader's workflow.
- Provide complete first-slice modules and wiring, followed by explicit additions.
  Distinguish implemented steps from later design exercises.
- Introduce complexity when the business needs it: quantity rules, stock, cancellation,
  reads, permissions, integrations, AI features, deployment, and operation.
- Teach use of the Neo IDE graph to connect business concepts with implementation.
- Show an early, recoverable agent misunderstanding and how Jess checks the correction.
- Gradually transfer decisions and verification to Jess. Exercises include optional
  suggested reasoning and observable happy, rejection, and boundary outcomes.
- Explain framework guarantees separately from tested behaviours and reader policies.
- Put adoption costs and limitations where evaluators will see them.
- End the main journey with operating and evolving an application confidently.
  Reusable packages, integration authoring, and framework contribution are branches.

## Code examples: reveal the behaviour before the file

Teaching examples show the relevant declarations and behaviour, in the style of
Java documentation examples: omit module headers, import lists, and unrelated
scaffolding. If an example needs to show a library import, show only `import Core`;
imports from the reader's own project are allowed when they help explain a connection.
Do not imply that a fragment is a standalone file or that today's `Core` exports
every framework API. After teaching the pieces, show the complete resulting file
in the page, including its module declaration and the imports it actually needs.
Complete files and checkpoints retain required imports; do not invent exports to
make an example shorter.
Neo configures
the compiler extensions in generated project settings. Application examples must
contain no `LANGUAGE` pragmas. If an example needs an extension missing from Neo's
preset, fix that preset and record the required CLI version; do not make the
reader configure the language manually.

Show each concept's canonical derivation marker with its declaration:
`deriveEvent`, `deriveCommand`, `deriveEntity`, `deriveQuery`, or
`deriveOutboundIntegration`, all imported through `Core`. The previous marker
names are compatibility APIs, not the style taught by new examples. Do not teach manual
`deriving` clauses or instances for boilerplate the marker supplies. Keep required
companion functions in the correct order; show them progressively when needed.
Additional equality used by complete event tests is separate from marker-generated
instances. Entity files place `initialState`, `update`, and their imported or
defined `getEventEntityId` before `deriveEntity ''CartEntity ''CartEvent`. The
marker supplies entity JSON, type-family, default, replay, and event-routing
instances, but does not impose `Show`. Ordinary value types retain any required
mechanical setup in complete files. State the required framework helpers and CLI
preset once in setup, with a link to the release notes for version selection.
Keep temporary release-status notices out of individual lessons. Merging feature
code and publishing the release are separate steps; maintainer evidence records
the actual publication state.

Keep files cohesive. Give each command, event payload, query, and integration a
clear home. Separate entity state and replay from the event definitions, keep a
small domain `Core` facade where useful, and keep service registration and
application composition distinct. Use the public starter as the reproducible
structural model; no private application material belongs in the examples.

Show code progressively within each page:

- Begin with the behaviour or question, then a small relevant declaration or
  decision. Explain its meaning before introducing the next part.
- Show only the lines needed for the point under discussion. Do not dump imports,
  complete modules, wiring, and tests together as the first explanation.
- Name the destination relative to the reader's project root and explicitly say
  whether to create, replace, or edit it. Explain directory creation when new
  directories are introduced. For edits, identify the existing declaration or
  registration beside which the addition belongs.
- Finish a file's explanation with its complete assembled contents in the page.
  A reader must be able to create the file without guessing omitted definitions,
  imports, wiring, or declaration order. Show complete files only after explaining
  their important pieces; keep separate responsibilities in separate files.
- Use a path-titled code block for each complete file, preceded in Markdown source
  by `<!-- complete-file -->`. This distinguishes an intentional assembled file
  from a focused teaching fragment. Long recaps can use clearly labeled disclosures,
  but essential creation and edit instructions remain visible.
- Keep downloads as convenient checkpoints and comparison material. They must not
  be the only way to obtain the code needed for an implemented lesson. Conceptual
  sketches and provider-specific exercises must say what still needs designing;
  never label them as complete runnable files.
- Grow the same files and project across lessons. Show incremental changes and
  observable checks; never switch to another application to simplify the example.
- Keep a complete verification checkpoint behind the lesson. The reader should
  see a small next step while maintainers can build and test the whole result.

Automated checks distinguish focused teaching fragments from explicit complete-file
blocks. They reject language pragmas and legacy concept markers in both; complete
files require a project-relative path title and module declaration. Fragments still
reject distracting scaffolding and long unexplained blocks. Download drift is checked
separately. Editorial review verifies the progression, file instructions, and whether
the assembled code belongs at that point in the journey.

## Content architecture and writing ownership

English source is in `src/content/docs/`; existing locale fallback and translation
automation remain the translation strategy. Generated ADR pages keep their generator.

| Section | Purpose | Planned subjects |
| --- | --- | --- |
| Start / evaluate | Benefit before setup | Why NeoHaskell; history and correction; growth by slices; fit and tradeoffs; a modeling exercise without setup; trusting an agent; event modeling; reading paths |
| Build applications | First working slice to richer behaviour | Setup; first cart; visual IDE; commands/events/entities; queries; stock and checkout; HTTP/frontend; tests; permissions; configuration; extend the reader’s working project; language essentials |
| Connect systems | Effects beyond a single service | Integration lifecycle; cross-domain workflows; HTTP/providers/payments; email; files; PDF/OCR; AI; tools/agents; timers; custom integration authoring |
| Run and evolve | Deploy, diagnose, and change | Persistence; deployment; observability; recovery; evolution; security; performance; contribution |
| Reference | Direct consultation | CLI; language vocabulary; capability map; glossary; troubleshooting |

Coverage comes from the public capability map: primitives, traits where useful,
concurrency, system, HTTP client, JSON/schema, auth/config, Decimal/NeoQL where
supported, events/commands/entities/queries, transports, integration runtime,
uploads, application wiring, Postgres, testing, public integrations, and Neo CLI/IDE.
Exclude the unrelated ring integration. Do not imply a payment, shipping, or commerce
adapter exists unless implementation evidence supports it.

## Source and confidentiality contract

Public NeoHaskell source, signatures, starter, and tests are the authority for
technical claims. Inspect the relevant implementation before teaching its behaviour.
Private application code may inform understanding only: never reproduce its code,
names, identifiers, paths, business rules, data, screenshots, architecture, or extracts
in these docs, source manifests, examples, or review records. Construct original
fictional ecommerce examples and substantiate APIs solely with public source.

Runnable examples must identify their execution context and expected result. Exact
public-source excerpts should be traceable. Adapted or partial snippets must be
labelled as such; business sketches must not masquerade as runnable applications.
Do not present a generic prompt as a substitute for teaching a feature concretely.

## Verification contract

Event-model examples use the notation taught by Adam Dymitruk and Martin Dilger:
blue commands, orange events, green read models, screens above behaviour, and a
left-to-right timeline. Use original worked examples with concrete information
and explicit role labels. Introduce one action before a longer scenario; teach
acceptance/refusal examples alongside the timeline. Cite the primary teaching
sources and keep their methodology distinct from NeoHaskell implementation claims.
Use the same semantic colours in earlier conceptual illustrations.

The visual IDE guide includes actual captures of a running IDE, with a small public
example and a closer view of a selected command. Explain what to look at in each
image, and provide in-page image enlargement
and useful text alternatives. Keep editable Draw.io sources for diagrams
and a reproducible public model fixture for screenshots. Record capture version and
curated scope in the maintainer review, outside the reader-facing lesson. The content gate checks
registered assets and their hashes; image review checks what readers can see.

Automated checks:

- Validate the planned page inventory, frontmatter, source-evidence paths, internal
  links, and section reachability; include negative fixtures for the checker.
- Check exact source excerpts against their public originals where registered.
- Run Astro content/type checking and a production build; inspect rendered pages.
- Keep ADR generation faithful and run applicable repository governance checks.
- Wire the documentation checks into CI so the plan has an executable maintenance gate.

Semantic review of every authored page:

1. Does the opening make sense to Jess and an evaluator without reading code?
2. Is the depth appropriate to this point in the journey?
3. Are the business decision, benefit, and relevant tradeoff explicit?
4. Are implementation claims grounded in public code and limitations honest?
5. Does Jess gain something she can understand, decide, or confidently delegate?
6. Can a direct-entry reader identify context and the next useful step? For an
   implemented lesson, can they create or update each named file from the page,
   using the complete contents after the explanation rather than guessing gaps?
7. Do exercises transfer responsibility and provide observable checks?
8. Is the prose natural, with flexible structure and no private-source disclosure?
9. Is NeoHaskell the subject, with ecommerce serving as the example? Can someone
   building a different kind of application use the explanation without following
   an assumed company's story? Keep worked examples concrete rather than replacing
   every domain term with an abstraction.

Record source inspection, automated results, and semantic review separately. A
passing link check is not proof of pedagogy or a live external-provider transaction.

## Reader trial

After an editorial review, ask a real reader matching Jess's profile to explain a
rule, navigate the relevant IDE graph, spot a wrong agent proposal, make an unfamiliar
variation, and find an answer again later. Separately ask an evaluator to read only
accessible openings and explain fit and tradeoffs. Record observed difficulty and
revise. Agent review is a rehearsal, never evidence that these human trials occurred.
Include a reader working outside ecommerce: ask them to apply a concept to their
own process and identify which choices in the practice project are domain-specific.

## Delivery and evidence

Maintain `documentation-manifest.json` as the page/coverage/source inventory and
`DOCUMENTATION_REVIEW.md` as the review record. The docs checker validates that every
planned page and every required topic exists. Record pending live-provider tests and
human reader trials explicitly rather than calling them verified.

## Main website and documentation entry

The public root `/` introduces NeoHaskell as a language, application framework,
and development toolkit. The documentation overview lives at `/docs/`; existing
lesson routes remain stable. The landing page links into the ideas, evaluation,
working-project, and contribution paths rather than duplicating the whole guide.

Its narrative follows the strategic explainer: abundant implementation makes
meaning important; accepted facts explain current values; explicit corrections
preserve history; a shared event model connects domain experts and implementers;
and bounded slices make the next change understandable. Show those relationships
visually before describing technical foundations. Identify the Haskell dialect,
typed application concepts, Neo CLI, and actual visual IDE.

Keep the claims grounded. History contains reasons and attribution only when the
application models them. Corrections and business rules are designed, not inferred
automatically. Bounded feature costs depend on stable contracts. Include adoption
tradeoffs and preserve human judgment. Do not invent customer logos, performance
numbers, release availability, or automatic correctness guarantees.

Use an original responsive visual design, accessible navigation and interactions,
and clear documentation calls to action. Reuse maintained Draw.io graphics and
real IDE captures where useful. Website-only verification covers root/docs routing,
CTA destinations, assets, keyboard interaction, and rendered layout; it does not
restart framework compilation.
