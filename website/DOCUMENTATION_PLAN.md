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

The rhythm is situation → decisions → model → implementation → evidence → deeper
explanation. This is an editorial tool, not a mandatory set of repeated headings.
An evaluator reading only accessible openings must still encounter a coherent
account of business benefits, choices, costs, and limitations. Do not make those
openings depend on previously skipped code sections.

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
- Use the existing public starter and testbed as executable anchors. Distinguish
  implemented examples from shop-specific designs the reader must implement.
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

## Content architecture and writing ownership

English source is in `src/content/docs/`; existing locale fallback and translation
automation remain the translation strategy. Generated ADR pages keep their generator.

| Section | Purpose | Planned subjects |
| --- | --- | --- |
| Start / evaluate | Benefit before setup | Why NeoHaskell; fit and tradeoffs; a shop without setup; trusting an agent; event modeling; reading paths |
| Build applications | First working slice to richer behaviour | Setup; visual IDE; first cart; commands/events/entities; queries; stock and checkout; HTTP/frontend; tests; permissions; configuration; transfer the working slice into the reader’s project; language essentials |
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
6. Can a direct-entry reader identify context and the next useful step?
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
