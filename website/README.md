# NeoHaskell Documentation

The documentation site for [NeoHaskell](https://github.com/neohaskell/NeoHaskell),
built with [Astro Starlight](https://starlight.astro.build/).

The human docs teach NeoHaskell from application intent through implementation,
integrations, and operation, with ecommerce as a continuing practice project.
Pages progressively deepen the explanation while
supporting evaluators and direct consultation. The approved authoring method lives
in [DOCUMENTATION_PLAN.md](DOCUMENTATION_PLAN.md); verification and its limits live
in [DOCUMENTATION_REVIEW.md](DOCUMENTATION_REVIEW.md).

English is the source language. Spanish, French, Armenian,
Japanese, and Russian locales are kept in sync automatically by
[`.github/workflows/translate.yml`](../.github/workflows/translate.yml).

## Prerequisites

- [Node.js](https://nodejs.org/) 22.12 or newer (required by Astro 7)
- [pnpm](https://pnpm.io/) — the version is pinned via the `packageManager` field in
  `package.json` (use [Corepack](https://nodejs.org/api/corepack.html) or install it
  manually)

## Develop

```bash
pnpm install
pnpm dev
```

Open [localhost:4321](http://localhost:4321) to view the site.

## Build

```bash
pnpm build      # production build into ./dist
pnpm preview    # serve the production build locally
pnpm check      # type-check and validate content collections
pnpm test:docs  # positive/negative fixtures for the documentation gate
pnpm check:docs # inventory, public evidence, excerpts, links, and review records
pnpm check:links # after build: rendered routes and anchors
```

## Content

Documentation source files live in `src/content/docs/`:

| Path | Content |
|------|---------|
| `index.mdx` | Home page |
| `start/**` | Philosophy, evaluation, no-install modeling exercise, and trust |
| `getting-started/**` | Executable setup and visual IDE orientation |
| `build/**` | Cart/stock, commands, state, queries, access, tests, and language |
| `connect/**` | External providers, email, uploads, documents, AI, and timers |
| `operate/**` | Persistence, deployment, recovery, evolution, and contribution |
| `reference/**` | Capability map, CLI, glossary, and troubleshooting |
| `guides/**` | Preserved guide URLs and deployment probe reference |
| `adrs/index.mdx` | **Generated** ADR landing page — do not edit by hand |
| `adrs/<slug>.md` | **Generated** ADR detail pages — gitignored, do not edit |
| `<locale>/**` | Localized pages for `es`, `fr`, `hy`, `ja`, `ru` |

## Author and verify a human page

Read the plan before writing. Give the page an accessible business opening, a
purposeful increase in depth, concrete evidence, and useful onward links. Teach
NeoHaskell concepts first; use ecommerce as the continuing example and practice
project without assuming the reader belongs to a particular business. Use flexible
headings. Distinguish runnable public examples, partial snippets, and design
exercises. Human–agent collaboration belongs here;
agent-consumed instructions belong in their separate documentation project.

`documentation-manifest.json` is the maintained inventory. Add each page to
`requiredPages` and `pages`, record the public source files actually inspected,
and complete the per-page semantic review, including `domainTransfer`: the concept
can be applied outside ecommerce, and the example requires no existing company
story. The checker requires that review record; a boolean is not proof of teaching
quality. Register literal code excerpts in
`excerpts` when they are copied directly. `sourceHashes` uses SHA-256 over the
UTF-8 source bytes. When the gate reports a changed source, inspect its diff and
review every page listing it before updating that fingerprint. Update affected
excerpts, claims, and review notes together; refreshing hashes alone is not review.

Use `sidebar.order` to preserve the intended learning sequence, and link each new
page from an existing page. The checker rejects unreachable pages and stale
registered evidence. Run all checks above and inspect the rendered result.
`.github/workflows/docs.yml` runs the same checks on every PR and main push, so
an API change can flag affected docs even when no Markdown changed.

The source review proves what the inspected implementation says; compilation,
live-provider tests, and observed human comprehension are separate evidence.
Record those boundaries honestly in the review document.

### Architecture Decision Records

The ADR pages are generated from `docs/decisions/` by
[`scripts/generate-adrs.mjs`](scripts/generate-adrs.mjs):

- `adrs/index.mdx` — the landing page: a table of every ADR linking to its
  **internal** page (tracked in git; kept in sync).
- `adrs/<slug>.md` — one Starlight page per `docs/decisions/NNNN-*.md`, transcribed
  faithfully from the source record (gitignored — regenerated on every build).

The generator runs automatically before `pnpm dev`, `pnpm build`, and `pnpm check`
(the `predev` / `prebuild` / `precheck` npm lifecycle scripts), so a clean
`pnpm install --frozen-lockfile && pnpm build` regenerates everything from source.
Regenerate explicitly with `pnpm run generate:adrs` or `./dev adr-website`; CI
verifies the tracked landing page is in sync via `./dev adr-website --check`.

ADR records are maintained in **English** and are not machine-translated; each
locale's ADR landing page links to the English records.

## Translations

Non-English content lives under `src/content/docs/<locale>/`. English source pages
are translated to `es`, `fr`, `ja`, `ru` (and Armenian, `hy`) by
[`.github/workflows/translate.yml`](../.github/workflows/translate.yml) on pushes to
`main`. Pages without a translation fall back to the English source, and the
canonical-English ADR pages are excluded from translation.

## License

See the [NeoHaskell repository](https://github.com/neohaskell/NeoHaskell) for license
information.
