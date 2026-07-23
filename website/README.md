# NeoHaskell Documentation

The documentation site for [NeoHaskell](https://github.com/neohaskell/NeoHaskell),
built with [Astro Starlight](https://starlight.astro.build/).

This is a deliberately small scaffold: a home page, a getting-started page, and an
Architecture Decision Records section generated from the repository's decision log.
It is published in English (the source language) plus Spanish, French, Armenian,
Japanese, and Russian; the non-English locales are kept in sync automatically by
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
```

## Content

Documentation source files live in `src/content/docs/`:

| Path | Content |
|------|---------|
| `index.mdx` | Home page |
| `getting-started/index.mdx` | Setup pointer to the repository |
| `adrs/index.mdx` | **Generated** ADR landing page — do not edit by hand |
| `adrs/<slug>.md` | **Generated** ADR detail pages — gitignored, do not edit |
| `<locale>/**` | Localized pages for `es`, `fr`, `hy`, `ja`, `ru` |

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
