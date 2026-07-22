# NeoHaskell Documentation

The documentation site for [NeoHaskell](https://github.com/neohaskell/NeoHaskell),
built with [Astro Starlight](https://starlight.astro.build/).

This is a deliberately small scaffold: a home page, a getting-started page, and an
Architecture Decision Records index generated from the repository's decision log.

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
| `adrs/index.mdx` | **Generated** — do not edit by hand |

The ADR index is generated from `docs/decisions/README.md` by `scripts/adr-website`
(run `./dev adr-website` from the repository root; CI verifies it is in sync with
`./dev adr-website --check`).

## License

See the [NeoHaskell repository](https://github.com/neohaskell/NeoHaskell) for license
information.
