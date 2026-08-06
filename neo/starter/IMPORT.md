# Internalized NeoHaskell starter (`neo/starter/`)

This directory is the monorepo-owned copy of the NeoHaskell project starter that
`neo new` scaffolds from. It is the single source of truth for generation: the
`neo` binary embeds this tree at compile time (rust-embed, see
`neo/src/network.rs`), so `neo new` never downloads a starter at runtime and the
starter content is pinned to the exact monorepo revision the binary was built
from.

Fix generation bugs here, in the monorepo, not in any external repository.

## Provenance

- Source repository: `github.com/NeoHaskell/neo-starter`
- Imported ref: `origin/main`
- Import policy: every git-tracked file of the source repository is imported
  verbatim, preserving the starter's functional contents and the generated
  project UX. No source files are edited on import.

`neo new` copies this tree into the new project, then `neo` reconciliation
(`neo/src/reconcile/`) regenerates derived files such as `<name>.cabal`,
`cabal.project`, and `flake.nix`. With network access, reconciliation resolves the
current upstream `neohaskell` revision. Offline generation preserves the
dependency pin committed in this starter; it guarantees deterministic scaffolding,
not automatic equivalence to a newer upstream checkout. The monorepo consumer
contract overrides that input to the checkout under test when verifying cross-
component compatibility. Internalization changes only the origin of the starter
files from a network download to this embedded tree.

## Intentional exclusions (parity policy)

Only the source repository's git-tracked working tree is imported. The following
are intentionally NOT imported, expressed as durable patterns rather than a
snapshot file count (which would rot on the next starter change):

- Version-control metadata: the source repo's `.git/` directory.
- Build artifacts and caches (the source repo's own `.gitignore` set):
  `dist-newstyle/`, `dist/`, `result`, `result-*`, `.direnv/`, `uploads/`.
- Secrets and local environment: `.env` (only the committed `.env.example`
  template is imported), `*.swp`, `*~`, `.DS_Store`, `.idea/`.
- Repository-only metadata not part of a generated project: any CI workflow
  definitions, issue/PR templates, or funding/marketing files the source repo
  may carry at its top level.

This exclusion policy is enforced executably by `./dev neo-skills-check`, which
fails if any excluded artifact class appears under `neo/starter/` or if this
manifest is missing. The invariant surfaces a generated project needs
(application entry point, launcher, cabal project, dev flake, test tree) are
also asserted there by presence, never by count.
