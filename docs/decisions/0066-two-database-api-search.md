# ADR-0066: Two-database API search (`./dev api`) — engine, workarounds, and freshness contract

> Part of #715 / PR #720 — pipeline plan Phase 4 (API knowledge delivery).
> Supersedes plan Phase 4 task 3 as written ("scripts/hoogle-setup builds a
> database over nhcore") — the plan text carries a dated amendment pointing
> here.

## Status

Accepted (2026-07-08)

## Context

Phase 4's goal is "transcribe, never recall": an executor model must resolve
API symbols by lookup, not from training-data vanilla Haskell. That needs a
type-directed search over the NeoHaskell surface that (a) ranks the dialect
first, (b) still shows what vanilla offers — the input to the escape-hatch
decision (`.hlint.yaml within:` + belongs-in note; rule of three → Core
primitive) — and (c) can never serve a stale or wrong surface with
confidence, because a transcribing executor copies what it reads.

The design went through three iterations in one day; this ADR records why
the shipped one is shaped the way it is, and the exact reproduction for the
two upstream defects it works around, so the workarounds can be retired
when upstream fixes land.

## Decision

**Two hoogle databases, one verb (`./dev api`, `scripts/api`):**

- **Pass 1 — `.hoogle-neo.hoo`**: nhcore + nhintegrations only, ranked on
  top. Indexed **directly from the committed, CI-sync-gated
  `codemap/signatures/*.txt`** — not from dist-newstyle haddock output.
  Consequences: the DB cannot drift from the surface CI verified; a fresh
  clone uses `./dev api` with zero cabal build; and `scripts/api` rebuilds
  lazily whenever the DB is older than any signature file (mtime check
  against a gated source).
- **Pass 2 — `.hoogle-vanilla.hoo`**: the **real dependency closure + boot
  libs**. The dev-shell GHC package DB (haskell.nix `shellFor`) enumerates
  the closure; each package's hoogle txt is fetched from Hackage
  (`https://hackage.haskell.org/package/<pkg>-<ver>/docs/<pkg>.txt`) into
  the gitignored, pkg-version-keyed cache `.hoogle-vanilla-src/`.
  Compiler-internal packages (`ghc`, `ghc-prim`, `rts`, …) are excluded —
  `GHC.Utils.Json` as the only answer to `toJSON` was anti-guidance.
  Shown below a disclaimer whenever non-empty; omitted when empty.
  Exit contract: 0 neo hit / 1 none / 2 usage / 3 vanilla-only.
- Both DBs and all warn files are **local build artifacts, gitignored**
  (the vanilla DB is machine-shaped and ~20 MB; haddock output is not
  byte-stable across platforms, so a commit-and-diff gate is impossible).
- **Smoke assertions** close every diagnosed failure in this family
  (`build-hoogle-dbs`): neo must answer `Task.yield` + an nhintegrations
  symbol; vanilla must know `Data.Aeson` and must NOT serve `GHC.Utils.*`;
  `.hoogle-neo.warn` is capped at `WARN_BUDGET` lines.

**Engine: the nixpkgs hoogle binary, with `shell.withHoogle = false`.**

## The two upstream defects (with reproductions)

Both verified 2026-07-08 against haskell.nix (flake.lock pin of that date),
GHC 9.8.4, hoogle 5.0.18.4. Retire the corresponding workaround when the
reproduction stops reproducing.

1. **haskell.nix's `withHoogle` wrapper silently overrides `--database`.**
   With `shell.withHoogle = true`, the shell's `hoogle` is a wrapper that
   injects its own generated database; explicit `--database=FILE` flags are
   ignored without diagnostic — queries answer from the wrapper's DB.
   Reproduce: enable `withHoogle`, `hoogle generate --database=/tmp/x.hoo
   --local=codemap/signatures`, then `hoogle --database=/tmp/x.hoo
   Task.yield` → results clearly not from `/tmp/x.hoo`.
   Workaround: `shell.withHoogle = false` in `nix/hix.nix`.

2. **The haskell.nix-built hoogle 5.0.18.4 ignores `--local=DIR` txt dirs.**
   `hoogle generate --local=DIR` over a directory of hoogle-format txts
   produces a database that answers "No results found" for symbols present
   in those txts; the nixpkgs build of the *same version* indexes them
   correctly. Reproduce: build both binaries, run the identical
   `generate`/query pair, diff the answers.
   Workaround: `haskellPackages.hoogle` (nixpkgs) in `shell.buildInputs`,
   NOT `shell.tools`.

## Alternatives considered

- **Plan task 3 as written** (one local hoogle DB over nhcore, stock
  toolchain): rejected — a single database cannot both rank the dialect
  first and show vanilla below, and the stock setup hit defect 2.
- **Signature-grep only, no hoogle** (interim design, PR #720 early
  commits): worked but had no type-directed search — respelling vanilla
  intent ("String -> IO ()") into the dialect surface is exactly where
  type search earns its keep.
- **haskell.nix `withHoogle` database as pass 2** (it does contain the dep
  closure with docs): rejected — couples pass 2 to haskell.nix internals
  (defect 1's wrapper), and haddock-building all deps locally is a heavy
  shell-entry cost; Hackage already hosts the txts.

## Consequences

- `./dev codemap` refreshes signatures, then DBs; `./dev api` lazy-builds
  on miss/staleness. CI's codemap-sync job builds + smoke-checks the DBs
  as a canary but diffs only `codemap/` (the DBs are not committable
  artifacts by construction).
- First vanilla build fetches ~250 txts from Hackage (cached thereafter;
  CI caches `.hoogle-vanilla-src/`). Offline first-build degrades to
  boot-libs-only and FAILS the `Data.Aeson` smoke — loudly, by design.
- hoogle 5's parser cannot express a handful of legitimate signature
  shapes (implicit params, some class members): 13 warn lines today,
  budgeted at 20. Exceeding the budget fails the build rather than letting
  dialect symbols go silently invisible (the `Record.lens` failure class).
