# Change 007: Give `bd` a first-class devShell entry through hix's own extension point

Issue #811 asks for `bd` (beads) to stop entering the dev shell through the
`pkgs.mkShell { inputsFrom = [ flake.devShells.default ]; }` workaround
(`flake.nix:66-71`) and to enter through the extension point hix actually
documents for "add one more CLI tool to the dev shell", so the wiring survives a
hix/haskell.nix input bump; and it asks that the procedure for adding the *next*
non-Haskell CLI tool be written down. Nothing about the shell's contents changes:
`bd` stays the same `flake.lock`-pinned derivation from beads' own flake, at the
same version, on the same `PATH`. This is a rewiring plus a documented recipe —
the caller-visible surface of `nix develop` is intended to be byte-for-byte the
same set of tools, and the criteria below exist to prove exactly that.

```yaml spec
issue: issue#811
kind: refactor
touches: [ci-cd, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Infrastructure only. No `nhcore` / `nhintegrations` public API changes — no
`codemap/signatures/` line is added or removed — so the promised diff is empty.

```diff signatures
```

### The wiring, stated precisely (this is the reviewable contract)

**Today** (`flake.nix:63-71`): hix builds `flake.devShells.default`; `flake.nix`
then *replaces* it with `pkgs.mkShell { inputsFrom = [ flake.devShells.default ];
packages = [ beads.packages.${system}.bd ]; }`. `inputsFrom` is a
build-environment composition trick — it re-derives a shell from another shell's
inputs. That is the fragility issue #811 names.

**After**, two edits, per the binding localization (`nh-mol-b8b`):

1. **`flake.nix`** — the `overlays` list gains one overlay that injects a single
   new top-level attribute:

   ```nix
   (final: _prev: { neohaskellTools = { bd = beads.packages.${system}.bd; }; })
   ```

   and the whole `devShells = (flake.devShells or { }) // { default = pkgs.mkShell
   {...}; }` block is **deleted**, so `devShells` comes straight from hix again.

2. **`nix/hix.nix`** — `shell.buildInputs` gains `pkgs.neohaskellTools.bd`,
   alongside the `git` / `nixfmt-classic` / `postgresql` / `hurl` /
   `poppler_utils` / `python3.withPackages` / `haskellPackages.hoogle` entries it
   already carries. `shell.tools` is **not** the route (haskell.nix resolves those
   against the project's GHC; `bd` is a Go binary).

3. **`README.md`** — the AC3 recipe: *add a key to `neohaskellTools` in
   `flake.nix`, then list `pkgs.neohaskellTools.<key>` in `nix/hix.nix`
   `shell.buildInputs`* — plus the rule that a **Haskell** tool goes to
   `shell.tools` instead, and the `doctest` note (`nix/hix.nix:28-30`) explaining
   why that distinction is load-bearing.

**Why a namespaced `neohaskellTools` set and not a bare `bd` attribute.** An
overlay that binds `bd` directly would silently *shadow* any same-named nixpkgs
attribute, in a diff where the shadowing is invisible. Today's pin has no such
attribute (`nix eval nixpkgs#bd` fails: "does not provide attribute"), so the
collision is latent, not present — which is precisely the kind of thing that
breaks a year later during an unrelated input bump. One namespaced attribute also
makes AC3's recipe a real two-step recipe with an obvious place to put the next
tool, and keeps the blast radius on `legacyPackages` down to one new name.

**What stays out of contract** (restated from intake, because a "cleaner" wiring
is exactly where these get lost): `beads.overlays.default` and
`inputs.beads.inputs.nixpkgs.follows` stay **unused** — that overlay calls
`final.buildGo126Module`, which our pin (`haskellNix/nixpkgs-unstable`) lacks, and
an overlay always evaluates against the pkgs it is applied to. `bd` keeps coming
from beads' self-contained `packages.${system}.bd`, built against beads' own
nixpkgs. No version bump: `bd` stays whatever `flake.lock` pins today.

## Criteria

Eight criteria, in two shapes. **Static (`unit`)** — the wiring is text that must
say specific things, and a static checker is the only thing that catches "someone
re-introduced `inputsFrom`" or "someone pointed `bd` at `pkgs.beads`" during a
future refactor; this mirrors how `./dev workflow-check` freezes CI wiring
(change 003, C2/C3/C7). **In-shell (`integration`)** — the properties that only a
real `nix develop` can prove: which store path `bd` resolves to, that every hix
tool survived the rewiring, and that the fingerprint witness still matches.

The proving tests are a new checker, `scripts/devshell-check` (registered as
`./dev devshell-check`, per the "every pipeline asset registers a verb" rule
`./dev doctor` enforces), in the two modes the repo already uses elsewhere:
`--self-test` for the pure static assertions (no nix, seconds — it runs in the
nix-free `checks.yml` job) and the default in-shell mode for the rest (invoked
from inside `nix develop`, locally at verify and from the two CI jobs that
already enter the shell). See **Edit-set delta** below.

**No property-based criterion is declared, and that is a decision, not an
omission.** The contract here is derivation *identity* and a fixed tool
inventory — `bd` is this store path, these fifteen binaries resolve, the
fingerprint is stable — not an algebraic law with a generator. The closest thing
to a property is C5's universal quantification over the declared tool set, which
is a finite enumeration and is written as one.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | `flake.nix` composes no shell: no `mkShell`, no `inputsFrom`, and no `devShells` override anywhere in the file — `devShells.default` is hix's own shell, unwrapped | `./dev devshell-check --self-test` (flake-wiring case) | unit |
| C2 | The overlay injects **exactly one** new top-level attribute, `neohaskellTools`, whose `bd` is `beads.packages.${system}.bd`; a bare `bd` (or any other bare top-level binding) is rejected as silent shadowing; `beads.overlays.default` and an `inputs.beads.inputs.nixpkgs.follows` override appear nowhere in `flake.nix` | `./dev devshell-check --self-test` (overlay-shape case) | unit |
| C3 | `nix/hix.nix` reaches `bd` as `pkgs.neohaskellTools.bd` from `shell.buildInputs` and **not** from `shell.tools`; the pre-existing shell declarations are still present and unweakened — every `shell.tools` entry (`cabal`, `hlint`, `fourmolu`, `hspec-discover`, `haskell-language-server`, `cabal-gild`, `ghcid`, `hiedb`, `doctest`), every prior `shell.buildInputs` entry, `shell.withHoogle = false`, and a `shell.shellHook` still exporting both `NEOHASKELL_SHELL_FP` and `NEOHASKELL_SHELL_PATH` | `./dev devshell-check --self-test` (hix-wiring case) | unit |
| C4 | Inside `nix develop`, `command -v bd` resolves into the **same store path** as `beads.packages.<current system>.bd` — not the host's `bd`, not nixpkgs' `beads` — and `bd --version` is unchanged from the pre-change shell and `>= 1.1.0` (the `.beads/formulas/*.toml` schema floor) | `./dev devshell-check` (bd store-path identity + version floor case), run in-shell | integration |
| C5 | Inside `nix develop`, every declared tool still resolves to a `/nix/store` path — `cabal`, `hlint`, `fourmolu`, `hspec-discover`, `haskell-language-server`, `cabal-gild`, `ghcid`, `hiedb`, `doctest`, `git`, `nixfmt`, `psql`, `hurl`, `pdftotext`, `python3 -c 'import yaml'`, `hoogle` — and `hoogle` is still the nixpkgs build, not haskell.nix's `hoogle-with-packages` wrapper (which would silently re-break `./dev api --local`) | `./dev devshell-check` (tool-inventory case), run in-shell | integration |
| C6 | The toolchain-fingerprint contract survives: `scripts/toolchain-fp` prints the same non-empty sha256 on two consecutive runs, in-shell `NEOHASKELL_SHELL_FP` equals that value, and `NEOHASKELL_SHELL_PATH` is non-empty and contains the `bin` directory `bd` resolves from (so `scripts/with-toolchain`'s fast path restores a PATH that still has `bd`) | `./dev devshell-check` (fingerprint-witness case), run in-shell | integration |
| C7 | The checker is wired where it cannot rot: the two jobs that already enter the shell (`test.yml` `build`, `test-macos.yml` `build`) invoke `./dev devshell-check` in-shell, and the nix-free `checks.yml` job invokes `./dev devshell-check --self-test`; a regression that drops `bd` or a hix tool therefore fails a **required** check on both covered systems | `./dev workflow-check` (devshell-check wiring assertions, incl. its `--self-test` fixtures) | unit |
| C8 | `README.md` carries the AC3 recipe and it cannot drift: it names the two steps (add a key under `neohaskellTools` in `flake.nix`; list `pkgs.neohaskellTools.<key>` in `nix/hix.nix` `shell.buildInputs`) and the Haskell-tool exception (`shell.tools`), and the checker asserts the recipe's anchors against the actual attribute name in the code, so renaming the attribute breaks the doc check | `./dev devshell-check --self-test` (doc-coherence case) | unit |

## Edge cases and failure modes

Each is named with its symptom, the thing that catches it, and what should
happen. The first is the one that can invalidate the route.

**F1 — hix's module `pkgs` does not see our overlay.** The route assumes the
`pkgs` argument in `nix/hix.nix` is the fixpoint our overlays are applied to
(`hixProject` is itself created inside an overlay, from `final.haskell-nix.hix`).
If haskell.nix hands the module a differently-pinned pkgs, evaluation fails with
`attribute 'neohaskellTools' missing`. That is a **contradiction of the binding
localization, not an invitation to improvise**: the implement node must **park the
run as `wrong-localization`** and re-enter at intake (where the recorded
alternative — an `overrideAttrs` on `flake.devShells.default`, `flake.nix` only —
gets weighed properly). Silently falling back to a `flake.nix`-only fix is the
one outcome this spec forbids.

**F2 — silent attribute shadowing.** A bare `bd` overlay attribute would override
a same-named nixpkgs package for everything built from our `pkgs`, including
`legacyPackages.<system>.bd`, with nothing in the diff to show it. Today's pin has
no `bd` attribute, so this is latent; C2 rejects the shape outright rather than
relying on that staying true.

**F3 — wrong `bd` source.** `pkgs.beads` resolves (nixpkgs pins **1.0.3**) and
would put a `bd` on `PATH` that passes "is bd available?" while being below the
`>= 1.1.0` floor the `.beads/formulas/*.toml` schema needs. C4 asserts the store
path, not merely the binary's presence, precisely because the weaker assertion
passes here.

**F4 — the beads overlay creeps back.** Applying `beads.overlays.default` (or
setting `inputs.beads.inputs.nixpkgs.follows`) reaches `final.buildGo126Module`,
which our pin lacks: evaluation fails on **every** system with a missing-attribute
error. Out of contract by intake; C2 freezes it statically.

**F5 — a supported system beads does not publish.** `beads.packages.${system}.bd`
is referenced inside `flake-utils.lib.eachSystem` for all four supported systems.
Nix attribute access is lazy, so a missing system fails only when that system's
`devShells.default` (or `legacyPackages.<system>.neohaskellTools`) is forced —
same failure surface as today's `flake.nix:69`, moved, not widened. Note the one
real widening: `neohaskellTools` is now reachable through `legacyPackages`, so a
`nix flake show`-style walk that forces it can surface this where it previously
could not.

**F6 — host `bd` shadowing in-shell `bd`.** The dispatcher host has its own `bd`
on `PATH` (a documented version skew; `nh-c3e`). A `bd --version`-only assertion
can pass while resolving to the host binary. C4 compares the resolved path against
the store path, and C6 checks that `NEOHASKELL_SHELL_PATH` — the witness
`scripts/with-toolchain` restores on its fast path — still contains `bd`'s `bin`
directory, so the fast path cannot silently hand back a host binary.

**F7 — fingerprint churn (expected, once).** Both edited files are inside
`scripts/toolchain-fp`'s hashed set (`flake.nix`, `nix/*.nix`), so the fingerprint
**value changes exactly once** when this lands. Every already-open shell, tmux
pane, and mid-run agent re-enters `nix develop` on its next `./dev` call and then
matches again. The *mechanism* must not change: an empty fingerprint (missing
hasher) still disables the fast path with a warning rather than failing silently.
C6 proves determinism and the witness; a changed value is expected, not a
regression.

**F8 — shell derivation identity changes.** Dropping the `mkShell` wrapper changes
the devShell derivation's name and hash. Nothing in the repo keys on the shell's
*name* (`scripts/with-toolchain` keys on the fingerprint plus the PATH witness), so
this is inert — but it does mean a one-time rebuild/substitute for everyone,
including CI until the cache warms.

**F9 — cache population.** `cachix-push.yml`'s path filter lists `nix/**` but not
`flake.nix`. This change touches `nix/hix.nix`, so the new shell closure **does**
get pushed. The gap itself (a `flake.nix`-only shell change never repopulating the
cache) is real and pre-existing, and is left to a follow-up bead rather than
widened into this change's scope.

**F10 — documentation drift.** A future rename of `neohaskellTools` that leaves
`README.md` describing the old name is caught by C8, which asserts the recipe's
anchors against the code rather than merely checking that prose exists.

**F11 — `shell.tools` misuse for the next tool.** The AC3 recipe must not tempt
someone into putting a Haskell tool in `neohaskellTools` (it would be resolved
against nixpkgs' GHC, not the project's — the exact reason `doctest` is pinned as
a `shell.tools` entry at `nix/hix.nix:28-30`). C3 and C8 both carry the
distinction.

### Concurrency-sensitive behavior

Nix is the concurrency story here, and the honest claim is that this change adds
no new sharing:

- **Concurrent `nix develop` entries** (parallel agents, several tmux panes, the
  CI matrix) racing to realise the new shell are serialised by the nix daemon's
  per-derivation locks. Worst case is duplicated substitution work, never a
  partially-populated shell — and each entry either gets the complete new closure
  or fails; there is no intermediate state a shell can be entered in.
- **The `with-toolchain` fast path** is read-only against `NEOHASKELL_SHELL_FP` /
  `NEOHASKELL_SHELL_PATH`; concurrent readers of a *stale* fingerprint all take the
  slow path (re-enter `nix develop`) independently. Stale-read behavior is
  fail-safe by construction, and this change does not alter it.
- **`scripts/toolchain-fp`** hashes files, holds no state, and writes nothing, so
  concurrent invocations are independent. C6's two-consecutive-runs assertion
  covers the determinism this relies on.

## Known limits — not proven mechanically

Kept out of the criteria table on purpose, so the table stays honest about what CI
actually proves:

**Only two of the four supported systems are exercised.** `flake-utils.lib.eachSystem`
covers `{x86_64,aarch64} × {linux,darwin}`, but CI enters the shell on
`x86_64-linux` (`test.yml`) and `aarch64-darwin` (`test-macos.yml`) only.
Cross-evaluating the other two from a developer machine is not available either:
the flake sets `allow-import-from-derivation = true`, so evaluating a foreign
system's devShell tries to *build* foreign derivations. The wiring is uniform
inside `eachSystem` with no system-conditional, which is the argument that the
remaining two systems behave identically — an argument, not a test. This limit is
pre-existing and unchanged by this change.

**AC2 ("survives a hix/haskell.nix input bump") is not directly testable** at
merge time — no bump exists to test against. What this change ships instead is the
*mechanism* that makes the next bump cheap (hix's own extension point rather than
a composition trick) plus C1-C3, which fail loudly if a future bump is "fixed" by
re-introducing `inputsFrom`.

## Edit-set delta (declared, not silent)

The binding localization's `files:` list is the **implementation** edit set:
`flake.nix`, `nix/hix.nix`, `README.md` — unchanged by this spec, and the route
(overlay injection + `shell.buildInputs`, no `mkShell`/`inputsFrom`) is followed
exactly. Localization enumerated no test assets, so the proving tests above add,
at test-writing time: `scripts/devshell-check` (+ its `./dev` verb registration),
`workflow-check` assertions for C7, and one `- run:` step in each of `test.yml`,
`test-macos.yml`, and `checks.yml`. These are test assets, not a re-scoping of the
implementation, and they route to no additional design review (`dev-pipeline`
carries no risk tag and no test globs, exactly like `ci-cd`). The spec commit also
adds `docs/decisions/0075-*.md`, its row in `docs/decisions/README.md`, and the
regenerated tracked `website/src/content/docs/adrs/index.mdx` that `./dev
adr-website --check` gates.

## Primitives

The primitives-first lens (lock 1), answered so the primitives review has a
record to check rather than re-derive. **The mechanical triggers are all
negative**: no new Haskell module, no new `build-depends`, no new flake input
(`beads` already exists), and no direct hackage import — this change contains no
Haskell at all, so the dialect's "reach hackage only through a `core/` wrapper"
rule has no surface here. The binding localization recorded the same conclusion
(`nh-mol-x4c` not triggered).

The lens still has a non-trivial answer at the Nix layer, and it is the reason
the attribute is a **set**, not a bare name. `neohaskellTools` is the primitive
this change introduces: the single, named place where a non-Haskell, non-hix-managed
CLI tool enters the dev environment. Everything about the design follows from
treating it that way — one injection point instead of a per-tool composition
trick, extension by adding a key rather than by wrapping the shell again, and a
documented recipe (C8) so the second tool costs one line instead of a fresh
invention. The rule-of-three question ("should this be promoted?") is therefore
answered pre-emptively rather than after the third `mkShell` workaround.

What is deliberately **not** promoted: no `./dev` verb, script, or abstraction is
added for *managing* the tool set (the recipe is two edits in two files), and
`scripts/devshell-check` is a test asset, not a primitive — it asserts the wiring,
it does not own it. If a third or fourth tool ever makes the two-file edit feel
repetitive, the next promotion is a `codemap/extension-points.yaml` row, which is
listed as a follow-up rather than pre-built here.

## User impact

**Not breaking, and deliberately invisible at runtime.** `nix develop` offers the
same tools at the same versions; `bd` is the same `flake.lock`-pinned build on the
same `PATH`. No public signature, wire format, or testbed behavior changes, so
there is no acceptance-level (`.hurl`) coverage — this change has no
HTTP-observable surface.

**One-time cost when it lands.** The toolchain fingerprint changes once (F7), so
the first `./dev` call in any already-open session re-enters `nix develop`, and
the devShell derivation is rebuilt/substituted once (F8) before the cache warms.

**New, documented extension point for contributors and agents.** Adding the next
non-Haskell CLI tool is now: add a key under `neohaskellTools` in `flake.nix`, list
`pkgs.neohaskellTools.<key>` in `nix/hix.nix` `shell.buildInputs` — with the
Haskell-tool exception (`shell.tools`) written down next to it in `README.md`.

**One new flake attribute.** `legacyPackages.<system>.neohaskellTools` now exists
(holding `bd`). It is purely additive and shadows nothing (C2); `packages`, `apps`
(including `neo`), and `nixConfig` are untouched.

**Follow-ups deliberately not bundled** (to be filed as beads, not fixed here):
`cachix-push.yml`'s path filter missing `flake.nix` (F9); no codemap alias routing
"dev shell"/"dev environment" to `ci-cd`; and a possible
`codemap/extension-points.yaml` row for "add a non-Haskell CLI tool to the
devShell", which today's registry (Haskell-only) has no entry for.

## ADR

[ADR-0075](../decisions/0075-devshell-tool-injection-via-overlay-and-hix-buildinputs.md)
— why `bd` enters through an overlay-injected, namespaced `neohaskellTools` set
plus hix's `shell.buildInputs` rather than `mkShell.inputsFrom`; why
`beads.overlays.default` stays unused; why the attribute is namespaced rather than
bare; and what happens if hix's module `pkgs` turns out not to see our overlay.
Triggered by the "significant new infrastructure" / "changing code style
conventions" entries in the `docs/decisions/README.md` ADR-required list — the
mechanical flags (`breaking` / `new-dependency` / `new-capability` /
`new-extension-point`) are all false, exactly as in change 003.
