# ADR-0075: Non-Haskell devShell tools enter through a namespaced overlay attribute and hix's `shell.buildInputs`

## Status

Accepted

## Context

`bd` (beads) is a Go binary that the dispatcher, the agents, and every
contributor need on `PATH` inside `nix develop`. It cannot come from hix's
`shell.tools`: those are resolved by haskell.nix against the project's own GHC
(the reason `doctest` is pinned there — `nix/hix.nix:28-30`). It also cannot come
from `beads.overlays.default`: that overlay calls `final.buildGo126Module`, which
our pin (`haskellNix/nixpkgs-unstable`) does not have, and an overlay always
evaluates against the pkgs it is applied to, not the pkgs it was authored
against. So `bd` is consumed as beads' self-contained `packages.${system}.bd`,
built entirely against beads' own nixpkgs.

The first wiring (`flake.nix:63-71`) got it into the shell by *replacing* hix's
devShell:

```nix
devShells = (flake.devShells or { }) // {
  default = pkgs.mkShell {
    inputsFrom = [ flake.devShells.default ];
    packages = [ beads.packages.${system}.bd ];
  };
};
```

`inputsFrom` is a build-environment composition trick — it re-derives a shell
from another shell's inputs. Issue #811 asks for the extension point hix actually
documents for "add one more CLI tool", on the grounds that the composition trick
is fragile across hix/haskell.nix bumps, and asks that the procedure for the
*next* such tool be written down. This decision covers the shape of that
extension point. The change itself is specified in
[docs/changes/007-bd-via-hix-shell-extension-point.md](../changes/007-bd-via-hix-shell-extension-point.md).

## Decision

**One overlay injects one namespaced attribute set; hix's own `shell.buildInputs`
consumes it.**

```nix
# flake.nix — in the `overlays` list
(final: _prev: { neohaskellTools = { bd = beads.packages.${system}.bd; }; })

# nix/hix.nix — shell.buildInputs
shell.buildInputs = with pkgs; [ git nixfmt-classic … neohaskellTools.bd ];
```

and the `devShells` override block in `flake.nix` is deleted, so
`devShells.default` is hix's shell again, unwrapped.

Three sub-decisions, each of which was the alternative worth arguing about:

**1. hix's `shell.buildInputs`, not a cleverer `flake.nix`-level composition.**
The rejected alternative — `flake.devShells.default.overrideAttrs`, keeping the
fix inside `flake.nix` — satisfies the letter of AC1 ("available without relying
on `mkShell.inputsFrom`") by swapping one composition trick for another. Using
hix's documented extension point satisfies it structurally: the shell is
*declared* with `bd` in it rather than post-processed, which is what makes the
wiring survive a hix bump, and it gives AC3's recipe a real second step. The cost
is that `nix/hix.nix` is now part of the edit set, which is accepted.

**2. A namespaced `neohaskellTools` set, not a bare `bd` attribute.** A bare
overlay binding would shadow any same-named nixpkgs attribute for everything
built from our pkgs — including `legacyPackages.<system>.bd` — with nothing in
the diff to show it. Today's pin has no `bd` attribute, so the collision is
latent rather than present, which is exactly the failure that surfaces a year
later during an unrelated bump. The namespaced set also gives the AC3 recipe an
obvious home for the next tool and keeps the added surface on `legacyPackages`
down to one name. In the spec's vocabulary, `neohaskellTools` **is** the
primitive: the single named door through which non-Haskell CLI tools enter the
dev environment.

**3. `beads.overlays.default` stays unused, permanently.** Not a temporary
workaround pending a nixpkgs bump: any wiring that reintroduces it fails
evaluation on every system with a missing `buildGo126Module`, and is out of
contract by intake. `bd`'s version stays whatever `flake.lock` pins — this change
bumps nothing.

**If hix's module `pkgs` turns out not to see our overlay** (evaluation fails with
`attribute 'neohaskellTools' missing`, because haskell.nix hands the module a
differently-pinned pkgs), the run **parks as `wrong-localization` and re-enters at
intake**, where the `overrideAttrs` alternative gets weighed on its merits. It does
not silently degrade into the `flake.nix`-only fix — the whole point of the
decision is which extension point is used, so quietly changing that answer while
keeping the issue closed would make the record lie.

## Consequences

**Positive.**

- The devShell is declared in one place again (`nix/hix.nix`), the way hix
  intends; `flake.nix` composes no shell at all.
- Adding the next non-Haskell CLI tool is a documented two-step edit (a key under
  `neohaskellTools`, an entry in `shell.buildInputs`) rather than a fresh
  invention, with the Haskell-tool exception (`shell.tools`) written next to it.
- The wiring becomes statically checkable: `./dev devshell-check --self-test`
  fails if `inputsFrom` returns, if the overlay binds a bare attribute, if `bd` is
  pointed at nixpkgs' `beads` (pinned 1.0.3, below the `.beads/formulas/*.toml`
  schema floor of 1.1.0), or if the README recipe drifts from the attribute name
  in the code.

**Negative / accepted.**

- `legacyPackages.<system>.neohaskellTools` is a new public flake attribute. It is
  additive and shadows nothing, but it is reachable from a `nix flake show`-style
  walk, which can force `beads.packages.${system}.bd` for a system beads may not
  publish — the same failure as today's `flake.nix:69`, moved rather than widened.
- The toolchain fingerprint (`scripts/toolchain-fp`, which hashes `flake.nix` and
  `nix/*.nix`) changes value once, so every open shell re-enters `nix develop`
  once, and the devShell derivation is rebuilt/substituted once before the cache
  warms. The fingerprint *mechanism* is untouched.
- Only two of the four supported systems are exercised by CI (`x86_64-linux`,
  `aarch64-darwin`). The wiring is uniform inside `flake-utils.lib.eachSystem`
  with no system-conditional, which is an argument for the other two, not a test —
  a pre-existing limit this decision does not change.
