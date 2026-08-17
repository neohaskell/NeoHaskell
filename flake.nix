{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # bd (beads) issue tracker — dispatcher setup (S3.1). nixpkgs' own `beads`
  # package is pinned too low (1.0.3; the .beads/formulas/ TOML schema
  # requires bd >= 1.1.0), so this pulls the upstream flake directly and
  # lets flake.lock pin the exact version. Deliberately NOT wired in via
  # `beads.overlays.default` / `nixpkgs.follows`: that overlay calls
  # `final.buildGo126Module`, which only exists on very recent nixpkgs —
  # our own pin (`haskellNix/nixpkgs-unstable`) is older and lacks it, and
  # mixing the overlay into OUR pkgs fails regardless of what beads' own
  # nixpkgs input is set to (an overlay always evaluates against the pkgs
  # it's applied to, not the pkgs it was authored against). Instead this
  # consumes beads' self-contained `packages.${system}.bd` output directly,
  # built entirely against beads' own independently-pinned nixpkgs
  # (nixos-25.11, which does have buildGo126Module) — see the devShells
  # override below.
  inputs.beads.url = "github:gastownhall/beads";
  outputs = { self, nixpkgs, flake-utils, haskellNix, beads }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              # uncomment with your current system for `nix flake show` to work:
              #evalSystem = "x86_64-linux";
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hixProject.flake { };

        # The Rust Neo CLI (neo/) exposed as a first-class monorepo output.
        # Defined in nix/neo-package.nix as the single source of truth; it stays
        # an independent crate (own Cargo.toml/Cargo.lock, own version; no root
        # workspace, no coupling to the NeoHaskell library outputs below).
        neo = pkgs.callPackage ./nix/neo-package.nix { };
      in flake // {
        legacyPackages = pkgs;
        # Merge alongside the existing hix outputs; the default NeoHaskell
        # package/app is untouched; `neo` is purely additive.
        packages = flake.packages // { inherit neo; };
        apps = (flake.apps or { }) // {
          neo = {
            type = "app";
            program = "${neo}/bin/neo";
          };
        };
        # `default` is the shell contributors/agents actually use
        # (scripts/with-toolchain enters it via `nix develop --command`).
        # Extend the hix-generated shell with bd (beads), rather than
        # touching nix/hix.nix's shell.buildInputs: bd comes from a
        # separately-pinned nixpkgs (see the `beads` input comment above),
        # so it is composed in via `mkShell { inputsFrom; }` instead of
        # being folded into the hix project's own pkgs/overlay chain.
        devShells = (flake.devShells or { }) // {
          default = pkgs.mkShell {
            inputsFrom = [ flake.devShells.default ];
            packages = [ beads.packages.${system}.bd ];
          };
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # Binary caches for faster builds:
    # - cache.iog.io: IOHK's cache (GHC, haskell.nix infrastructure)
    # - neohaskell.cachix.org: NeoHaskell's cache (project deps, shell tools)
    extra-substituters = [
      "https://cache.iog.io"
      "https://neohaskell.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="
    ];
    allow-import-from-derivation = "true";
  };
}
