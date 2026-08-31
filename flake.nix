{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
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
