{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  # Tracks `main`; the actual rev is pinned by the committed flake.lock
  # (currently 3fd3d1327c). To bump: `nix flake lock --update-input neohaskell`
  # then sync the `tag:` in cabal.project and `neohaskellCommit` below.
  inputs.neohaskell.url = "git+https://github.com/neohaskell/neohaskell.git?ref=main";
  inputs.neohaskell.flake = false;

  outputs = { self, nixpkgs, flake-utils, haskellNix, neohaskell }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      # Must match the `tag:` in cabal.project for both subdirs AND the rev in the neohaskell input above.
      neohaskellCommit = "3fd3d1327c3f64c0e6ded278b803b1a2b96ab875";

    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              inputMap = {
                "https://github.com/neohaskell/neohaskell.git/${neohaskellCommit}" = neohaskell;
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hixProject.flake { };
      in flake // { legacyPackages = pkgs; });

  nixConfig = {
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
