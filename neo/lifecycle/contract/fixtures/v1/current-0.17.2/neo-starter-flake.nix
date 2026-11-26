{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";


  inputs.neohaskell.url = "git+https://github.com/neohaskell/neohaskell.git?rev=ea75873fbd0da7da39124c82f328c2e70bdca629";
  inputs.neohaskell.flake = false;

  outputs = { self, nixpkgs, flake-utils, haskellNix, neohaskell }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      neohaskellCommit = "ea75873fbd0da7da39124c82f328c2e70bdca629";

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
        toolchain = import ./nix/toolchain.nix {
          inherit pkgs;
          frameworkSource = neohaskell;
          frameworkRevision = neohaskellCommit;
        };
        flake = pkgs.lib.optionalAttrs (builtins.pathExists ./neo.json)
          (pkgs.hixProject.flake { });
      in flake // {
        legacyPackages = pkgs;
        packages = (flake.packages or { }) // pkgs.lib.optionalAttrs
          (system != "x86_64-darwin") { neo-toolchain = toolchain.bundle; };
      });

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
