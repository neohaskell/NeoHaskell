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
        ciProject = pkgs.hixProject.appendModule ({ lib, ... }: {
          shell.tools = lib.mkForce {
            cabal = "latest";
            hspec-discover = "latest";
            doctest = "latest";
          };
        });

        neo = pkgs.callPackage ./nix/neo-package.nix { };
        neoctlSource = pkgs.runCommand "neoctl-project" {
          nativeBuildInputs = [ neo ];
        } ''
          cp -R ${./neoctl} "$out"
          chmod -R u+w "$out"
          cd "$out"
          export HOME="$TMPDIR"
          export NEO_SKIP_NETWORK=1
          neo --ci generate
        '';
        neoctlFlake = (import "${neoctlSource}/flake.nix").outputs {
          self = neoctlSource;
          inherit nixpkgs flake-utils haskellNix;
          neohaskell = self;
        };
        neoctl = neoctlFlake.packages.${system}."neoctl:exe:neoctl";
        neo-installer = pkgs.callPackage ./nix/neo-package.nix {
          command = "neo-installer";
        };
        starterLock = builtins.fromJSON (builtins.readFile ./neo/starter/flake.lock);
        starter = (import haskellNix.inputs.flake-compat {
          src = ./neo/starter;
          inherit system;
          override-inputs.neohaskell = builtins.fetchTree starterLock.nodes.neohaskell.locked;
        }).defaultNix;
        neo-toolchain = starter.packages.${system}.neo-toolchain;
        neo-pair = pkgs.symlinkJoin {
          name = "neo-pair-${neo.version}";
          paths = [ neo neoctl ];
          postBuild = ''
            mkdir -p "$out/share/neohaskell"
            ln -s ${neo-toolchain} "$out/share/neohaskell/toolchain"
            test -x "$out/bin/neo"
            test -x "$out/bin/neoctl"
            for installed in "$out/bin/"*; do
              case "$installed" in
                "$out/bin/neo"|"$out/bin/neoctl") ;;
                *) exit 1 ;;
              esac
            done
          '';
        };
      in flake // {
        devShells = flake.devShells // {
          ci = ciProject.shell;
        };
        legacyPackages = pkgs;
        packages = flake.packages // { inherit neo neoctl; }
          // pkgs.lib.optionalAttrs (system != "x86_64-darwin") {
            inherit neo-installer neo-pair neo-toolchain;
          };
        apps = (flake.apps or { }) // {
          neo = {
            type = "app";
            program = "${neo}/bin/neo";
          };
          neoctl = {
            type = "app";
            program = "${neoctl}/bin/neoctl";
          };
        } // pkgs.lib.optionalAttrs (system != "x86_64-darwin") {
          neo-installer = {
            type = "app";
            program = "${neo-installer}/bin/neo-installer";
          };
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # Binary caches for faster builds:
    # - cache.iog.io: IOHK's cache (GHC, haskell.nix infrastructure)
    # - neohaskell.cachix.org: NeoHaskell's cache (project deps, shell tools)
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.iog.io"
      "https://neohaskell.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="
    ];
    allow-import-from-derivation = "true";
  };
}
