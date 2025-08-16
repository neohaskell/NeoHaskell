{ pkgs ? import <nixpkgs> { } }:

let
  hp = ({ pkgs }:

    # Generic function to create a Haskell project template
    { projectName, src }:
    let
      haskellNix = import (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "haskell.nix";
        rev = "4d493449406ec91db804511a6d15b6f076ba40e7";
        sha256 = "sha256-CHNMDgFfpTV5WkVhmMLf5d5qaLUjgeoziVgmgnhPGrI=";
      }) { inherit pkgs; };
      # Apply haskell.nix overlay
      pkgsWithOverlay = import haskellNix.sources.nixpkgs-unstable {
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            "${projectName}Project" = final.haskell-nix.cabalProject {
              inherit src;
              compiler-nix-name = "ghc910";
            };
          })
        ];
        inherit (haskellNix) config;
      };

      flake = pkgsWithOverlay."${projectName}Project".flake { };

    in flake.packages."${projectName}:exe:${projectName}");

in (hp { inherit pkgs; }) {
  projectName = "neo-sandbox";
  src = ./.;
}

