{ pkgs ? import <nixpkgs> { }, haskellNix ? import (pkgs.fetchFromGitHub {
  owner = "input-output-hk";
  repo = "haskell.nix";
  # Pin to a recent commit - you may want to update this
  rev = "4d493449406ec91db804511a6d15b6f076ba40e7"; # or specific commit hash
  sha256 = "sha256-CHNMDgFfpTV5WkVhmMLf5d5qaLUjgeoziVgmgnhPGrI=";
}) { pkgSet = pkgs; } }:

let
  # Apply haskell.nix overlay
  pkgsWithOverlay = import haskellNix.sources.nixpkgs-unstable {
    overlays = [
      haskellNix.overlay
      (final: _prev: {
        neo-sandboxProject = final.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc910";
        };
      })
    ];
    inherit (haskellNix) config;
  };

  flake = pkgsWithOverlay.neo-sandboxProject.flake { };

in flake.packages."neo-sandbox:exe:neo-sandbox"

