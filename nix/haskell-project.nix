{ pkgs ? import <nixpkgs> { }
, haskellNix ? import (import ./haskellnix.nix { inherit pkgs; }) { inherit pkgs; } }:

# Generic function to create a Haskell project template
projectName: src:
let
  # Apply haskell.nix overlay
  pkgsWithOverlay = import haskellNix.sources.nixpkgs-unstable {
    overlays = [
      haskellNix.overlay
      (final: _prev: {
        "${projectName}Project" = final.haskell-nix.project' {
          inherit src;
          compiler-nix-name = "ghc910";
        };
      })
    ];
    inherit (haskellNix) config;
  };

  flake = pkgsWithOverlay."${projectName}Project".flake { };

in flake.packages."${projectName}:exe:${projectName}"
