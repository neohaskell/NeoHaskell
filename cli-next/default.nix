{ pkgs ? import <nixpkgs> { }
, haskellNix ? import ../nix/haskellnix.nix { pkgSet = pkgs; } }:

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
