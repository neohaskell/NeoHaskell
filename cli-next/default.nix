{ pkgs ? import <nixpkgs> { } }:

let
  createHaskellProject = import ../nix/haskell-project.nix { inherit pkgs; };

in createHaskellProject "neo-sandbox" ./.
