{ pkgs ? import <nixpkgs> { } }:

(import ../nix/haskell-project.nix { inherit pkgs; }) {
  projectName = "neo-sandbox";
  src = ./.;
}
