{ pkgs ? import ../nix/nixpkgs.nix { } }:
let nixpkgs = import ../nix/nixpkgs.nix { };
in nixpkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = { };
  modifier = drv:
    nixpkgs.haskell.lib.dontCheck (nixpkgs.haskell.lib.dontHaddock drv);
}
