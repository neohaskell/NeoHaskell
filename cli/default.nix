{ pkgs ? import ../nix/nixpkgs.nix { } }:
pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
    nhcore = ../core;
  };
  modifier = drv: pkgs.haskell.lib.dontHaddock drv;
}
