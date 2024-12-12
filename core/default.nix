{ pkgs ? import ../nix/nixpkgs.nix { } }:
pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = { };
  modifier = drv: pkgs.haskell.lib.dontHaddock drv;
}
