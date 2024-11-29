{pkgs ? import ../nix/nixpkgs.nix {} }:
pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
  };
}
