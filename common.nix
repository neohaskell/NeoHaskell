{ pkgs ? import <nixpkgs> { } }:

let hp = import ./cli/nix/haskell-project.nix { inherit pkgs; };
in (hp {
  packages = {
    "nhcore" = ./.;
    "nhcli" = ./.;
  };

  mainPackageName = "nhcli";
  executableName = "nhcli:exe:neo";
})
