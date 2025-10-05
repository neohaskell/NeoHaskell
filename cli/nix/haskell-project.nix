{ pkgs }:

{ packages, mainPackageName, executableName }:
let
  haskellNix = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "4d493449406ec91db804511a6d15b6f076ba40e7";
    sha256 = "sha256-CHNMDgFfpTV5WkVhmMLf5d5qaLUjgeoziVgmgnhPGrI=";
  }) { };
  pkgsWithOverlay = import haskellNix.sources.nixpkgs-unstable {
    overlays = [
      haskellNix.overlay
      (final: _prev:
        builtins.listToAttrs (builtins.map (packageName: {
          name = "${packageName}";
          value = final.haskell-nix.cabalProject {
            src = packages.${packageName};
            compiler-nix-name = "ghc910";
          };
        }) (builtins.attrNames packages)))
    ];
    inherit (haskellNix) config;
  };

  pkg = pkgsWithOverlay."${mainPackageName}";

in {
  shell = pkg.shellFor {
    tools = {
      cabal = { };
      hlint = { };
      haskell-language-server = { };
      hspec-discover = { };
      ghcid = { };
      cabal-gild = { };
      fourmolu = { };
    };
    buildInputs = [
      pkgs.nil
      pkgs.nixfmt-classic
      pkgs.nixpkgs-fmt
      pkgs.pkg-config
      pkgs.zlib
    ];
  };
  package = (pkg.flake { }).packages."${executableName}";
}
