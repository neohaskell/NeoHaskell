{ pkgs }:

{ packages, mainPackageName, executableName }:
let
  haskellNix = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "8244bb25bacd06f80fb7d79537eede0d6449faf6";
    sha256 = "sha256-Nx7bintaRzBarcV3S92xw5P68CdE+9/KkwjWibThd/M=";
  }) { };
  pkgsWithOverlay = import haskellNix.sources.nixpkgs-unstable {
    overlays = [
      haskellNix.overlay
      (final: _prev:
        builtins.listToAttrs (builtins.map (packageName: {
          name = "${packageName}";
          value = final.haskell-nix.cabalProject {
            src = packages.${packageName};
            compiler-nix-name = "ghc98";
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
