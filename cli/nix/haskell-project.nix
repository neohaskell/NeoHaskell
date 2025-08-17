{ pkgs }:

# Generic function to create a Haskell project template
{ packages, mainPackageName, executableName }:
let
  haskellNix = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "4d493449406ec91db804511a6d15b6f076ba40e7";
    sha256 = "sha256-CHNMDgFfpTV5WkVhmMLf5d5qaLUjgeoziVgmgnhPGrI=";
  }) { inherit pkgs; };
  # Apply haskell.nix overlay
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

  flake = pkgsWithOverlay."${mainPackageName}".flake { };

in flake.packages."${executableName}"
