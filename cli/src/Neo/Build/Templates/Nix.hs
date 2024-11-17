module Neo.Build.Templates.Nix (
  template,
) where

import Neo.Core


template :: ProjectConfiguration -> Text
template _ =
  {-
  https://github.com/NixOS/nixpkgs/blob/777a9707e72e6dbbbdf9033c44f237154c64e9f7/pkgs/development/haskell-modules/make-package-set.nix#L227-L254

  Also: https://srid.ca/haskell-nix

  FIXME: Pin GHC to specific version so packages aren't broken

  TODO: Consider dropping developPackage in favor of making the package
  out of neo.json. Although maybe that could lead to IDEs not being able
  to find the source code.
  -}
  [fmt|{{ nixpkgs ? import <nixpkgs> {{}} }}:
  let
    neoHaskellGitHub = builtins.fetchTarball
          "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/dev.tar.gz";
  in
    nixpkgs.haskellPackages.developPackage {{
      root = ./.;
      source-overrides = {{
        nhcore = "${{neoHaskellGitHub}}/core";
      }};
    }}
  |]