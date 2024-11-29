module Neo.Build.Templates.Nix (
  template,
) where

import Neo.Core


-- FIXME: Use file-embed instead of duplicating this and what's in `nix/nixpkgs.nix`
pinnedNixpkgs :: Text
pinnedNixpkgs = 
  [fmt|import (builtins.fetchTarball {{
      name = "haskell-fixes";
      url = "https://github.com/nixos/nixpkgs/archive/c95b3e3904d1c3138cafab0ddfbc08336128f664.tar.gz";
      sha256 = "03b5i7almr4v68b677qqnbyvrmqdxq02gks7q1jr6kfm2j51bgw5";
  }})
  |]


template :: ProjectConfiguration -> Text
template _ =
  {-
  https://github.com/NixOS/nixpkgs/blob/777a9707e72e6dbbbdf9033c44f237154c64e9f7/pkgs/development/haskell-modules/make-package-set.nix#L227-L254

  Also: https://srid.ca/haskell-nix

  FIXME: Pin GHC to specific version so packages aren't broken

  TODO: Consider dropping developPackage in favor of making the package
  out of neo.json. Although maybe that could lead to IDEs not being able
  to find the source code.

  TODO: Figure out how to do caching here
  -}
  [fmt|{{ pkgs ? ({pinnedNixpkgs}) {{}} }}:
  let
    neoHaskellGitHub = builtins.fetchTarball
          "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/dev.tar.gz";
  in
    pkgs.haskellPackages.developPackage {{
      root = ./.;
      source-overrides = {{
        nhcore = "${{neoHaskellGitHub}}/core";
      }};
    }}
  |]
