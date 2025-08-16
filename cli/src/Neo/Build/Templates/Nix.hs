module Neo.Build.Templates.Nix (
  template,
) where

import Neo.Core
import Text qualified


-- FIXME: Use file-embed instead of duplicating this and what's in `nix/nixpkgs.nix`
pinnedNixpkgs :: Text
pinnedNixpkgs =
  [fmt|import (builtins.fetchTarball {
      name = "haskell-fixes";
      url = "https://github.com/nixos/nixpkgs/archive/c95b3e3904d1c3138cafab0ddfbc08336128f664.tar.gz";
      sha256 = "03b5i7almr4v68b677qqnbyvrmqdxq02gks7q1jr6kfm2j51bgw5";
  })
  |]


haskellNix :: Text
haskellNix = [Text.fromStaticFile|./nix/haskellnix.nix|]


template :: ProjectConfiguration -> Text
template ProjectConfiguration {name} =
  [fmt|{ pkgs ? (#{pinnedNixpkgs}) {} }:
  let
    neoHaskellGitHub = builtins.fetchTarball
          "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/main.tar.gz";
  in
    pkgs.haskellPackages.developPackage {
      name = "#{name}";
      root = ./.;
      returnShellEnv = false;
      source-overrides = {
        nhcore = "${neoHaskellGitHub}/core";
      };
    } // { name = "#{name}"; }
  |]
