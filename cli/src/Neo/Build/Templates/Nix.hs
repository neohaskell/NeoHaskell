module Neo.Build.Templates.Nix (
  template,
) where

import Neo.Core


template :: ProjectConfiguration -> Text
template _ =
  {-
  https://github.com/NixOS/nixpkgs/blob/777a9707e72e6dbbbdf9033c44f237154c64e9f7/pkgs/development/haskell-modules/make-package-set.nix#L227-L254
  -}
  [fmt|{{ nixpkgs ? import <nixpkgs> {{}} }}: nixpkgs.haskellPackages.developPackage {{ root = ./.; }}|]
