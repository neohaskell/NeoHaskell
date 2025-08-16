module Neo.Build.Templates.Nix (
  template,
) where

import Neo.Core
import Text qualified


haskellProject :: Text
haskellProject = [Text.fromStaticFile|./nix/haskell-project.nix|]


template :: ProjectConfiguration -> Text
template ProjectConfiguration {name} =
  [fmt|{ pkgs ? import <nixpkgs> { } }:

let
  hp = (#{haskellProject});

in (hp { inherit pkgs;  }) {
  projectName = "#{name}";
  src = ./.;
}

  |]
