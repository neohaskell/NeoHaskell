module Neo.Build.Templates.Nix (
  template,
) where

import Neo.Core


haskellProject :: Text
haskellProject = panic "FIXME: Should use the new flake"


template :: ProjectConfiguration -> Text
template ProjectConfiguration {name} =
  [fmt|{ pkgs ? import <nixpkgs> { } }:

let
  hp = (#{haskellProject});

in (hp { inherit pkgs;  }) {
  packages = {
    "#{name}" = ./.;
  };
  mainPackageName = "#{name}";
  executableName = "#{name}:exe:#{name}";
}

  |]
