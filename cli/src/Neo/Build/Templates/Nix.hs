module Neo.Build.Templates.Nix where

import Neo.Core


template :: ProjectConfiguration -> Text
template _ =
    -- FIXME: inflect properly the name of the project in the different places of the nix file
    [fmt|{{ inputs, pkgs, ... }}:

{{
  packages = with pkgs; [
    haskellPackages.implicit-hie
    haskellPackages.doctest
  ];
  languages.haskell.enable = true;
}}
  |]
