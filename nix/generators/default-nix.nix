{ pkgs }:
{ neoConfig }:
let
  inherit (neoConfig) name;
  
  # Use the same pinned nixpkgs as the main repo
  pinnedNixpkgs = ''
    import (builtins.fetchTarball {
      name = "nixpkgs-054144e5";
      url = "https://github.com/nixos/nixpkgs/archive/054144e516ca1e3cf5706d5c3427d350eaa107af.tar.gz";
      sha256 = "0ixd2mc9kkmr8mz20ah2j7akgwllxsjv94ah895zgi9b9q1h25ka";
    })
  '';
  
in ''
{ pkgs ? (${pinnedNixpkgs}) {} }:
let
  neoHaskellGitHub = builtins.fetchTarball
        "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/main.tar.gz";
in
  pkgs.haskellPackages.developPackage {
    name = "${name}";
    root = ./.;
    returnShellEnv = false;
    source-overrides = {
      nhcore = "''${neoHaskellGitHub}/core";
    };
  } // { name = "${name}"; }
''