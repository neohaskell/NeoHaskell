{ pkgs ? import ./nix/nixpkgs.nix { } }:
pkgs.mkShell rec {
  buildInputs = [
    # Haskell dev tools
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server
    pkgs.fourmolu
    pkgs.hlint
    pkgs.haskellPackages.zlib

    # Nix dev tools
    pkgs.nil
    pkgs.nixfmt-classic
    pkgs.nixpkgs-fmt

    # Required native libs
    pkgs.pkg-config
    pkgs.zlib
  ];

  # Required for cabal to find the location of zlib and other native libraries
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
}
