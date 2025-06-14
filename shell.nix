{ pkgs ? import ./nix/nixpkgs.nix { } }:
let
  nix-pre-commit-hooks = import (builtins.fetchTarball
    "https://github.com/cachix/git-hooks.nix/tarball/master");
in let
  pre-commit = {
    # Configured with the module options defined in `modules/pre-commit.nix`:
    pre-commit-check = nix-pre-commit-hooks.run {
      src = ./.;
      # If your hooks are intrusive, avoid running on each commit with a default_states like this:
      # default_stages = ["manual" "pre-push"];
      hooks = { fourmolu.enable = true; };
    };
  };
in pkgs.mkShell rec {
  buildInputs = [
    # Haskell dev tools
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server
    pkgs.fourmolu
    pkgs.hlint
    pkgs.haskellPackages.zlib
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.doctest

    # Nix dev tools
    pkgs.nil
    pkgs.nixfmt-classic
    pkgs.nixpkgs-fmt

    # Required native libs
    pkgs.pkg-config
    pkgs.zlib
  ] ++ pre-commit.pre-commit-check.enabledPackages;

  shellHook = ''
    unset TEMP TMP TEMPDIR TMPDIR   # Required for nix-shell to work
    ${pre-commit.pre-commit-check.shellHook}

    run-doctests-verbose() {
      cabal repl --with-compiler=doctest --repl-options=--verbose core
    }
    
    run-doctests() {
      cabal repl --with-compiler=doctest core
    }
  '';

  # Required for cabal to find the location of zlib and other native libraries
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
}
