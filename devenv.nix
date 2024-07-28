{ inputs, pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs; [
    git
    ghcid
    haskellPackages.implicit-hie
    haskellPackages.doctest
  ];

  # https://devenv.sh/scripts/
  scripts = {
    run-watch.exec = "ghcid --command=cabal repl $1";
    run-build.exec = "cabal build all";
    run-update.exec = "cabal update";
    run-cli.exec = "cabal run nhcli -- $@";
    run-test.exec = ''
      cabal repl nhcore --with-ghc=doctest
      cabal repl nhcli --with-ghc=doctest
    '';
  };

  enterShell = ''
    gen-hie > hie.yaml
  '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;
  languages.haskell.enable = true;
  # languages.haskell.package = pkgs.haskell.compiler.ghc92;

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
