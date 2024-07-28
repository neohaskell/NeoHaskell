{ inputs, pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs; [
    git
    ghcid
    haskellPackages.implicit-hie
  ];

  # https://devenv.sh/scripts/
  scripts = {
    watch.exec = "ghcid --command=cabal repl $1";
    build.exec = "cabal build all";
    neo.exec = "cabal run nhcli -- $@";
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
