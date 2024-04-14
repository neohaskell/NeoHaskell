{ inputs, pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    # pkgs.haskell.packages.ghc963.cabal-install
    # pkgs.haskell.compiler.ghc963
    # pkgs.haskell.packages.ghc963.haskell-language-server
    # (haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
  ];

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo hello from $GREET";

  enterShell = ''
    hello
    git --version
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
