{ pkgs, ... }: {
  name = "neohaskell";
  compiler-nix-name = "ghc910"; # Version of GHC to use

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell.tools = {
    cabal = "latest";
    hlint = "latest";
    fourmolu = "latest";
    hspec-discover = "latest";
    haskell-language-server = "latest";
  };
  shell.buildInputs = with pkgs; [ git nixfmt-classic ];
}
