{ pkgs, ... }: {
  name = "neohaskell-starter";
  compiler-nix-name = "ghc98";

  shell.tools = {
    cabal = "latest";
    hlint = "latest";
    fourmolu = "latest";
    hspec-discover = "latest";
    haskell-language-server = "latest";
  };
  shell.buildInputs = with pkgs; [ git postgresql hurl ];
}
