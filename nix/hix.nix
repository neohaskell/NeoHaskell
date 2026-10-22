{ pkgs, ... }: {
  name = "neohaskell";
  compiler-nix-name = "ghc98";

  modules = [{
    doHaddock = false;
  }];


  shell.tools = {
    cabal = "latest";
    hlint = "latest";
    fourmolu = "latest";
    hspec-discover = "latest";
    haskell-language-server = "latest";
    cabal-gild = "latest";
    ghcid = "latest";
    hiedb = "latest";
    doctest = "latest";
  };
  shell.withHoogle = false;

  shell.shellHook = ''
    _nh_root="''${NEOHASKELL_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
    export NEOHASKELL_ROOT="$_nh_root"
    export NEOHASKELL_SHELL_FP=$("$_nh_root/.agents/skills/neohaskell-best-practices/scripts/toolchain-fp" 2>/dev/null || true)
    if [ -z "$NEOHASKELL_SHELL_FP" ]; then
      echo "with-toolchain: warning — toolchain fingerprint unavailable; fast path disabled (every call re-enters nix develop)" >&2
    fi
    export NEOHASKELL_SHELL_PATH="$PATH"
    unset _nh_root
  '';
  shell.buildInputs = with pkgs; [
    git
    nixfmt-classic
    postgresql
    hurl
    poppler_utils
    (python3.withPackages (ps: [ ps.pyyaml ]))
    haskellPackages.hoogle
  ];
}
