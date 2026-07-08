{ pkgs, ... }: {
  name = "neohaskell";
  compiler-nix-name = "ghc98"; # Version of GHC to use

  # Disable haddock builds for faster CI
  modules = [{
    doHaddock = false;
  }];

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
    cabal-gild = "latest";
    ghcid = "latest"; # powers scripts/dev-loop (agent + human inner loop)
    hiedb = "latest"; # symbol reference DB — powers ./dev who-calls (codemap)
    doctest = "latest"; # doctest gate (./dev doctest; CI: test.yml doctest
    # job) — MUST be a shell.tool, not a nixpkgs buildInput: doctest links
    # the GHC lib API and only works built against the project's own GHC.
  };
  # Kill haskell.nix's hoogle-with-packages wrapper: it injects its own
  # --database flag, silently overriding ./dev api's databases (root cause of
  # "--local ignored", diagnosed 2026-07-08). We run the raw nixpkgs binary.
  shell.withHoogle = false;

  # Fingerprint for scripts/with-toolchain's verified fast path: commands run
  # directly (no nested `nix develop`) only when this matches the current
  # hash of the shell-defining files. scripts/toolchain-fp = single source.
  shell.shellHook = ''
    export NEOHASKELL_SHELL_FP=$(scripts/toolchain-fp 2>/dev/null || true)
  '';
  shell.buildInputs = with pkgs; [
    git
    nixfmt-classic
    postgresql
    hurl
    poppler_utils
    (python3.withPackages (ps: [ ps.pyyaml ])) # codemap tooling (check.py)
    # nixpkgs' hoogle build — NOT shell.tools: the haskell.nix-built hoogle
    # 5.0.18.4 silently ignores --local=DIR txt dirs (verified 2026-07-08);
    # the nixpkgs build of the same version indexes them correctly.
    haskellPackages.hoogle
  ];
}
