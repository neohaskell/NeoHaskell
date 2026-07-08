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
  # Path is repo-root-anchored (a cwd-relative path breaks when nix develop
  # is entered from a subdirectory); an empty FP is warned about, not
  # swallowed — it silently disables the fast path.
  # NEOHASKELL_SHELL_PATH is the PATH WITNESS: with-toolchain restores it on
  # the fast path so later PATH mutations in the session (tmux panes, rc
  # files) cannot swap in host binaries behind a matching fingerprint.
  shell.shellHook = ''
    _nh_root=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
    export NEOHASKELL_SHELL_FP=$("$_nh_root/scripts/toolchain-fp" 2>/dev/null || true)
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
    (python3.withPackages (ps: [ ps.pyyaml ])) # codemap tooling (check.py)
    # nixpkgs' hoogle build — NOT shell.tools: the haskell.nix-built hoogle
    # 5.0.18.4 silently ignores --local=DIR txt dirs (verified 2026-07-08);
    # the nixpkgs build of the same version indexes them correctly.
    haskellPackages.hoogle
  ];
}
