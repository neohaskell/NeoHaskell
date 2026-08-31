# Canonical Nix packaging for the Rust Neo CLI that lives at `neo/`.
#
# This is the ONE source of truth for building `neo` as a first-class monorepo
# output. The repo-root `flake.nix` calls it to expose `packages.neo` and
# `apps.neo`; a component CI workflow (`.github/workflows/neo-ci.yml`) builds it
# on Linux and macOS.
#
# Crate-boundary invariants (do not break):
#   - `src` is scoped to `neo/` only. There is NO root Cargo workspace; the
#     NeoHaskell libraries and the Neo CLI stay independent crates.
#   - `version` is read from `neo/Cargo.toml`, so the Neo CLI release train is
#     never coupled to the NeoHaskell library version.
#   - dependencies are vendored from the pinned `neo/Cargo.lock`, so the build
#     is reproducible and offline.
#
# IDE frontend (rust-embed) contract:
#   `src/commands/ide.rs` embeds `assets/ide/dist/` at compile time. That built
#   tree is a committed part of the source (see `neo/assets/ide/.gitignore`).
#   This derivation therefore compiles the frontend that ships in the binary
#   from the committed `dist/`. To keep that from silently going stale, the
#   committed bundle is proven against a from-lockfile rebuild by
#   `./dev neo-dist-check` (run in `neo-ci.yml` and locally) and the frontend is
#   rebuilt + diffed on every IDE-touching PR. The Nix build stays offline; the
#   freshness proof lives in the CI/`./dev` contract, not in an unverified blob.
#
# Embedded starter (rust-embed) contract:
#   `src/network.rs` embeds `neo/starter/` at compile time so `neo new` scaffolds
#   projects offline, with no runtime download. Because `src` below includes the
#   whole `neo/` tree, any edit under `neo/starter/` changes the derivation hash
#   and rebuilds the binary — the installed `neo` is always revision-coherent
#   with the internalized starter. The sealed `doCheck` unit tests exercise
#   `write_starter_template` into a temp dir, so the package build itself proves
#   the release binary can generate a project with the network disabled.
{ lib
, rustPlatform
, git
, nix
}:

let
  cargoToml = lib.importTOML ../neo/Cargo.toml;

  # Only the crate sources. Drop build outputs and scratch dirs so the hash is
  # stable and the sandbox copy is small. `assets/ide/dist/` is KEPT on purpose
  # (rust-embed needs it); `assets/ide/node_modules` is dropped.
  src = lib.cleanSourceWith {
    src = ../neo;
    filter = path: type:
      let base = baseNameOf path;
      in !(builtins.elem base [
        "target"
        "result"
        "node_modules"
        ".git"
      ]);
  };
in
rustPlatform.buildRustPackage {
  pname = "neo";
  version = cargoToml.package.version;
  inherit src;

  cargoLock = {
    lockFile = ../neo/Cargo.lock;

    # nixpkgs' pinned importCargoLock still defaults to crates.io's API download
    # endpoint, which can reject unauthenticated fixed-output fetches with 403.
    # Override that registry's download base with crates.io's canonical static
    # host; importCargoLock keeps verifying every tarball against Cargo.lock.
    extraRegistries."https://github.com/rust-lang/crates.io-index" =
      "https://static.crates.io/crates";
  };

  # importCargoLock emits an extra Cargo source stanza for every override. Since
  # this override replaces crates-io itself, that stanza aliases the built-in
  # source and Cargo rejects it as a duplicate. The vendored-sources stanza is
  # already authoritative, so remove only the redundant alias after setup.
  preConfigure = ''
    sed -i '/^\[source\."https:\/\/github\.com\/rust-lang\/crates\.io-index"\]$/,+2d' \
      "$NIX_BUILD_TOP/.cargo/config.toml"
  '';

  # Package check = the in-crate unit tests. `neo` is a binary crate, so unit
  # tests are `--bins` (never `--lib`). The integration/e2e suites talk to real
  # nix + network and are deliberately EXCLUDED from the sealed package build;
  # they run in their own CI jobs, not here.
  doCheck = true;
  cargoTestFlags = [ "--bins" ];

  # Some in-crate tests shell out to `git` (repo init, `git status`, the locking
  # system) and probe for `nix` on PATH (the prereqs guard). None of them touch
  # the network (`test_dispatch_new` sets NEO_SKIP_NETWORK=1). Per the neo
  # strict-assertion contract we FIX THE ENVIRONMENT rather than weaken those
  # assertions: supply the two binaries the sealed sandbox otherwise lacks.
  nativeCheckInputs = [ git nix ];

  # `ide::methods::heal_event_model` is the one test module that EXECUTES a
  # written stub script whose shebang is `#!/usr/bin/env bash`. The hermetic
  # Linux build sandbox has no `/usr/bin/env`, so those subprocess-spawning
  # tests cannot run sealed (they pass on the looser macOS sandbox, which would
  # make the package check platform-dependent). They are integration-flavored
  # (spawn a real `claude` subprocess), so per the same rule that keeps
  # integration/e2e out of the sealed build, they are scoped out HERE and run in
  # full by the neo-ci.yml `rust` job (the complete binary suite in the dev
  # shell, where `/usr/bin/env` exists). This scopes the sealed build to the
  # hermetic tests; it does not weaken any assertion.
  checkFlags = [ "--skip=ide::methods::heal_event_model" ];

  meta = {
    description = "The NeoHaskell CLI (scaffold, build, run and test NeoHaskell projects)";
    mainProgram = "neo";
    license = lib.licenses.mit;
  };
}
