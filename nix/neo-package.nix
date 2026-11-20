# Canonical Nix packaging for the Rust commands under `neo/`.
#
# Shared filtering and vendoring keep both commands on the same Rust source tree.
#
# Cargo boundaries:
#   - `src` stays scoped to `neo/`. The nested lifecycle package remains inside
#     this source boundary and shares the one `neo/Cargo.lock`.
#   - `neo/Cargo.toml` keeps the canonical product version. The lifecycle
#     package uses an internal crate version and derives product identity from
#     the parent manifest at build time.
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
, stdenv
, rustPlatform
, git
, nix
, bash
, command ? "neo"
}:

let
  cargoToml = lib.importTOML ../neo/Cargo.toml;
  isInstaller = command == "neo-installer";
  cargoPackage = if isInstaller then "neo-lifecycle" else "neo";

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
assert builtins.elem command [ "neo" "neo-installer" ];
rustPlatform.buildRustPackage {
  pname = command;
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

  cargoBuildFlags = [ "--package" cargoPackage "--bin" command ];

  postInstall = ''
    test -x "$out/bin/${command}"
    for installed in "$out/bin/"*; do
      test "$installed" = "$out/bin/${command}"
    done
  '';

  postFixup = lib.optionalString (isInstaller && stdenv.hostPlatform.isDarwin) ''
    /usr/bin/codesign --force --sign - --timestamp=none --options runtime,library \
      --identifier org.neohaskell.neo-installer "$out/bin/${command}"
    /usr/bin/codesign --verify --strict "$out/bin/${command}"
  '';

  doCheck = true;
  # Lifecycle tests fork workers and observe the release of inherited file locks.
  dontUseCargoParallelTests = true;
  cargoTestFlags =
    (if isInstaller then [ "--package" cargoPackage ] else [ "--workspace" ])
    ++ [ "--lib" "--bins" ];

  # In-crate tests need Git, Nix, and a POSIX shell for immutable fixture
  # commands. None of them touch the network (`test_dispatch_new` sets
  # NEO_SKIP_NETWORK=1).
  nativeCheckInputs = [ bash git nix ];

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
  checkFlags = [
    "--skip=ide::methods::heal_event_model"
  ] ++ lib.optionals stdenv.hostPlatform.isLinux [
    # Nix user namespaces expose root-owned ancestors as unmapped UID 65534.
    # Host CI retains the diagnostic ownership, ACL and publication tests.
    "--skip=diagnostics::store::tests::setgid_parent_keeps_owner_only_access"
    "--skip=diagnostics::store::tests::begin_and_finish_keep_one_name_and_replace_the_inode"
    "--skip=diagnostics::store::tests::retention_uses_exact_millisecond_boundary_and_keeps_future_records"
    "--skip=diagnostics::store::tests::full_store_reserves_stage_bytes_and_evicts_oldest_only"
    "--skip=diagnostics::store::tests::foreign_entries_survive_and_overflow_refuses_before_pruning"
    "--skip=diagnostics::store::tests::corrupt_reserved_header_prevents_all_pruning"
    "--skip=diagnostics::store::tests::hardlinked_report_is_neither_read_as_owned_nor_removed"
    "--skip=diagnostics::store::tests::symlink_and_fifo_reserved_entries_refuse_without_following_or_blocking"
    "--skip=diagnostics::store::tests::malformed_lock_is_preserved"
    "--skip=diagnostics::store::tests::lock_contention_declines_and_keeps_stable_inode"
    "--skip=diagnostics::store::tests::stale_empty_stage_is_removed_only_after_whole_inventory_validation"
    "--skip=diagnostics::store::tests::finish_never_resurrects_deleted_started_record_and_export_survives"
    "--skip=diagnostics::store::tests::uninstall_clears_owned_reports_and_staging_but_preserves_exports"
    "--skip=diagnostics::store::tests::uninstall_preserves_reserved_foreign_file_and_reports_on_inventory_failure"
    "--skip=diagnostics::store::tests::moved_store_refuses_a_misleading_report_locator"
    "--skip=diagnostics::store::tests::wrong_modes_and_symlinked_parents_are_not_repaired"
    "--skip=diagnostics::store::tests::linux_default_acl_refuses_before_creating_store"
    "--skip=diagnostics::store::tests::process_exit_at_publication_cuts_leaves_a_recoverable_store"
    # Host shell locations and startup files are unavailable in the sealed build.
    "--skip=installation::process::shell_startup::tests::interactive_startup_has_a_controlling_terminal_and_noise_is_discarded"
    "--skip=installation::process::shell_startup::tests::qualified_lookup_and_pinned_versions_in_all_startup_modes"
    "--skip=installation::process::shell_startup::tests::startup_timeout_and_cancellation_reap_the_shell_group"
    # Sealed Nix returns EOPNOTSUPP for flistxattr. Profile exchange requires readable metadata.
    "--skip=installation::shell::removal::tests::removal_preserves_prefix_suffix_and_completed_retry"
    "--skip=installation::shell::removal::tests::removal_refuses_editor_replacement_before_exchange"
    "--skip=installation::shell::removal::tests::removal_retains_late_editor_writes_and_refuses_unknown_retry"
  ];

  meta = {
    description = if isInstaller
      then "The native NeoHaskell installer"
      else "The NeoHaskell CLI (scaffold, build, run and test NeoHaskell projects)";
    mainProgram = command;
    license = lib.licenses.mit;
  };
}
