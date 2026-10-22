---
group: platform
component: CLI
impact: breaking
category: Breaking changes
---

## Summary

NeoHaskell no longer publishes the standalone `neo-install` bootstrap. The
public `bootstrap.sh` URL stops working, and future Neo releases no longer
include installer assets. Future Neo releases also stop producing native CLI
assets for Linux ARM and Intel macOS. Historical releases and their assets
remain unchanged. New Neo CLI assets remain available for Linux x86-64 and
Apple silicon macOS.

## Migration

Remove scripts that call
`https://raw.githubusercontent.com/neohaskell/NeoHaskell/main/installer/scripts/bootstrap.sh`.
Existing copies of the old installer have no guarantee of working with future
Neo releases.

Download `neo-<target>` and `SHA256SUMS` from the same `neo-vX.Y.Z` release. The
new native targets are `x86_64-unknown-linux-gnu` and `aarch64-apple-darwin`.
New assets are no longer produced for `aarch64-unknown-linux-gnu` or
`x86_64-apple-darwin`; historical release assets remain unchanged. No
replacement installation method is defined here for those platforms. Compare
the downloaded asset's SHA-256 with its line in `SHA256SUMS`. From a NeoHaskell
checkout, the verified release helper checks the selected asset and installs
it atomically:

```sh
mkdir -p "$HOME/.local/bin"
./.agents/skills/neohaskell-release/scripts/neo-release install \
  "$HOME/Downloads/neo-vX.Y.Z" x86_64-unknown-linux-gnu \
  "$HOME/.local/bin/neo"
"$HOME/.local/bin/neo" --version
```

Use `aarch64-apple-darwin` on an Apple silicon Mac. The example invokes the
installed destination directly. Add `$HOME/.local/bin` to `PATH` to run `neo`
by name. Confirm the binary reports the version you downloaded. Nix setup for
building and running generated projects remains a separate prerequisite; use
the existing [Nix installation instructions](https://nixos.org/download).

### Verify

Confirm that the release directory contains the selected `neo-<target>` asset
and `SHA256SUMS`. Install it with the verified helper, then run the installed
destination's `--version` command and confirm it prints the selected release
version. Keep project build and run verification unchanged after installation.

## Agent prompt

```text
Migrate uses of the retired NeoHaskell installer to direct Neo CLI release
assets. The public bootstrap path
https://raw.githubusercontent.com/neohaskell/NeoHaskell/main/installer/scripts/bootstrap.sh
and the standalone installer release assets are retired. New Neo CLI assets are
no longer produced for `aarch64-unknown-linux-gnu` or `x86_64-apple-darwin`.
Historical release assets remain unchanged. Existing installer binaries have
no guarantee of working with future Neo releases.

Find scripts and setup instructions that call the bootstrap URL. Replace that
step with a download of `neo-<target>` and `SHA256SUMS` from the same
`neo-vX.Y.Z` release. The supported targets are `x86_64-unknown-linux-gnu`
and `aarch64-apple-darwin`. Verify the selected asset against the manifest
with the platform checksum tool before placing it on PATH. The verified
repository helper is `.agents/skills/neohaskell-release/scripts/neo-release`;
its `install <dir> <target> <dest>` command verifies the bytes during atomic
installation. Preserve the existing external Nix setup for generated projects.

The example's installed destination may need to be added to `PATH`. If a
consumer uses a retired native target, flag the need for an owner-selected
installation method instead of proposing an unverified replacement.

Run the installed destination's `--version` command and check that it reports
the chosen release. Preserve unrelated setup behavior. Report changed files,
commands and results, and any script whose owner must choose a different
installation method. Do not restore the bootstrap, publish assets, or assume
old installer binaries are compatible with future releases.
```
