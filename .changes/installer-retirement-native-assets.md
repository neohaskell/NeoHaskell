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

Use the following standalone example from a shell with `curl`, `awk`, and
either `sha256sum` or `shasum` installed. It downloads both files from the
same release, checks the selected asset, installs it, and prints its version.

~~~sh
set -eu
version=0.11.0
target=x86_64-unknown-linux-gnu
download_dir="$HOME/Downloads/neo-$version"
destination="$HOME/.local/bin/neo"
release_url="https://github.com/neohaskell/NeoHaskell/releases/download/neo-v$version"
mkdir -p "$download_dir" "$HOME/.local/bin"
curl -fL "$release_url/neo-$target" -o "$download_dir/neo-$target"
curl -fL "$release_url/SHA256SUMS" -o "$download_dir/SHA256SUMS"
expected=$(awk -v name="neo-$target" '$2 == name {print $1}' "$download_dir/SHA256SUMS")
test -n "$expected"
if command -v sha256sum >/dev/null 2>&1; then
  actual=$(sha256sum "$download_dir/neo-$target" | awk '{print $1}')
else
  actual=$(shasum -a 256 "$download_dir/neo-$target" | awk '{print $1}')
fi
test "$actual" = "$expected"
install -m 0755 "$download_dir/neo-$target" "$destination"
export PATH="$HOME/.local/bin:$PATH"
"$destination" --version
~~~

Use `aarch64-apple-darwin` for an Apple silicon Mac. Add `$HOME/.local/bin`
to your shell configuration to keep `neo` on `PATH` in new sessions. If a
consumer uses a retired native target, flag the need for an owner-selected
installation method instead of proposing an unverified replacement.

Run the installed destination's `--version` command and check that it reports
the chosen release. Preserve unrelated setup behavior. Report changed files,
commands and results, and any script whose owner must choose a different
installation method. Do not restore the bootstrap, publish assets, or assume
old installer binaries are compatible with future releases.
```
