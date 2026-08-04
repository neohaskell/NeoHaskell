#!/usr/bin/env bash
# scripts/build.sh — full pipeline: frontend (TS/Vite) then backend (Rust/cargo).
#
# Run from anywhere, in or out of `nix develop`: when invoked outside the
# dev shell, the script re-execs itself via `nix develop --command` so the
# nix-pinned toolchain (with macOS `-liconv` / `-lSystem` paths set up
# correctly) is always the one that compiles.
#
# Idempotent:
#   - `npm install` runs only when `assets/ide/node_modules` is absent.
#   - `npm run build` always runs (cheap when nothing changed; Vite caches).
#   - `cargo build` always runs (cargo's own incremental compile is the brake).
#
# Forwards extra args to cargo, so e.g. `scripts/build.sh --release` works.

set -euo pipefail

# repo root = parent of this script's dir
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# If we're outside `nix develop`, re-exec ourselves inside it. The dev
# shell sets `IN_NIX_SHELL` ("impure" or "pure"); presence (not value) is
# what we key on. Without this guard, a host `cargo` shadows the nix one
# and the link step fails with `ld: library not found for -liconv` on
# macOS (Homebrew clang can't find the iconv that nix's `darwin.libiconv`
# would have provided).
if [ -z "${IN_NIX_SHELL:-}" ]; then
    if ! command -v nix >/dev/null 2>&1; then
        echo "[error] not inside \`nix develop\` and \`nix\` is not on PATH" >&2
        echo "[info]  install Nix (https://nixos.org/download) then re-run this script" >&2
        exit 1
    fi
    echo "[info] not inside \`nix develop\` — re-executing inside the dev shell"
    exec nix develop --command "$0" "$@"
fi

for tool in cargo npm node; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo "[error] required tool \`$tool\` not found on PATH" >&2
        echo "[info]  re-run this script from outside \`nix develop\` so it can re-enter the dev shell, or fix the dev shell to provide \`$tool\`" >&2
        exit 1
    fi
done

echo "[info] === step 1/2: frontend (TS + Vite) ==="
cd assets/ide
if [ ! -d node_modules ]; then
    echo "[info] node_modules missing — running \`npm install\` (first-time, ~30s+)"
    npm install
else
    echo "[info] node_modules present — skipping \`npm install\` (rerun manually if package.json changed)"
fi
echo "[info] running \`npm run build\` (tsc -b && vite build)"
npm run build
cd "$REPO_ROOT"

echo ""
echo "[info] === step 2/2: backend (Rust + cargo) ==="
echo "[info] running \`cargo build $*\`"
cargo build "$@"

echo ""
echo "[ok] full build complete"
echo "[info]   frontend artifacts: assets/ide/dist/   (embedded into the binary at compile time)"
case " $* " in
    *" --release "*) echo "[info]   binary:             target/release/neo" ;;
    *)               echo "[info]   binary:             target/debug/neo" ;;
esac
