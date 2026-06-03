#!/usr/bin/env bash
#
# SessionStart hook for Claude Code on the web.
#
# NeoHaskell builds entirely through `nix develop --command ...`, so a remote
# session is only useful once Nix is installed, the haskell.nix toolchain is
# pulled from the binary caches, and `cabal` can resolve packages. This hook
# provisions all of that.
#
# IMPORTANT — network policy:
#   The build relies on two Nix binary caches. The environment's network
#   policy MUST allow outbound HTTPS to:
#     - cache.iog.io            (haskell.nix / GHC)
#     - neohaskell.cachix.org   (project + dev-shell tools)
#     - cache.nixos.org         (nixpkgs)
#     - releases.nixos.org      (the Nix installer payload)
#     - github.com / objects.githubusercontent.com / raw.githubusercontent.com
#     - hackage.haskell.org     (cabal update)
#   Without the first two, Nix falls back to compiling GHC and every dependency
#   from source (~30+ min and likely to time out). If you see
#   "Host not in allowlist" 403s below, widen the environment's network policy.
#
set -euo pipefail

# Only run inside the remote (Claude Code on the web) environment.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

NIX_PROFILE_BIN="/nix/var/nix/profiles/default/bin"
NIX_CUSTOM_CONF="/etc/nix/nix.custom.conf"

IOG_KEY="hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
NEO_KEY="neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="
SUBSTITUTERS="https://cache.iog.io https://neohaskell.cachix.org"

log() { printf '\n=== %s ===\n' "$*"; }

# --- 1. Install Nix (idempotent) -------------------------------------------
# Uses the Determinate nix-installer binary fetched straight from GitHub
# releases (no dependency on install.determinate.systems, which may be blocked).
# --prefer-upstream-nix pulls upstream Nix from releases.nixos.org.
# --init none installs a root-only Nix (the container has no systemd).
if [ ! -x "${NIX_PROFILE_BIN}/nix" ]; then
  log "Installing Nix"
  installer="$(mktemp)"
  curl --proto '=https' --tlsv1.2 -fsSL \
    "https://github.com/DeterminateSystems/nix-installer/releases/latest/download/nix-installer-x86_64-linux" \
    -o "${installer}"
  chmod +x "${installer}"
  "${installer}" install linux \
    --init none \
    --no-confirm \
    --prefer-upstream-nix \
    --diagnostic-endpoint "" \
    --extra-conf "experimental-features = nix-command flakes" \
    --extra-conf "accept-flake-config = true" \
    --extra-conf "extra-substituters = ${SUBSTITUTERS}" \
    --extra-conf "extra-trusted-public-keys = ${IOG_KEY} ${NEO_KEY}"
  rm -f "${installer}"
else
  log "Nix already installed — skipping installation"
fi

# --- 2. Ensure the binary caches are configured ----------------------------
# Guards against a pre-existing Nix install that lacks our caches.
if [ ! -f "${NIX_CUSTOM_CONF}" ] || ! grep -q "neohaskell.cachix.org" "${NIX_CUSTOM_CONF}" 2>/dev/null; then
  log "Writing NeoHaskell cache configuration"
  {
    echo "extra-experimental-features = nix-command flakes"
    echo "accept-flake-config = true"
    echo "extra-substituters = ${SUBSTITUTERS}"
    echo "extra-trusted-public-keys = ${IOG_KEY} ${NEO_KEY}"
  } >> "${NIX_CUSTOM_CONF}"
fi

# --- 3. Put Nix on PATH for this hook and all future tool calls -------------
export PATH="${NIX_PROFILE_BIN}:${PATH}"
if [ -n "${CLAUDE_ENV_FILE:-}" ] && ! grep -q "${NIX_PROFILE_BIN}" "${CLAUDE_ENV_FILE}" 2>/dev/null; then
  echo "export PATH=\"${NIX_PROFILE_BIN}:\$PATH\"" >> "${CLAUDE_ENV_FILE}"
fi

log "Nix version"
nix --version

# --- 4. Warm the dev shell + update the package index ----------------------
# Entering the dev shell pulls the GHC toolchain (HLS, fourmolu, hlint, cabal,
# ...) from the caches and runs `cabal update` so package resolution works.
log "Warming the Nix dev shell and updating the cabal index"
cd "${CLAUDE_PROJECT_DIR:-$(pwd)}"
nix develop --command cabal update

# --- 5. Best-effort build warm-up ------------------------------------------
# Compiling the project caches dist-newstyle for faster first edits. Kept
# best-effort so a build hiccup never blocks the session from starting.
log "Pre-building the project (best-effort)"
if ! nix develop --command cabal build all --disable-documentation; then
  echo "WARNING: 'cabal build all' did not complete. The session will still start;" >&2
  echo "         run it manually once any build issue is resolved." >&2
fi

log "SessionStart hook complete"
