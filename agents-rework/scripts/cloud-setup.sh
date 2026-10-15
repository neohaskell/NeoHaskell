#!/usr/bin/env bash
#
# Cloud environment SETUP SCRIPT for NeoHaskell.
#
# Configure this as a cloud environment setup command before agent work.
# It installs Nix and warms the pinned toolchain. Run in a disposable Linux
# environment with root access; local contributors use README.md instead.
# No agent-specific session hook is required.
#
# Network: the environment's allowed hosts MUST include the Nix binary caches
#   cache.iog.io, neohaskell.cachix.org, cache.nixos.org, releases.nixos.org
# plus github.com / objects.githubusercontent.com and hackage.haskell.org.
# Without the first two, Nix compiles GHC + every dependency from source and
# will blow the time budget.
#
set -euo pipefail

NIX_PROFILE_BIN="/nix/var/nix/profiles/default/bin"
NIX_CUSTOM_CONF="/etc/nix/nix.custom.conf"

IOG_KEY="hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
NEO_KEY="neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="
SUBSTITUTERS="https://cache.iog.io https://neohaskell.cachix.org"

log() { printf '\n=== %s ===\n' "$*"; }

# --- 1. Install Nix (idempotent) -------------------------------------------
# Determinate nix-installer fetched straight from GitHub releases.
# --prefer-upstream-nix pulls upstream Nix; --init none = root-only (no systemd).
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

export PATH="${NIX_PROFILE_BIN}:${PATH}"

# --- 2. Ensure the binary caches are configured ----------------------------
if [ ! -f "${NIX_CUSTOM_CONF}" ] || ! grep -q "neohaskell.cachix.org" "${NIX_CUSTOM_CONF}" 2>/dev/null; then
  log "Writing NeoHaskell cache configuration"
  {
    echo "extra-experimental-features = nix-command flakes"
    echo "accept-flake-config = true"
    echo "extra-substituters = ${SUBSTITUTERS}"
    echo "extra-trusted-public-keys = ${IOG_KEY} ${NEO_KEY}"
  } >> "${NIX_CUSTOM_CONF}"
fi

# --- 3. Warm the dev shell + update the cabal index ------------------------
# Pulls GHC + tooling from the caches into /nix/store and populates the cabal
# index — both land on disk and are captured by the snapshot. Tolerant of a
# transient cache hiccup: the dev shell simply resolves on first use instead.
PROJECT_DIR="${NEOHASKELL_PROJECT_DIR:-}"
if [ -z "${PROJECT_DIR}" ] || [ ! -f "${PROJECT_DIR}/flake.nix" ]; then
  for d in /home/user/NeoHaskell "$(pwd)" /root/NeoHaskell /workspace/NeoHaskell; do
    if [ -f "${d}/flake.nix" ]; then PROJECT_DIR="${d}"; break; fi
  done
fi

if [ -n "${PROJECT_DIR}" ] && [ -f "${PROJECT_DIR}/flake.nix" ]; then
  log "Warming the Nix dev shell and updating the cabal index (${PROJECT_DIR})"
  cd "${PROJECT_DIR}"
  nix develop --command cabal update \
    || echo "WARNING: dev-shell warm / cabal update did not complete; will resolve on first use." >&2
else
  echo "NOTE: repo not checked out at setup time; the dev shell will warm on first session instead." >&2
fi

log "Setup script complete"
