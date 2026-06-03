#!/usr/bin/env bash
#
# SessionStart hook for Claude Code on the web.
#
# The heavy provisioning — installing Nix, configuring the binary caches, and
# warming the haskell.nix dev shell — now lives in the cloud environment's
# **setup script** (configured in the Claude Code web UI, see
# scripts/cloud-setup.sh). A setup script runs once and is snapshotted, so
# /nix/store and the cabal index are already on disk when a session starts and
# never have to be re-pulled.
#
# This hook only does the per-session work a snapshot cannot capture:
#   1. Put Nix on PATH for every Bash tool call (env vars are not snapshotted).
#   2. Kick a best-effort background `cabal build all` to warm dist-newstyle
#      (the repo is cloned fresh each session, so a build never carries over).
#
# Network policy reminder: the setup script needs outbound HTTPS to the Nix
# binary caches (cache.iog.io, neohaskell.cachix.org, cache.nixos.org,
# releases.nixos.org) plus github.com and hackage.haskell.org. Configure these
# in the environment's allowed hosts.
#
set -euo pipefail

# Only run inside the remote (Claude Code on the web) environment.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Run asynchronously so the session is usable immediately while the build warms
# in the background. The JSON control line must be the first thing on stdout.
echo '{"async": true, "asyncTimeout": 1800000}'

NIX_PROFILE_BIN="/nix/var/nix/profiles/default/bin"

log() { printf '\n=== %s ===\n' "$*"; }

# --- 1. Put Nix on PATH for this hook and all future tool calls -------------
# The setup-script snapshot leaves Nix on disk, but per-session env vars are not
# captured, so PATH is re-exported every session via $CLAUDE_ENV_FILE.
if [ ! -x "${NIX_PROFILE_BIN}/nix" ]; then
  echo "WARNING: Nix not found at ${NIX_PROFILE_BIN}." >&2
  echo "         Configure the environment **setup script** in the Claude Code" >&2
  echo "         web UI (use scripts/cloud-setup.sh) to install Nix and warm the" >&2
  echo "         dev shell. Without it this session has no toolchain." >&2
  exit 0
fi

export PATH="${NIX_PROFILE_BIN}:${PATH}"
if [ -n "${CLAUDE_ENV_FILE:-}" ] && ! grep -q "${NIX_PROFILE_BIN}" "${CLAUDE_ENV_FILE}" 2>/dev/null; then
  echo "export PATH=\"${NIX_PROFILE_BIN}:\$PATH\"" >> "${CLAUDE_ENV_FILE}"
fi

log "Nix version"
nix --version

# --- 2. Best-effort build warm-up -------------------------------------------
# The dev shell + cabal index come from the snapshot, but the repo is cloned
# fresh each session so dist-newstyle is empty. Warm it (this hook is already
# async, so it does not block the session); a build hiccup must never stop the
# session from starting.
log "Pre-building the project (best-effort)"
cd "${CLAUDE_PROJECT_DIR:-$(pwd)}"
if ! nix develop --command cabal build all --disable-documentation; then
  echo "WARNING: 'cabal build all' did not complete. The session will still start;" >&2
  echo "         run it manually once any build issue is resolved." >&2
fi

log "SessionStart hook complete"
