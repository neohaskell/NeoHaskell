#!/bin/sh
set -e

# NeoHaskell Installer Bootstrap
# Source: https://github.com/neohaskell/NeoHaskell
#
# The installer ships from the NeoHaskell monorepo: its release job
# (.github/workflows/installer-ci.yml) publishes assets named
# 'installer-neo-install-<target>' on 'installer-v*' tags. Pin a specific
# release with NEO_INSTALLER_VERSION=installer-vX.Y.Z.
#
# Installer assets exist ONLY on 'installer-v*' tags, so the default ("latest")
# resolves the newest 'installer-v*' release explicitly via the GitHub API —
# NOT the repository-wide 'releases/latest' redirect, which would point at the
# newest release of ANY tag (e.g. a core-library tag) and 404 for our asset.

REPO="neohaskell/NeoHaskell"
VERSION="${NEO_INSTALLER_VERSION:-latest}"

detect_platform() {
  OS=$(uname -s | tr '[:upper:]' '[:lower:]')
  ARCH=$(uname -m)
  case "$OS" in
    darwin) OS="apple-darwin" ;;
    linux) OS="unknown-linux-gnu" ;;
    *) echo "Unsupported OS: $OS" >&2; exit 1 ;;
  esac
  case "$ARCH" in
    arm64|aarch64) ARCH="aarch64" ;;
    x86_64|amd64) ARCH="x86_64" ;;
    *) echo "Unsupported architecture: $ARCH" >&2; exit 1 ;;
  esac
  echo "${ARCH}-${OS}"
}

# newest_installer_tag: read a GitHub "list releases" JSON payload on stdin and
# print the tag_name of the most recent installer release ('installer-v*'). The
# list API returns releases newest-first, so filtering to 'installer-v*' and
# taking the first match yields the newest installer release while ignoring any
# newer non-installer release. No jq: plain grep/sed, matching the rest of this
# POSIX-sh script.
newest_installer_tag() {
  grep -o '"tag_name":[[:space:]]*"[^"]*"' \
    | sed 's/.*"\([^"]*\)"$/\1/' \
    | grep '^installer-v' \
    | head -n 1
}

# resolve_latest_installer_tag: query the repo's releases and pick the newest
# 'installer-v*' one. per_page=100 so the newest installer release is on the
# first page even behind a run of newer core releases.
resolve_latest_installer_tag() {
  curl -fsSL "https://api.github.com/repos/${REPO}/releases?per_page=100" \
    | newest_installer_tag
}

main() {
  PLATFORM=$(detect_platform)

  ASSET="installer-neo-install-${PLATFORM}"

  if [ "$VERSION" = "latest" ]; then
    VERSION=$(resolve_latest_installer_tag)
    if [ -z "$VERSION" ]; then
      echo "Could not resolve the latest 'installer-v*' release from ${REPO}" >&2
      echo "Pin one explicitly with NEO_INSTALLER_VERSION=installer-vX.Y.Z" >&2
      exit 1
    fi
  fi

  DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${VERSION}/${ASSET}"

  echo "🔍 Source: https://github.com/${REPO}"
  echo "📦 Downloading neo-install ${VERSION} for ${PLATFORM}..."

  curl -fsSL "$DOWNLOAD_URL" -o /tmp/neo-install
  chmod +x /tmp/neo-install
  /tmp/neo-install "$@"
}

# Run the installer only when executed directly. The consistency tests source
# this file with NEO_BOOTSTRAP_SOURCE_ONLY=1 to load the functions above and
# exercise newest_installer_tag without any download or install side effect.
if [ "${NEO_BOOTSTRAP_SOURCE_ONLY:-0}" != "1" ]; then
  main "$@"
fi
