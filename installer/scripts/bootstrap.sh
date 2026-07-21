#!/bin/sh
set -e

# NeoHaskell Installer Bootstrap
# Source: https://github.com/neohaskell/NeoHaskell
#
# The installer ships from the NeoHaskell monorepo: its release job
# (.github/workflows/installer-ci.yml) publishes assets named
# 'installer-neo-install-<target>' on 'installer-v*' tags. Pin a specific
# release with NEO_INSTALLER_VERSION=installer-vX.Y.Z.

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

PLATFORM=$(detect_platform)

ASSET="installer-neo-install-${PLATFORM}"

if [ "$VERSION" = "latest" ]; then
  DOWNLOAD_URL="https://github.com/${REPO}/releases/latest/download/${ASSET}"
else
  DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${VERSION}/${ASSET}"
fi

echo "🔍 Source: https://github.com/${REPO}"
echo "📦 Downloading neo-install for ${PLATFORM}..."

curl -fsSL "$DOWNLOAD_URL" -o /tmp/neo-install
chmod +x /tmp/neo-install
/tmp/neo-install "$@"
