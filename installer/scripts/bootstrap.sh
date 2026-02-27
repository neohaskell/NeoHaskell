#!/bin/sh
set -e

# NeoHaskell Installer Bootstrap
# Source: https://github.com/neohaskell/neo-installer

REPO="neohaskell/neo-installer"
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

if [ "$VERSION" = "latest" ]; then
  DOWNLOAD_URL="https://github.com/${REPO}/releases/latest/download/neo-install-${PLATFORM}"
else
  DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${VERSION}/neo-install-${PLATFORM}"
fi

echo "🔍 Source: https://github.com/${REPO}"
echo "📦 Downloading neo-install for ${PLATFORM}..."

curl -fsSL "$DOWNLOAD_URL" -o /tmp/neo-install
chmod +x /tmp/neo-install
/tmp/neo-install "$@"
