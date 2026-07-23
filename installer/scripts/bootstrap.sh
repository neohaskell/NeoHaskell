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

# fetch_releases_page: print the GitHub "list releases" JSON for page $1 (100
# per page, newest-first). Factored out as the single network seam so the
# consistency tests can redefine it with in-memory fixtures — no download —
# while production always hits the real GitHub API.
fetch_releases_page() {
  curl -fsSL "https://api.github.com/repos/${REPO}/releases?per_page=100&page=$1"
}

# resolve_latest_installer_tag: page through the repo's releases (newest-first)
# and print the newest 'installer-v*' tag. GitHub paginates across ALL tags, so
# a run of more than 100 newer non-installer releases can push the newest
# installer release onto a later page — keep fetching until an 'installer-v*'
# tag turns up or an empty page ends the stream. Prints nothing when no
# installer release exists, so the caller fails loudly instead of building a
# URL that would 404.
resolve_latest_installer_tag() {
  page=1
  while :; do
    body=$(fetch_releases_page "$page") || return 1
    case "$body" in
      *'"tag_name"'*) ;; # page has releases — inspect it below
      *) return 0 ;;     # empty/last page → stop; nothing found
    esac
    tag=$(printf '%s\n' "$body" | newest_installer_tag)
    if [ -n "$tag" ]; then
      printf '%s\n' "$tag"
      return 0
    fi
    page=$((page + 1))
  done
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

  # Stage the download in an unpredictable temp file (mktemp) with a cleanup
  # trap: a fixed /tmp path lets another local user pre-create or swap the file
  # between download, chmod, and exec (TOCTOU/symlink hijack).
  tmpfile=$(mktemp "${TMPDIR:-/tmp}/neo-install.XXXXXX") || exit 1
  trap 'rm -f "$tmpfile"' 0
  curl -fsSL "$DOWNLOAD_URL" -o "$tmpfile"
  chmod +x "$tmpfile"
  "$tmpfile" "$@"
}

# Run the installer only when executed directly. The consistency tests source
# this file with NEO_BOOTSTRAP_SOURCE_ONLY=1 to load the functions above and
# exercise newest_installer_tag / resolve_latest_installer_tag (redefining
# fetch_releases_page with fixtures) without any download or install side effect.
if [ "${NEO_BOOTSTRAP_SOURCE_ONLY:-0}" != "1" ]; then
  main "$@"
fi
