#!/usr/bin/env bash

set -euo pipefail

fail() {
    echo ""
    echo "neo: ❌ Oops! The installation script encountered an error."
    echo ""
    echo "     If it's taking you more than 15 minutes to figure out,"
    echo "     consider it a bug in the installer, not your fault."
    echo ""
    echo "     Please report the issue at:"
    echo ""
    echo "     https://github.com/neohaskell/neohaskell/issues/new"
    echo ""
    echo "     Include your OS, shell, and anything you saw printed above."
    echo ""
    echo "     I'll be waiting for you."
    exit 1
}

trap fail ERR

if ! command -v nix &> /dev/null; then
    echo "⚙️  Nix is not installed. Installing Nix with Determinate Systems..."
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --determinate --yes
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

echo "📦 Installing NeoHaskell from GitHub..."
nix-env -if https://github.com/neohaskell/NeoHaskell/archive/refs/heads/main.tar.gz

echo "✅ NeoHaskell installed successfully!"
