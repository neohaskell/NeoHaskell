#!/usr/bin/env bash

set -euo pipefail

# Binary cache configuration
BINARY_CACHE_SUBSTITUTERS="https://cache.iog.io"
BINARY_CACHE_PUBLIC_KEYS="hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="

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
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --determinate
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

echo "🚀 Setting up binary caches to speed up builds..."
echo ""
echo "NeoHaskell can use pre-built binaries from cache servers to dramatically"
echo "speed up installation (from hours to minutes). This requires sudo access"
echo "to modify /etc/nix/nix.conf."
echo ""
read -p "Add binary caches? (recommended, requires sudo) [Y/n]: " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "⚠️  Skipping binary cache setup. Expect longer build times (potentially hours)."
else
    echo "🔧 Adding binary cache configuration..."
    
    # Add binary cache configuration to custom config file
    sudo mkdir -p /etc/nix
    if [ ! -f /etc/nix/nix.custom.conf ]; then
        echo "Creating /etc/nix/nix.custom.conf..."
        sudo touch /etc/nix/nix.custom.conf
    fi
    
    # Append our config if not already present
    if ! sudo grep -q "$BINARY_CACHE_SUBSTITUTERS" /etc/nix/nix.custom.conf; then
        echo "extra-substituters = $BINARY_CACHE_SUBSTITUTERS" | sudo tee -a /etc/nix/nix.custom.conf > /dev/null
    fi
    if ! sudo grep -q "$BINARY_CACHE_PUBLIC_KEYS" /etc/nix/nix.custom.conf; then
        echo "extra-trusted-public-keys = $BINARY_CACHE_PUBLIC_KEYS" | sudo tee -a /etc/nix/nix.custom.conf > /dev/null
    fi
    
    echo "✅ Binary cache configuration added successfully!"
fi

echo ""
echo "📦 Installing NeoHaskell from GitHub..."
nix-env -if https://github.com/neohaskell/NeoHaskell/archive/refs/heads/main.tar.gz

echo "✅ NeoHaskell installed successfully!"
echo ""
echo "Try running 'neo --help' to see what you can do with it."
echo ""
echo ""
echo "If the command is not found, try running"
echo ""
echo '. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
echo ""
echo "first. Or restart your shell."
echo ""
