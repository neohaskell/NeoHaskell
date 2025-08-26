# Pinned nixpkgs for NeoHaskell project
# Last updated: 2025-08-25
# Commit from nixos-unstable branch (2025-08-19)
import (builtins.fetchTarball {
  name = "nixpkgs-20075955";
  url = "https://github.com/nixos/nixpkgs/archive/20075955deac2583bb12f07151c2df830ef346b4.tar.gz";
  # To get the sha256, run:
  # nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/20075955deac2583bb12f07151c2df830ef346b4.tar.gz
  sha256 = "1s3lxb33cwazlx72pygcbcc76bbgbhdil6q9bhqbzbjxj001zk0w";
})
