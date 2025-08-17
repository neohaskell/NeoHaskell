{ pkgs ? import <nixpkgs> { } }: ((import ./common.nix { inherit pkgs; }).shell)
