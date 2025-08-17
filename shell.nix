{ pkgs ? import <nixpkgs> { } }: ((import ./default.nix { inherit pkgs; }).env)
