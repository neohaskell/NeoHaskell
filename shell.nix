{ pkgs ? import ./nix/nixpkgs.nix { } }: ((import ./common.nix { inherit pkgs; }).shell)
