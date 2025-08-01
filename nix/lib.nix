{ pkgs ? import ./nixpkgs.nix {} }:

let
  # Import haskell.nix
  haskellNix = import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
  }) {};
  
  nixpkgs = import haskellNix.sources.nixpkgs {
    overlays = [ haskellNix.overlay ];
    system = builtins.currentSystem;
  };
  
  pkgsWithHaskellNix = nixpkgs;

in {
  # Main function that user projects will call
  buildNeoProject = import ./project-builder.nix { pkgs = pkgsWithHaskellNix; };
  
  # Shell function for development environment
  shellForNeoProject = import ./project-shell.nix { pkgs = pkgsWithHaskellNix; };
  
  # New haskell.nix versions
  buildNeoProjectHaskellNix = import ./project-builder-haskellnix.nix { 
    pkgs = pkgsWithHaskellNix; 
  };
  shellForNeoProjectHaskellNix = import ./project-shell-haskellnix.nix { 
    pkgs = pkgsWithHaskellNix;
  };
  
  # Individual generators (for advanced usage)
  generators = {
    cabal = import ./generators/cabal.nix { pkgs = pkgsWithHaskellNix; };
    main = import ./generators/main.nix { pkgs = pkgsWithHaskellNix; };
    defaultNix = import ./generators/default-nix.nix { pkgs = pkgsWithHaskellNix; };
  };
  
  # Utility functions
  utils = {
    discoverModules = import ./utils/modules.nix { pkgs = pkgsWithHaskellNix; };
    processDeps = import ./utils/deps.nix { pkgs = pkgsWithHaskellNix; };
  };
}