{ pkgs ? import ./nixpkgs.nix {} }:
{
  # Main function that user projects will call
  buildNeoProject = import ./project-builder.nix { inherit pkgs; };
  
  # Individual generators (for advanced usage)
  generators = {
    cabal = import ./generators/cabal.nix { inherit pkgs; };
    main = import ./generators/main.nix { inherit pkgs; };
    defaultNix = import ./generators/default-nix.nix { inherit pkgs; };
  };
  
  # Utility functions
  utils = {
    discoverModules = import ./utils/modules.nix { inherit pkgs; };
    processDeps = import ./utils/deps.nix { inherit pkgs; };
  };
}