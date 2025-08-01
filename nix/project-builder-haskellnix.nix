{ pkgs, haskell-nix }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:

let
  common = import ./common.nix { inherit pkgs; };
  project = common.setupProject { inherit neoJsonPath neoHaskellSource srcPath; };
  generatedFiles = common.generateProjectFiles {
    inherit (project) neoConfig srcPathValue modules generators;
    inherit neoHaskellSource;
  };
  
  haskellProject = haskell-nix.cabalProject {
    src = generatedFiles;
    compiler-nix-name = "ghc984";
    
    # Handle nhcore dependency via source-repository-package in cabal file
    modules = [{
      packages.${project.neoConfig.name} = {
        ghcOptions = common.ghcFlags;
        doCheck = false;
        doHaddock = false;
      };
    }];
  };
  
in haskellProject.${project.neoConfig.name}.components.exes.${project.neoConfig.name}