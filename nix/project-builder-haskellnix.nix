{ pkgs }:
{ neoJsonPath, neoHaskellCommit, srcPath ? null }:

let
  common = import ./common.nix { inherit pkgs; };
  project = common.setupProject { inherit neoJsonPath neoHaskellCommit srcPath; };
  generatedFiles = common.generateProjectFiles {
    inherit (project) neoConfig srcPathValue modules generators;
    inherit neoHaskellCommit;
  };
  
  haskellProject = pkgs.haskell-nix.cabalProject {
    src = pkgs.lib.cleanSource generatedFiles;
    compiler-nix-name = "ghc984";
    
    modules = [{
      packages.${project.neoConfig.name} = {
        ghcOptions = common.ghcFlags;
        doCheck = false;
        doHaddock = false;
      };
    }];
  };
  
in haskellProject.${project.neoConfig.name}.components.exes.${project.neoConfig.name}