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
    src = generatedFiles;
    compiler-nix-name = "ghc984";
  };
  
in haskellProject.shellFor {
  packages = p: [ p.${project.neoConfig.name} ];
  
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    fourmolu = "latest";
    hlint = "latest";
  };
  
  shellHook = ''
    echo "NeoHaskell development environment ready!"
    echo "Project: ${project.neoConfig.name}"
    echo "Source: ${project.actualSrcPath}"
  '';
}