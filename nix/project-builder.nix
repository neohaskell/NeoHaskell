{ pkgs }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:
let
  common = import ./common.nix { inherit pkgs; };
  project = common.setupProject { inherit neoJsonPath neoHaskellSource srcPath; };
  generatedFiles = common.generateProjectFiles {
    inherit (project) neoConfig srcPathValue modules generators;
    inherit neoHaskellSource;
  };

in pkgs.haskellPackages.developPackage {
  name = project.neoConfig.name;
  root = generatedFiles;
  returnShellEnv = false;
  source-overrides = common.sourceOverrides neoHaskellSource;
  modifier = drv: pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontHaddock drv);
}
