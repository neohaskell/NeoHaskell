{ pkgs }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:
let
  common = import ./common.nix { inherit pkgs; };
  project = common.setupProject { inherit neoJsonPath neoHaskellSource srcPath; };
  generatedFiles = common.generateProjectFiles {
    inherit (project) neoConfig srcPathValue modules generators;
  };

  # Build the shell environment using developPackage like the builder does
  devShell = pkgs.haskellPackages.developPackage {
    name = project.neoConfig.name;
    root = generatedFiles;
    returnShellEnv = true;
    source-overrides = common.sourceOverrides neoHaskellSource;
    modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
      fourmolu
      hlint
    ]);
  };

  # Convert GHC flags to hie.yaml arguments
  hieArguments = [
    ''-i${project.actualSrcPath}''
    ''-package-db''
    ''$PACKAGE_DB''
    ''-hide-all-packages''
    ''-package''
    ''base''
    ''-package''
    ''nhcore''
  ] ++ common.ghcFlags;

in devShell.overrideAttrs (oldAttrs: {
  shellHook = (oldAttrs.shellHook or "") + ''
    # Use the correct package database path
    PACKAGE_DB="$NIX_GHC_LIBDIR/package.conf.d"
    
    # Generate hie.yaml for HLS using direct cradle to avoid polluting user directory
    cat > ${project.projectDir}/hie.yaml << 'HIEYAML'
cradle:
  direct:
    arguments:
${pkgs.lib.concatMapStringsSep "\n" (arg: "      - \"${arg}\"") hieArguments}
HIEYAML
    
    echo "NeoHaskell development environment ready!"
  '';
})