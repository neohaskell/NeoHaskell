{ pkgs }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:
let
  neoConfig = builtins.fromJSON (builtins.readFile neoJsonPath);
  projectDir = builtins.dirOf neoJsonPath;
  actualSrcPath = if srcPath != null then srcPath else (projectDir + "/src");

  utils = import ./utils/modules.nix { inherit pkgs; };
  generators = {
    cabal = import ./generators/cabal.nix { inherit pkgs; };
    main = import ./generators/main.nix { inherit pkgs; };
    defaultNix = import ./generators/default-nix.nix { inherit pkgs; };
  };

  modules = utils.discoverModules actualSrcPath;

  # Generate project structure first
  generatedFiles = pkgs.runCommand "neo-project-files-${neoConfig.name}" { } ''
    mkdir -p $out/app $out/src

    # Copy source files if they exist
    if [ -d "${actualSrcPath}" ]; then
      cp -r ${actualSrcPath}/* $out/src/ 2>/dev/null || true
    fi

    # Generate project files
    cat > $out/${neoConfig.name}.cabal << 'EOF'
    ${generators.cabal { inherit neoConfig modules; }}
    EOF

    cat > $out/default.nix << 'EOF'
    ${generators.defaultNix { inherit neoConfig; }}
    EOF

    cat > $out/app/Main.hs << 'EOF'
    ${generators.main { inherit neoConfig; }}
    EOF
  '';

  # Build the actual Haskell project using the generated files
in pkgs.haskellPackages.developPackage {
  name = neoConfig.name;
  root = generatedFiles;
  returnShellEnv = false;
  source-overrides = { nhcore = neoHaskellSource + "/core"; };
  modifier = drv: pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontHaddock drv);
}
