{ pkgs }:
{ neoJsonPath, srcPath ? null }:
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

  # Handle different override types from neo.json
  neoHaskellOverride = neoConfig.override_neohaskell or "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/main.tar.gz";
  
  neoHaskellSource = 
    if pkgs.lib.hasPrefix "file://" neoHaskellOverride then
      # Local file path - strip file:// prefix and convert to path
      /. + (builtins.substring 7 (builtins.stringLength neoHaskellOverride) neoHaskellOverride)
    else if pkgs.lib.hasPrefix "https://" neoHaskellOverride then
      # HTTPS URL - use fetchTarball
      builtins.fetchTarball neoHaskellOverride
    else
      # Assume it's a commit SHA/branch - build GitHub URL
      builtins.fetchTarball "https://github.com/NeoHaskell/NeoHaskell/archive/${neoHaskellOverride}.tar.gz";

in
  # Build the actual Haskell project using the generated files
  pkgs.haskellPackages.developPackage {
    name = neoConfig.name;
    root = generatedFiles;
    returnShellEnv = false;
    source-overrides = {
      nhcore = neoHaskellSource + "/core";
    };
    modifier = drv: pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontHaddock drv);
  }
