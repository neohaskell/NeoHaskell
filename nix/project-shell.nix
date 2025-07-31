{ pkgs }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:
let
  neoConfig = builtins.fromJSON (builtins.readFile neoJsonPath);
  projectDir = builtins.dirOf neoJsonPath;
  actualSrcPath = if srcPath != null then srcPath else "${projectDir}/src";
  # Convert string path to Nix path for sandbox access
  srcPathValue = if srcPath != null then /. + srcPath else projectDir + "/src";

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
    echo "Copying from: ${srcPathValue}"
    echo "Source structure before copy:"
    find ${srcPathValue} -type f -name "*.hs" || true
    cp -r ${srcPathValue}/. $out/src/ 2>/dev/null || true
    echo "Destination structure after copy:"
    find $out/src -type f -name "*.hs" || true

    # Debug: show discovered modules
    echo "Discovered modules: ${builtins.toJSON modules}"
    echo "Discovered modules: ${
      builtins.toJSON modules
    }" > /tmp/neo-debug-modules.txt

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

  # Build the shell environment using developPackage like the builder does
  devShell = pkgs.haskellPackages.developPackage {
    name = neoConfig.name;
    root = generatedFiles;
    returnShellEnv = true;
    source-overrides = { nhcore = /. + (toString neoHaskellSource) + "/core"; };
    modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
      fourmolu
      hlint
    ]);
  };
in devShell.overrideAttrs (oldAttrs: {
  shellHook = (oldAttrs.shellHook or "") + ''
    # Use the correct package database path
    PACKAGE_DB="$NIX_GHC_LIBDIR/package.conf.d"
    
    # Generate hie.yaml for HLS using direct cradle to avoid polluting user directory
    cat > ${projectDir}/hie.yaml << EOF
    cradle:
      direct:
        arguments:
          - "-i${actualSrcPath}"
          - "-package-db"
          - "$PACKAGE_DB"
          - "-hide-all-packages"
          - "-package"
          - "base"
          - "-package"
          - "nhcore"
    EOF
    
    echo "NeoHaskell development environment ready!"
    echo "Generated project files: ${generatedFiles}"
    echo "HLS configuration: ${projectDir}/hie.yaml"
    echo "Package DB: $PACKAGE_DB"
    echo "Package DB exists: $(test -d "$PACKAGE_DB" && echo "YES" || echo "NO")"
  '';
})