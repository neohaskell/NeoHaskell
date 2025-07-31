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

  # Build the shell environment with HLS
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      nhcore = self.callCabal2nix "nhcore" (neoHaskellSource + "/core") {};
    };
  };

in haskellPackages.shellFor {
  packages = p: [
    (p.callCabal2nix neoConfig.name generatedFiles {})
  ];
  buildInputs = with haskellPackages; [
    cabal-install
    haskell-language-server
    fourmolu
    hlint
  ];
  shellHook = ''
    echo "Welcome to the NeoHaskell development shell!"
    echo ""
    echo "Generated files are available at: ${generatedFiles}"
    echo ""
    echo "Available commands:"
    echo "  cabal build     - Build the project"
    echo "  cabal run       - Run the project"
    echo "  cabal repl      - Open a REPL"
    echo "  haskell-language-server - HLS is available for your IDE"
    echo ""
    echo "For best IDE experience, make sure your editor is configured to use:"
    echo "  - Working directory: ${projectDir}"
    echo "  - Cabal file: ${generatedFiles}/${neoConfig.name}.cabal"
    echo ""
    
    # Set up the environment to use the generated files
    export CABAL_DIR="${generatedFiles}"
    export HIE_BIOS_CRADLE="{\"cradle\":{\"cabal\":{\"component\":\"lib:${neoConfig.name}\"}}}"
  '';
}