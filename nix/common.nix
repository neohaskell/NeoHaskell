{ pkgs }: rec {
  # Common GHC options and language extensions
  ghcOptions = [ "-Wall" "-Werror" "-threaded" ];

  languageExtensions = [
    "ApplicativeDo"
    "BlockArguments"
    "DataKinds"
    "NoImplicitPrelude"
    "TemplateHaskell"
    "DeriveDataTypeable"
    "QuasiQuotes"
    "QualifiedDo"
    "ImpredicativeTypes"
    "ImportQualifiedPost"
    "OverloadedStrings"
    "OverloadedLabels"
    "OverloadedRecordDot"
    "DuplicateRecordFields"
    "PackageImports"
    "NamedFieldPuns"
    "Strict"
    "TypeFamilies"
  ];

  # Convert to GHC flags
  ghcFlags = ghcOptions ++ (map (ext: "-X${ext}") languageExtensions);

  # Common project setup
  setupProject = { neoJsonPath, neoHaskellSource, srcPath ? null }:
    let
      neoConfig = builtins.fromJSON (builtins.readFile neoJsonPath);
      projectDir = builtins.dirOf neoJsonPath;
      actualSrcPath = if srcPath != null then srcPath else "${projectDir}/src";
      srcPathValue =
        if srcPath != null then /. + srcPath else projectDir + "/src";

      utils = import ./utils/modules.nix { inherit pkgs; };
      generators = {
        cabal = import ./generators/cabal.nix { inherit pkgs; };
        main = import ./generators/main.nix { inherit pkgs; };
        defaultNix = import ./generators/default-nix.nix { inherit pkgs; };
      };

      modules = utils.discoverModules actualSrcPath;
    in {
      inherit neoConfig projectDir actualSrcPath srcPathValue modules utils
        generators;
    };

  # Common generated files
  generateProjectFiles = { neoConfig, srcPathValue, modules, generators, neoHaskellSource ? null }:
    pkgs.runCommand "nhs-${neoConfig.name}" { } ''
      mkdir -p $out/app $out/src
      cp -r ${srcPathValue}/. $out/src/ 2>/dev/null || true

      cat > $out/${neoConfig.name}.cabal << 'EOF'
      ${generators.cabal { inherit neoConfig modules neoHaskellSource; }}
      EOF

      cat > $out/default.nix << 'EOF'
      ${generators.defaultNix { inherit neoConfig; }}
      EOF

      cat > $out/app/Main.hs << 'EOF'
      ${generators.main { inherit neoConfig; }}
      EOF
    '';

  # Common source overrides
  sourceOverrides = neoHaskellSource: {
    nhcore = /. + (toString neoHaskellSource) + "/core";
  };
}
