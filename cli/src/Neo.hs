module Neo (
  run,
  fromText,
) where

import Array qualified
import Command qualified
import Core
import File qualified
import Json qualified


data CommonFlags = CommonFlags
  { projectFile :: Path
  }
  deriving (Show, Eq, Ord)


data NeoCommand
  = Build CommonFlags
  deriving (Show, Eq, Ord)


run :: IO ()
run = do
  let parser =
        Command.CommandOptions
          { name = "neo",
            description = "NeoHaskell's console helper",
            version = Just [Core.version|0.5.0|],
            decoder = commandsParser
          }
  cmd <- Command.parseHandler parser
  handleCommand cmd


commandsParser :: Command.OptionsParser NeoCommand
commandsParser = do
  let build =
        Command.CommandOptions
          { name = "build",
            description = "build a file or directory",
            version = Nothing,
            decoder = buildParser
          }
  Command.commands
    (Array.fromLinkedList [build])


buildParser :: Command.OptionsParser NeoCommand
buildParser = do
  common <- flagsParser
  pure (Build common)


flagsParser :: Command.OptionsParser CommonFlags
flagsParser = do
  projectFilePath <-
    Command.path
      Command.PathConfig
        { metavar = "PATH",
          short = 'c',
          help = "Path to the project configuration file",
          long = "projectConfig",
          value = Just [path|neo.json|]
        }
  pure (CommonFlags {projectFile = projectFilePath})


handleCommand :: NeoCommand -> IO ()
handleCommand command =
  case command of
    Build flags -> do
      let readOpts = File.ReadOptions {path = flags.projectFile}
      configTxt <- File.readTextHandler readOpts
      case configTxt of
        Err err -> toText err |> print
        Ok txt -> do
          case Json.decodeText txt of
            Err err -> print err
            Ok config -> handleBuild config


handleBuild :: ProjectConfiguration -> IO ()
handleBuild config = do
  let nixFile = makeNixFile config
  print nixFile

makeNixFile :: ProjectConfiguration -> Text
makeNixFile ProjectConfiguration{name} =
  --FIXME: inflect properly the name of the project in the different places of the nix file
  [fmt|
let
  myNixPkgs = import <nixpkgs> {
    overlays = [myNixPkgsOverlay];
  };

  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  myHaskellPkgsOverlay;
    });
  });

  myHaskellPkgsOverlay = (hSelf: hSuper: {
    myProject = hSelf.callCabal2nix "{name}" ./. {};
  });

  myDevTools = with myNixPkgs; [
    cabal-install
    haskellPackages.ghcid
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.myProject.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
  |]

makeCabalFile :: ProjectConfiguration -> Text
makeCabalFile ProjectConfiguration{name, version, description, license} =
  [fmt|
cabal-version:      3.4
name:               {name}
version:            {toText version}
synopsis:           {description}
license:            {license}
author:             {author}

common common_cfg
    ghc-options:    -Wall
                    -Werror
                    -threaded
    default-language: GHC2021
    default-extensions:
      ApplicativeDo
      BlockArguments
      DataKinds
      NoImplicitPrelude
      TemplateHaskell
      DeriveDataTypeable
      QuasiQuotes
      QualifiedDo
      ImpredicativeTypes
      ImportQualifiedPost
      OverloadedStrings
      OverloadedLabels
      OverloadedRecordDot
      DuplicateRecordFields
      PackageImports
      NamedFieldPuns
      Strict
      TypeFamilies

    build-depends:
      nhcore

library
    import:           common_cfg
    exposed-modules:
      Neo,
    -- other-modules:
    -- other-extensions:
    hs-source-dirs:   src

executable neo
    import:           common_cfg
    main-is:          Main.hs
    build-depends:
        {name}
    hs-source-dirs:   app

test-suite {name}-test
    import:           common_cfg
    type:             exitcode-stdio-1.0
    hs-source-dirs:   test
    main-is:          Main.hs
    build-depends:
        {name}
  |]

-- Project config

data ProjectConfiguration = ProjectConfiguration
  { name :: Text,
    version :: Version,
    description :: Text,
    author :: Text,
    license :: Text
  }
  deriving (Show, Eq, Ord, Generic)


-- | We allow the `ProjectConfiguration` type to be converted from JSON.
instance Json.FromJSON ProjectConfiguration


-- | We allow the `ProjectConfiguration` type to be converted to JSON.
instance Json.ToJSON ProjectConfiguration


-- | The `ProjectConfiguration.fromText` function allows you to convert a JSON
-- `Text` value to a `ProjectConfiguration` value.
--
-- >>> fromText "{\"name\":\"neo\",\"version\":\"0.5.0\",\"description\":\"NeoHaskell's console helper\",\"author\":\"NeoHaskell\",\"license\":\"MIT\"}"
-- Ok (ProjectConfiguration {name = "neo", version = [version|0.5.0|], description = "NeoHaskell's console helper", author = "NeoHaskell", license = "MIT"})
--
-- >>> fromText "some invalid json"
-- Err "Error in $: not enough input"
fromText :: Text -> Result Text ProjectConfiguration
fromText someText = Json.decodeText someText
