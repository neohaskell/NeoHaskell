module Neo.Core.ProjectConfiguration (
  ProjectConfiguration (..),
  fromText,
  toCabal,
) where

import Core
import Json qualified
import Neo.Core.ProjectConfiguration.Cabal qualified as Cabal


-- | ProjectConfig defines the fields that can be defined in the project
-- configuration file (`neo.json` by default).
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


toCabal :: ProjectConfiguration -> Text
toCabal config = do
  let modules :: Text
      modules = ""

  let name = config.name
  let ver = config.version |> toText
  let description = config.description
  let license = config.license
  let author = config.author

  let ghcOptions = Cabal.ghcOptions
  let defaultExtensions = Cabal.defaultExtensions
  let executableName = Cabal.executableName
  let launcherFile = Cabal.launcherFile
  let launcherDirectory = Cabal.launcherDirectory
  let sourceDirectory = Cabal.sourceDirectory
  let testDirectory = Cabal.testDirectory
  let testLauncherFile = Cabal.testLauncherFile
  let defaultDependencies = Cabal.defaultDependencies
  [fmt|
cabal-version:      3.4
name:               {name}
version:            {ver}
synopsis:           {description}
license:            {license}
author:             {author}
build-type:         Simple

common common_cfg
    ghc-options:
{ghcOptions}
    default-extensions:
{defaultExtensions}
    build-depends:
{defaultDependencies}

library
    import:           common_cfg
    exposed-modules:
      {modules}
    hs-source-dirs:   {sourceDirectory}
    default-language: GHC2021

executable {executableName}
    import:           common_cfg
    main-is:          {launcherFile}
    build-depends:
        {name}
    hs-source-dirs:   {launcherDirectory}
    default-language: GHC2021

test-suite {name}-test
    import:           common_cfg
    default-language: GHC2021
    type:             exitcode-stdio-1.0
    hs-source-dirs:   {testDirectory}
    main-is:          {testLauncherFile}
    build-depends:
        {name}
|]
