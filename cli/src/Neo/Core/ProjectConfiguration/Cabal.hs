module Neo.Core.ProjectConfiguration.Cabal (
  ghcOptions,
  defaultExtensions,
  executableName,
  launcherFile,
  launcherDirectory,
  sourceDirectory,
  testDirectory,
  testLauncherFile,
  defaultDependencies,
) where

import Core


executableName :: Text
executableName = "launch"


launcherFile :: Text
launcherFile = "Launcher.hs"


launcherDirectory :: Text
launcherDirectory = "Launcher"


sourceDirectory :: Text
sourceDirectory = "Source"


testLauncherFile :: Text
testLauncherFile = "TestLauncher.hs"


testDirectory :: Text
testDirectory = "Test"


ghcOptions :: Text
ghcOptions =
  [fmt|
      -Wall
      -Werror
      -threaded
|]


defaultExtensions :: Text
defaultExtensions =
  [fmt|
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
|]


defaultDependencies :: Text
defaultDependencies =
  [fmt|
      nhcore,
|]