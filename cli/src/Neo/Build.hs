module Neo.Build (
  handle,
  parseNeoHaskellSource,
  Error (..),
) where

import Array qualified
import Directory qualified
import Maybe qualified
import Neo.Core
import Path qualified
import Subprocess qualified
import Task qualified
import Text qualified


data Error
  = NixFileError
  | DirectoryError (Directory.Error)
  | CustomError Text
  deriving (Show)


handle :: ProjectConfiguration -> Task Error Unit
handle config = do
  -- FIXME: Validate config first, should be done as semantic types,
  -- so that validation can be done in the deserialization automatically

  rootPath <- Directory.getCurrent |> Task.mapError DirectoryError
  let neoPathText = Array.fromLinkedList [rootPath, [path|neo.json|]] |> Path.joinPaths |> Path.toText
  let rootPathText = rootPath |> Path.toText
  let defaultOverride = "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/main.tar.gz"
  let override = config.overrideNeohaskell |> Maybe.withDefault defaultOverride
  let neoHaskellSource = parseNeoHaskellSource override
  let buildExpression =
        [fmt|
let
  nhroot = (#{neoHaskellSource});
  lib = import (nhroot + "/nix/lib.nix") {};
in
  lib.buildNeoProject {
    neoJsonPath = #{neoPathText};
    neoHaskellSource = #{neoHaskellSource};
    srcPath = #{rootPathText}/src;
  }
  |]
  let nixBuildParams = Array.fromLinkedList ["--show-trace", "-E", buildExpression]
  completion <- Subprocess.openInherit "nix-build" nixBuildParams rootPath Subprocess.InheritBOTH
  if completion.exitCode != 0
    then errorOut completion.stderr
    else print completion.stdout


errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops the build failed:
    #{err}|]
    |> CustomError
    |> Task.throw


parseNeoHaskellSource :: Text -> Text
parseNeoHaskellSource override = do
  let filePrefix = "file://"
  let httpsPrefix = "https://"
  case True of
    _ | Text.startsWith filePrefix override -> do
      let path = override |> Text.dropLeft (Text.length filePrefix)
      [fmt|/. + "#{path}"|]
    _
      | Text.startsWith httpsPrefix override ->
          [fmt|builtins.fetchTarball "#{override}"|]
    _ ->
      [fmt|builtins.fetchTarball "https://github.com/NeoHaskell/NeoHaskell/archive/#{override}.tar.gz"|]
