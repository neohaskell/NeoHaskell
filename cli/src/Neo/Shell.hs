module Neo.Shell (
  handle,
  Error (..),
) where

import Array qualified
import Directory qualified
import Maybe qualified
import Neo.Core
import Path qualified
import Subprocess qualified
import Task qualified


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
  let neoHaskellCommit = override
  let shellExpression =
        [fmt|
let
  lib = import (builtins.fetchTarball "https://github.com/NeoHaskell/NeoHaskell/archive/#{neoHaskellCommit}.tar.gz" + "/nix/lib.nix") {};
in
  lib.shellForNeoProjectHaskellNix {
    neoJsonPath = "#{neoPathText}";
    neoHaskellCommit = "#{neoHaskellCommit}";
    srcPath = "#{rootPathText}/src";
  }
  |]
  let nixShellParams = Array.fromLinkedList ["--show-trace", "-E", shellExpression]
  completion <- Subprocess.openInherit "nix-shell" nixShellParams rootPath Subprocess.InheritBOTH
  if completion.exitCode != 0
    then errorOut completion.stderr
    else Task.yield ()


errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops the shell failed:
    #{err}|]
    |> CustomError
    |> Task.throw


