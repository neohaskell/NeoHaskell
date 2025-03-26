module Neo.Build (
  handle,
  Error (..),
) where

import Array qualified
import Directory qualified
import File qualified
import Maybe qualified
import Neo.Build.Templates.AppMain qualified as AppMain
import Neo.Build.Templates.Cabal qualified as Cabal
import Neo.Build.Templates.Nix qualified as Nix
import Neo.Core
import Path qualified
import Subprocess qualified
import Task qualified
import Text qualified
import ToText (toText)


data Error
  = NixFileError
  | CabalFileError
  | CustomError Text
  deriving (Show)


handle :: ProjectConfiguration -> Task Error Unit
handle config = do
  let haskellExtension = ".hs"
  let projectName = config.name
  let rootFolder = [path|nhout|]
  let nixFileName = [path|default.nix|]
  let cabalFileName =
        [fmt|{projectName}.cabal|]
          |> Path.fromText
          |> Maybe.getOrDie -- TODO: Make better error handling here
  let nixFile = Nix.template config
  let nixFilePath =
        Array.fromLinkedList [rootFolder, nixFileName]
          |> Path.joinPaths
  let cabalFilePath =
        Array.fromLinkedList [rootFolder, cabalFileName]
          |> Path.joinPaths
  let targetAppFolder =
        Array.fromLinkedList [rootFolder, "app"]
          |> Path.joinPaths
  let targetAppPath =
        Array.fromLinkedList [targetAppFolder, "Main.hs"]
          |> Path.joinPaths
  let targetSrcFolder =
        Array.fromLinkedList [rootFolder, "src"]
          |> Path.joinPaths

  Directory.copy [path|src|] targetSrcFolder
    |> Task.mapError (\e -> CustomError (toText e))

  filepaths <-
    Directory.walk [path|src|]
      |> Task.mapError (\_ -> CustomError "WALK ERROR")

  let haskellFiles = filepaths |> Array.takeIf (Path.endsWith haskellExtension)

  let convertToModule filepath = do
        let pathText = Path.toText filepath
        let pathWithoutExtension = Text.dropRight (Text.length haskellExtension) pathText
        let pathParts = Text.split "/" pathWithoutExtension
        pathParts |> Text.joinWith "."

  let modules = haskellFiles |> Array.map convertToModule
  let cabalFile = Cabal.template config modules
  let appMainFile = AppMain.template config

  Directory.create rootFolder
    |> Task.mapError (\_ -> [fmt|Could not create directory {Path.toText rootFolder}|] |> CustomError)

  Directory.create targetAppFolder
    |> Task.mapError (\_ -> [fmt|Could not create directory {Path.toText targetAppFolder}|] |> CustomError)

  File.writeText nixFilePath nixFile
    |> Task.mapError (\_ -> NixFileError)

  File.writeText cabalFilePath cabalFile
    |> Task.mapError (\_ -> CabalFileError)

  File.writeText targetAppPath appMainFile
    |> Task.mapError (\_ -> CustomError "Could not write app main file")

  -- FIXME: Create another thread that renders the output of the build via streaming.
  -- As right now there's no output at all
  completion <- Subprocess.openInherit "nix-build" (Array.fromLinkedList []) rootFolder Subprocess.InheritBOTH
  if completion.exitCode != 0
    then errorOut completion.stderr
    else print completion.stdout


errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops the build failed:
    {err}|]
    |> CustomError
    |> Task.throw
