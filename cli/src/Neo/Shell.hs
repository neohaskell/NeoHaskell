module Neo.Shell (
  handle,
  Error (..),
) where

import Array qualified
import Directory qualified
import File qualified
import Maybe qualified
import Neo.Build.Templates.AppMain qualified as AppMain
import Neo.Build.Templates.Cabal qualified as Cabal
import Neo.Build.Templates.CabalProject qualified as CabalProject
import Neo.Build.Templates.Nix qualified as Nix
import Neo.Core
import Path qualified
import Subprocess qualified
import Task qualified
import Text qualified


data Error
  = NixFileError
  | CabalFileError
  | CustomError Text
  deriving (Show)


handle :: ProjectConfiguration -> Task Error Unit
handle config = do
  let haskellExtension = ".hs"
  let projectName = config.name
  let rootFolder = [path|.|]
  let cabalFileName =
        [fmt|#{projectName}.cabal|]
          |> Path.fromText
          |> Maybe.getOrDie -- TODO: Make better error handling here
  let cabalProjectFileName = [path|cabal.project|]
  let nixFile = Nix.template config
  let cabalFilePath =
        Array.fromLinkedList [rootFolder, cabalFileName]
          |> Path.joinPaths
  let targetAppFolder =
        Array.fromLinkedList [rootFolder, ".launcher"]
          |> Path.joinPaths
  let targetAppPath =
        Array.fromLinkedList [targetAppFolder, "Main.hs"]
          |> Path.joinPaths

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
  let cabalProjectFile = CabalProject.template
  let appMainFile = AppMain.template config

  Directory.create targetAppFolder
    |> Task.mapError (\_ -> [fmt|Could not create directory #{Path.toText targetAppFolder}|] |> CustomError)

  File.writeText cabalFilePath cabalFile
    |> Task.mapError (\_ -> CabalFileError)

  File.writeText cabalProjectFileName cabalProjectFile
    |> Task.mapError (\_ -> CabalFileError)

  File.writeText targetAppPath appMainFile
    |> Task.mapError (\_ -> CustomError "Could not write app main file")

  let shellExpression :: Text =
        [fmt|{ pkgs ? import <nixpkgs> {} }:
  ( (#{nixFile}) { inherit pkgs; } ).shell|]

  completion <-
    Subprocess.openInherit "nix-shell" (Array.fromLinkedList ["-E", shellExpression]) rootFolder Subprocess.InheritBOTH
      |> Task.mapError (\err -> CustomError [fmt|Failed to open the shell: #{err}|])
  if completion.exitCode != 0
    then errorOut completion.stderr
    else Task.yield unit


errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops the shell failed:
    #{err}|]
    |> CustomError
    |> Task.throw
