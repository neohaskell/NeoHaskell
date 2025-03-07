module Neo.Build (
  handle,
  Error (..),
) where

import Array qualified
import Directory qualified
import File qualified
import Maybe qualified
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
  let rootFolder = [path|.neohaskell|]
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

  -- TODO: Remember to copy using https://hackage.haskell.org/package/directory-1.3.8.1/docs/System-Directory.html#v:copyFileWithMetadata
  -- I mean into .neohaskell
  Directory.copy [path|src|] rootFolder
    |> Task.mapError (\e -> CustomError (toText e))

  filepaths <-
    Directory.walk [path|src|]
      |> Task.mapError (\_ -> CustomError "WALK ERROR")

  let haskellFiles = filepaths |> Array.takeIf (Path.endsWith haskellExtension)

  -- -- haskellFiles = [A.hs,A/Lol.hs,A/B/Haha.hs,A/B/C/Rofl.hs]
  -- -- Convert to:
  -- -- modules = [A, A.Lol, A.B, A.B.C]
  let convertToModule filepath = do
        let pathText = Path.toText filepath
        -- first we remove the extension
        let pathWithoutExtension = Text.dropRight (Text.length haskellExtension) pathText
        -- then we split on the path separator
        let pathParts = Text.split "/" pathWithoutExtension
        -- then we convert each part to a module name
        pathParts |> Text.joinWith "."

  let modules = haskellFiles |> Array.map convertToModule
  let cabalFile = Cabal.template config modules

  Directory.create rootFolder
    |> Task.mapError (\_ -> [fmt|Could not create directory {Path.toText rootFolder}|] |> CustomError)

  File.writeText nixFilePath nixFile
    |> Task.mapError (\_ -> NixFileError)

  File.writeText cabalFilePath cabalFile
    |> Task.mapError (\_ -> CabalFileError)

  -- FIXME: Create another thread that renders the output of the build via streaming.
  -- As right now there's no output at all
  completion <- Subprocess.open "nix-build" (Array.fromLinkedList []) rootFolder
  -- completion <- Subprocess.open "nix-build" (Array.fromLinkedList ["-E", nixFile]) rootFolder
  if completion.exitCode != 0
    then errorOut completion.stderr
    else print completion.stdout


errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops the build failed:
    {err}|]
    |> CustomError
    |> Task.throw
