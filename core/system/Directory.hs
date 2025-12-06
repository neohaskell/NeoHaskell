module Directory (
  Error (..),
  CreateOptions (..),
  create,
  walk,
  copy,
  getCurrent,
) where

import Array (Array)
import Array qualified
import Basics
import Console qualified
import Control.Exception qualified as Exception
import Data.Either qualified as Either
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Verbosity qualified as CabalVerbosity
import GHC.IO.Exception qualified as Exception
import Maybe qualified
import Path (Path)
import Path qualified
import System.Directory qualified
import System.Directory.Internal.Prelude qualified as Exception
import System.Directory.Recursive qualified as RecursiveDir
import System.IO.Error (alreadyExistsErrorType)
import Task (Task)
import Task qualified
import Text qualified
import ToText qualified


data Error
  = NotFound
  | NotWritable
  | NotReadable
  | WalkError
  deriving (Show)


data CreateOptions = CreateOptions
  { path :: Path
  }
  deriving (Show)


create :: Path -> Task Error Unit
create dirPath = do
  let log m = Console.log m |> Task.fromIO
  let p = Path.toText dirPath
  log [fmt|[[Directory.create] Attempting to create directory: #{p}|]
  let createDirAction =
        dirPath
          |> Path.toLinkedList
          |> System.Directory.createDirectory
  result <- Exception.try @Exception.IOError createDirAction |> Task.fromIO
  case result of
    Either.Left err ->
      if Exception.ioeGetErrorType err == alreadyExistsErrorType
        then do
          pure unit
        else do
          log "[Directory.create] Failed to create directory"
          Task.throw NotWritable
    Either.Right _ -> do
      log [fmt|[[Directory.create] Directory created: #{p}|]
      Task.yield unit


copy :: Path -> Path -> Task Error Unit
copy src dest = do
  let log m = Console.log m |> Task.fromIO
  let srcP = Path.toText src
  let destP = Path.toText dest
  log [fmt|[[Directory.copy] Attempting to copy #{srcP} to #{destP}|]
  let copyFileAction =
        dest
          |> Path.toLinkedList
          |> Cabal.copyDirectoryRecursive CabalVerbosity.silent (Path.toLinkedList src)
  result <- Exception.try @Exception.IOError copyFileAction |> Task.fromIO
  case result of
    Either.Left err -> do
      log [fmt|[Directory.copy] Failed to copy #{srcP} to #{destP}: #{show err}|]
      Task.throw NotWritable
    Either.Right _ -> do
      log [fmt|[Directory.copy] Copied #{srcP} to #{destP}|]
      Task.yield unit


walk :: Path -> Task Error (Array Path)
walk dirPath = do
  let log m = Console.log m |> Task.fromIO
  let p = Path.toText dirPath
  let walkDirAction =
        dirPath
          |> Path.toLinkedList
          |> RecursiveDir.getDirRecursive
  result <- Exception.try @Exception.IOError walkDirAction |> Task.fromIO
  case result of
    Either.Left err -> do
      log [fmt|[Directory.walk] Failed to walk directory #{p}: #{show err}|]
      Task.throw WalkError
    Either.Right fps -> do
      log [fmt|[Directory.walk] Directory #{p} walked|]
      let paths = Array.fromLinkedList fps
      let numPathChars =
            if Text.endsWith "/" p
              then Text.length p
              else Text.length p + 1
      let trimInitialSrc pathElement =
            Text.fromLinkedList pathElement
              |> Text.dropLeft numPathChars
              |> Path.fromText
              |> Maybe.getOrDie
      log [fmt|[Directory.walk] Paths: #{ToText.toText paths}|]
      let trimmedPaths = paths |> Array.map trimInitialSrc
      log [fmt|[Directory.walk] Trimmed paths: #{ToText.toText trimmedPaths}|]
      Task.yield trimmedPaths


getCurrent :: Task Error Path
getCurrent = do
  let log m = Console.log m |> Task.fromIO
  log "[Directory.getCurrent] Getting current working directory"
  result <- Exception.try @Exception.IOError System.Directory.getCurrentDirectory |> Task.fromIO
  case result of
    Either.Left err -> do
      log [fmt|[Directory.getCurrent] Failed to get current directory: #{show err}|]
      Task.throw NotReadable
    Either.Right currentDir -> do
      log [fmt|[Directory.getCurrent] Current directory: #{currentDir}|]
      let result =
            currentDir
              |> Path.fromLinkedList
      case result of
        Maybe.Just path -> Task.yield path
        Maybe.Nothing -> Task.throw NotReadable
