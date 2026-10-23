module Directory (
  Error (..),
  CreateOptions (..),
  create,
  createIfMissing,
  walk,
  copy,
  getCurrent,
  removeRecursive,
  withTemporaryFolder,
) where

import Array (Array)
import Array qualified
import Basics
import Char (Char)
import Console qualified
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Verbosity qualified as CabalVerbosity
import LinkedList (LinkedList)
import Maybe qualified
import Path (Path)
import Path qualified
import Prelude qualified
import Result qualified
import System.Directory qualified
import System.Directory.Recursive qualified as RecursiveDir
import System.FilePath qualified
import System.IO.Error qualified as IOError
import System.Posix.Directory qualified as PosixDirectory
import System.Posix.Files qualified as PosixFiles
import Task (Task)
import Task qualified
import Text qualified
import ToText qualified
import Uuid qualified


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
  result <- createDirAction |> Task.fromFailableIO @IOError.IOError |> Task.asResult
  case result of
    Result.Err err ->
      if IOError.isAlreadyExistsError err
        then do
          pure unit
        else do
          log "[Directory.create] Failed to create directory"
          Task.throw NotWritable
    Result.Ok _ -> do
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
  result <- copyFileAction |> Task.fromFailableIO @IOError.IOError |> Task.asResult
  case result of
    Result.Err err -> do
      log [fmt|[Directory.copy] Failed to copy #{srcP} to #{destP}: #{show err}|]
      Task.throw NotWritable
    Result.Ok _ -> do
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
  result <- walkDirAction |> Task.fromFailableIO @IOError.IOError |> Task.asResult
  case result of
    Result.Err err -> do
      log [fmt|[Directory.walk] Failed to walk directory #{p}: #{show err}|]
      Task.throw WalkError
    Result.Ok fps -> do
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
  result <- System.Directory.getCurrentDirectory |> Task.fromFailableIO @IOError.IOError |> Task.asResult
  case result of
    Result.Err err -> do
      log [fmt|[Directory.getCurrent] Failed to get current directory: #{show err}|]
      Task.throw NotReadable
    Result.Ok currentDir -> do
      log [fmt|[Directory.getCurrent] Current directory: #{currentDir}|]
      let result =
            currentDir
              |> Path.fromLinkedList
      case result of
        Maybe.Just path -> Task.yield path
        Maybe.Nothing -> Task.throw NotReadable


createIfMissing :: Path -> Task Error Unit
createIfMissing dirPath = do
  let createDirAction =
        dirPath
          |> Path.toLinkedList
          |> System.Directory.createDirectoryIfMissing True
  createDirAction
    |> Task.fromFailableIO @IOError.IOError
    |> Task.mapError (\_ -> NotWritable)


removeRecursive :: Path -> Task Error Unit
removeRecursive dirPath = do
  let removeDirAction =
        dirPath
          |> Path.toLinkedList
          |> System.Directory.removeDirectoryRecursive
  removeDirAction
    |> Task.fromFailableIO @IOError.IOError
    |> Task.mapError (\_ -> NotWritable)


-- | Create a private folder for one task and attempt to remove it after every
-- exit, including typed errors, runtime exceptions, and cancellation.
withTemporaryFolder :: (Error -> err) -> (Path -> Task err value) -> Task err value
withTemporaryFolder mapError use = Task.mask \restore -> do
  folder <- newTemporaryFolder |> Task.mapError mapError
  Task.finally (removeRecursive folder) do
    callback <- Task.evaluate (use folder)
    restore callback


newTemporaryFolder :: Task Error Path
newTemporaryFolder = do
  temporaryRoot <-
    System.Directory.getTemporaryDirectory
      |> Task.fromFailableIO @IOError.IOError
      |> Task.mapError (\_ -> NotReadable)
  absoluteRoot <-
    System.Directory.makeAbsolute temporaryRoot
      |> Task.fromFailableIO @IOError.IOError
      |> Task.mapError (\_ -> NotReadable)
  reserveTemporaryFolder 3 absoluteRoot


reserveTemporaryFolder :: Int -> LinkedList Char -> Task Error Path
reserveTemporaryFolder retriesLeft temporaryRoot = do
  nonce <- Uuid.generate
  let name = "neohaskell-" Prelude.++ Text.toLinkedList (Uuid.toText nonce)
  let filePath = System.FilePath.combine temporaryRoot name
  candidate <-
    case Path.fromLinkedList filePath of
      Maybe.Just path -> Task.yield path
      Maybe.Nothing -> Task.throw NotWritable
  created <-
    PosixDirectory.createDirectory filePath PosixFiles.ownerModes
      |> Task.fromFailableIO @IOError.IOError
      |> Task.asResult
  case created of
    Result.Ok _ -> Task.yield candidate
    Result.Err err ->
      if IOError.isAlreadyExistsError err && retriesLeft > 0
        then reserveTemporaryFolder (retriesLeft - 1) temporaryRoot
        else Task.throw NotWritable
