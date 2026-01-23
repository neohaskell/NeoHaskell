module File (
  Error (..),
  readText,
  writeText,
  readBytes,
  writeBytes,
  exists,
  rename,
  deleteIfExists,
) where

import Basics
import Bytes (Bytes)
import Bytes qualified
import Data.ByteString qualified as GhcByteString
import Data.Text.IO qualified as TIO
import GHC.IO.Exception qualified as Exception
import Path (Path)
import Path qualified
import System.Directory qualified as GhcDir
import System.IO.Error qualified as GhcIOError
import Task (Task)
import Task qualified
import Text (Text)


data Error
  = NotFound
  | NotWritable
  | NotReadable
  deriving (Show)


readText :: Path -> Task Error Text
readText filepath =
  filepath
    |> Path.toLinkedList
    |> TIO.readFile
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotReadable)


writeText :: Path -> Text -> Task Error ()
writeText path textToWrite =
  TIO.writeFile (Path.toLinkedList path) textToWrite
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotWritable)


readBytes :: Path -> Task Error Bytes
readBytes filepath =
  filepath
    |> Path.toLinkedList
    |> GhcByteString.readFile
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotReadable)
    |> Task.map Bytes.fromLegacy


writeBytes :: Path -> Bytes -> Task Error ()
writeBytes filePath bytes = do
  let action = GhcByteString.writeFile (Path.toLinkedList filePath) (Bytes.unwrap bytes)
  action
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotWritable)


exists :: Path -> Task Error Bool
exists filePath =
  GhcDir.doesFileExist (Path.toLinkedList filePath)
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotReadable)


rename :: Path -> Path -> Task Error ()
rename source dest =
  GhcDir.renameFile (Path.toLinkedList source) (Path.toLinkedList dest)
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotWritable)


deleteIfExists :: Path -> Task Error ()
deleteIfExists filePath =
  GhcDir.removeFile (Path.toLinkedList filePath)
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError mapDeleteError
    |> Task.recover handleDeleteError
  where
    -- Map IOError to our Error type, preserving NotFound for recovery
    mapDeleteError :: Exception.IOError -> Error
    mapDeleteError ioErr =
      if GhcIOError.isDoesNotExistError ioErr
        then NotFound
        else NotWritable

    -- Recover from NotFound (file already gone = success), rethrow others
    handleDeleteError :: Error -> Task Error ()
    handleDeleteError err = case err of
      NotFound -> Task.yield ()
      other -> Task.throw other
