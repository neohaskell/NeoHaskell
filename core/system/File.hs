module File (
  Error (..),
  readText,
  writeText,
) where

import Basics
import Data.Text.IO qualified as TIO
import GHC.IO.Exception qualified as Exception
import Path (Path)
import Path qualified
import Task (Task)
import Task qualified
import Text (Text)
import ToText (Show (..))


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
