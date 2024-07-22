module File (
  Error (..),
  writeTextHandler,
  readTextHandler,
  ReadOptions,
  readText,
  WriteTextOptions,
  writeText,
) where

import Basics
import Command (Command)
import Command qualified
import Control.Exception qualified as Exception
import Data.Either qualified as Either
import Data.Text.IO qualified as TIO
import GHC.IO.Exception qualified as Exception
import Path (Path)
import Path qualified
import Text (Text)
import Unknown qualified


data Error
  = NotFound
  | NotWritable
  | NotReadable


type ReadOptions msg =
  Record
    '[ "path" := Path,
       "onSuccess" := (Text -> msg),
       "onError" := (Error -> msg)
     ]


readText :: (Unknown.Convertible msg) => ReadOptions msg -> Command msg
readText options =
  Command.named "File.readText" options


type WriteTextOptions msg =
  Record
    '[ "path" := Path,
       "text" := Text,
       "onSuccess" := msg,
       "onError" := (Error -> msg)
     ]


writeText :: (Unknown.Convertible msg) => WriteTextOptions msg -> Command msg
writeText options =
  Command.named "File.writeText" options


readTextHandler ::
  forall msg.
  (Unknown.Convertible msg) =>
  ReadOptions msg ->
  IO msg
readTextHandler options = do
  -- TODO: Figure out exceptions module and "raw IO" modules
  result <- options.path |> Path.toLinkedList |> TIO.readFile |> Exception.try @Exception.IOError
  case result of
    Either.Left _ -> pure (options.onError NotFound)
    Either.Right contents -> pure (options.onSuccess contents)


writeTextHandler :: (Unknown.Convertible msg) => WriteTextOptions msg -> IO msg
writeTextHandler _ =
  dieWith "File.writeTextHandler is not implemented yet"
