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
import Path
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


readTextHandler :: forall msg. (Unknown.Convertible msg) => ReadOptions msg -> IO msg
readTextHandler _ =
  dieWith "File.readTextHandler is not implemented yet"


writeTextHandler :: (Unknown.Convertible msg) => WriteTextOptions msg -> IO msg
writeTextHandler _ =
  dieWith "File.writeTextHandler is not implemented yet"
