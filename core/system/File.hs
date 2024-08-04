module File (
  Error (..),
  writeTextHandler,
  readTextHandler,
  ReadOptions,
  readText,
  WriteTextOptions,
  writeText,
) where

import Action (Action)
import Action qualified
import Appendable ((++))
import Basics
import Control.Exception qualified as Exception
import Data.Either qualified as Either
import Data.Text.IO qualified as TIO
import GHC.IO.Exception qualified as Exception
import Path (Path)
import Path qualified
import Text (Text, toLinkedList)
import ToText (Show (..))
import Unknown qualified


data Error
  = NotFound
  | NotWritable
  | NotReadable
  deriving (Show)


type ReadOptions msg =
  Record
    '[ "path" := Path,
       "onSuccess" := (Text -> msg),
       "onError" := (Error -> msg)
     ]


instance (Unknown.Convertible a, Unknown.Convertible b) => Show (a -> b) where
  show _ = do
    let t = "(" ++ Unknown.getTypeName @a ++ " -> " ++ Unknown.getTypeName @b ++ ")"
    Text.toLinkedList t


readText :: (Unknown.Convertible msg) => ReadOptions msg -> Action msg
readText options =
  Action.named "File.readText" options


type WriteTextOptions msg =
  Record
    '[ "path" := Path,
       "text" := Text,
       "onSuccess" := msg,
       "onError" := (Error -> msg)
     ]


writeText :: (Unknown.Convertible msg) => WriteTextOptions msg -> Action msg
writeText options =
  Action.named "File.writeText" options


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
