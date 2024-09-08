module File (
  Error (..),
  writeTextHandler,
  readTextHandler,
  ReadOptions (..),
  readText,
  WriteTextOptions (..),
  writeText,
) where

import Action (Action)
import Action qualified
import Basics
import Console (print)
import Control.Exception qualified as Exception
import Data.Either qualified as Either
import Data.Text.IO qualified as TIO
import GHC.IO.Exception qualified as Exception
import IO (IO)
import Path (Path)
import Path qualified
import Result (Result (..))
import Text (Text)
import ToText (Show (..))
import Unknown qualified


data Error
  = NotFound
  | NotWritable
  | NotReadable
  deriving (Show)


data ReadOptions = ReadOptions
  { path :: Path
  }
  deriving (Show)


readText :: ReadOptions -> Action (Result Error Text)
readText options =
  Action.named "File.readText" options


data WriteTextOptions event = WriteTextOptions
  { path :: Path,
    text :: Text,
    onSuccess :: event,
    onError :: (Error -> event)
  }
  deriving (Show)


writeText :: (Unknown.Convertible event) => WriteTextOptions event -> Action event
writeText options =
  Action.named "File.writeText" options


readTextHandler :: ReadOptions -> IO (Result Error Text)
readTextHandler options = do
  -- TODO: Figure out exceptions module and "raw IO" modules
  let p = Path.toText options.path
  print [fmt|[[File.readText] Attempting to read file: {p}|]
  result <- options.path |> Path.toLinkedList |> TIO.readFile |> Exception.try @Exception.IOError
  case result of
    Either.Left _ -> do
      print "[File.readText] File not found"
      pure (Err NotFound)
    Either.Right contents -> do
      print [fmt|[[File.readText] File contents: {contents}|]
      pure (Ok contents)


writeTextHandler :: (Unknown.Convertible event) => WriteTextOptions event -> IO event
writeTextHandler _ =
  dieWith "File.writeTextHandler is not implemented yet"
