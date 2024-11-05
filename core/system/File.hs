module File (
  Error (..),
  readTextHandler,
  ReadOptions (..),
  readText,
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


readTextHandler :: ReadOptions -> IO (Result Error Text)
readTextHandler options = do
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


writeText :: Path -> Text -> IO (Result Error ())
writeText path textToWrite = do
  result <-
    TIO.writeFile (Path.toLinkedList path) textToWrite
      |> Exception.try @Exception.IOError
  case result of
    Either.Left _ -> do
      print "[File.writeText] File not found"
      pure (Err NotFound)
    Either.Right _ -> do
      print [fmt|[[File.writeText] Written to file|]
      pure (Ok ())