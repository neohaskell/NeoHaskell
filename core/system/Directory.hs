module Directory (
  Error (..),
  createHandler,
  CreateOptions (..),
  create,
) where

import Action (Action)
import Action qualified
import Basics
import Console (print)
import Control.Exception qualified as Exception
import Data.Either qualified as Either
import GHC.IO.Exception qualified as Exception
import IO (IO)
import Path (Path)
import Path qualified
import Result (Result (..))
import System.Directory qualified
import System.Directory.Internal.Prelude qualified as Exception
import System.IO.Error (alreadyExistsErrorType)
import ToText (Show (..))


data Error
  = NotFound
  | NotWritable
  | NotReadable
  deriving (Show)


data CreateOptions = CreateOptions
  { path :: Path
  }
  deriving (Show)


create :: CreateOptions -> Action (Result Error Unit)
create options =
  Action.named "Directory.create" options


createHandler :: CreateOptions -> IO (Result Error Unit)
createHandler options = do
  let p = Path.toText options.path
  print [fmt|[[Directory.create] Attempting to create directory: {p}|]
  let createDirAction =
        options.path
          |> Path.toLinkedList
          |> System.Directory.createDirectory
  result <- Exception.try @Exception.IOError createDirAction
  case result of
    Either.Left err ->
      if Exception.ioeGetErrorType err == alreadyExistsErrorType
        then do
          pure (Ok unit)
        else do
          print "[Directory.create] Failed to create directory"
          pure (Err NotWritable)
    Either.Right _ -> do
      print [fmt|[[Directory.create] Directory created: {p}|]
      pure (Ok unit)
