module Directory
  ( Error (..),
    createAction,
    CreateOptions (..),
    create,
  )
where

import Action (Action)
import Action qualified
import Basics
import Console qualified
import Control.Exception qualified as Exception
import Data.Either qualified as Either
import GHC.IO.Exception qualified as Exception
import Path (Path)
import Path qualified
import Result (Result (..))
import System.Directory qualified
import System.Directory.Internal.Prelude qualified as Exception
import System.IO.Error (alreadyExistsErrorType)
import Task (Task)
import Task qualified
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

createAction :: CreateOptions -> Action (Result Error Unit)
createAction options =
  Action.named "Directory.create" options

create :: Path -> Task Error Unit
create dirPath = do
  let log m = Console.log m |> Task.fromIO
  let p = Path.toText dirPath
  log [fmt|[[Directory.create] Attempting to create directory: {p}|]
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
      log [fmt|[[Directory.create] Directory created: {p}|]
      pure unit
