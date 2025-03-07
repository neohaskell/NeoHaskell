module Subprocess (
  OpenOptions (..),
  Completion (..),
  open,
) where

import Array (Array)
import Array qualified
import Basics
import Maybe qualified
import Path (Path)
import Path qualified
import System.Exit qualified
import System.Process qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (Show)


data Completion = Completion
  { exitCode :: Int,
    stdout :: Text,
    stderr :: Text
  }
  deriving (Eq, Ord, Show)


data OpenOptions = OpenOptions
  { executable :: Text,
    arguments :: Array Text,
    directory :: Path
  }
  deriving (Eq, Ord, Show)


open :: Text -> Array Text -> Path -> Task _ Completion
open executable arguments directory = do
  let exec = Text.toLinkedList executable
  let args = Array.map Text.toLinkedList arguments |> Array.toLinkedList
  let processToExecute =
        ( System.Process.proc
            exec
            args
        )
          { System.Process.cwd =
              directory
                |> Path.toLinkedList
                |> Maybe.Just
          }
  (ec, out, err) <-
    System.Process.readCreateProcessWithExitCode processToExecute ""
      |> Task.fromIO
  let exitCode = case ec of
        System.Exit.ExitSuccess -> 0
        System.Exit.ExitFailure code -> code
  let stdout = Text.fromLinkedList out
  let stderr = Text.fromLinkedList err
  Task.yield Completion {exitCode, stdout, stderr}