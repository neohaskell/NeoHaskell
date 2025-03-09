module Subprocess (
  OpenOptions (..),
  Completion (..),
  InheritStream (..),
  open,
  openInherit,
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


data InheritStream
  = InheritSTDOUT
  | InheritSTDERR
  | InheritBOTH
  | InheritNONE
  deriving (Eq, Ord, Show)


openInherit :: Text -> Array Text -> Path -> InheritStream -> Task _ Completion
openInherit executable arguments directory inheritStream = do
  let (stdoutStream, stderrStream) = case inheritStream of
        InheritSTDOUT -> (System.Process.Inherit, System.Process.CreatePipe)
        InheritSTDERR -> (System.Process.CreatePipe, System.Process.Inherit)
        InheritBOTH -> (System.Process.Inherit, System.Process.Inherit)
        InheritNONE -> (System.Process.CreatePipe, System.Process.CreatePipe)
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
                |> Maybe.Just,
            System.Process.std_out = stdoutStream,
            System.Process.std_err = stderrStream
          }
  (_, _, _, ph) <-
    -- System.Process.readCreateProcessWithExitCode processToExecute ""
    System.Process.createProcess processToExecute
      |> Task.fromIO
  ec <- System.Process.waitForProcess ph |> Task.fromIO
  let exitCode = case ec of
        System.Exit.ExitSuccess -> 0
        System.Exit.ExitFailure code -> code
  let stdout = ""
  let stderr = ""
  Task.yield Completion {exitCode, stdout, stderr}


open :: Text -> Array Text -> Path -> Task _ Completion
open executable arguments directory =
  openInherit executable arguments directory InheritNONE