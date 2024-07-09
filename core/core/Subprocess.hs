module Subprocess (open) where

import Array (Array)
import Array qualified
import Basics
import IO qualified
import System.Exit qualified
import System.Process qualified
import Text (Text)
import Text qualified


data Completion = Completion
  { exitCode :: Int,
    stdout :: Text,
    stderr :: Text
  }
  deriving (Eq, Ord)


open :: Text -> Array Text -> IO Completion
open executable arguments = do
  let exec = Text.toLinkedList executable
  let args = Array.map Text.toLinkedList arguments |> Array.toLinkedList
  (ec, out, err) <- System.Process.readProcessWithExitCode exec args ""
  let exitCode = case ec of
        System.Exit.ExitSuccess -> 0
        System.Exit.ExitFailure code -> code
  let stdout = Text.fromLinkedList out
  let stderr = Text.fromLinkedList err
  IO.yield Completion {exitCode, stdout, stderr}