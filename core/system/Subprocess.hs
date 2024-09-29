module Subprocess (
  OpenOptions (..),
  open,
  openHandler,
) where

import Action (Action)
import Action qualified
import Array (Array)
import Array qualified
import Basics
import IO (IO)
import IO qualified
import System.Exit qualified
import System.Process qualified
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
    arguments :: Array Text
  }
  deriving (Eq, Ord, Show)


open :: OpenOptions -> Action Completion
open options =
  options
    |> Action.named "Subprocess.open"


openHandler :: OpenOptions -> IO Completion
openHandler OpenOptions {executable, arguments} = do
  let exec = Text.toLinkedList executable
  let args = Array.map Text.toLinkedList arguments |> Array.toLinkedList
  (ec, out, err) <- System.Process.readProcessWithExitCode exec args ""
  let exitCode = case ec of
        System.Exit.ExitSuccess -> 0
        System.Exit.ExitFailure code -> code
  let stdout = Text.fromLinkedList out
  let stderr = Text.fromLinkedList err
  IO.yield Completion {exitCode, stdout, stderr}