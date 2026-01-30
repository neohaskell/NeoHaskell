module Subprocess (
  -- * Types
  OpenOptions (..),
  Completion (..),
  InheritStream (..),
  Error (..),

  -- * Running Processes
  open,
  openInherit,
  run,
  runWithTimeout,

  -- * Tool Discovery
  which,
) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent.Async qualified as GhcAsync
import GHC.IO.Exception qualified as Exception
import GHC.IO.Handle qualified as GhcHandle
import Maybe (Maybe (..))
import Path (Path)
import Path qualified
import Result (Result (..))
import System.Directory qualified as GhcDir
import System.Exit qualified
import System.Process qualified
import System.Timeout qualified as GhcTimeout
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Result of running a subprocess.
data Completion = Completion
  { exitCode :: Int
  , stdout :: Text
  , stderr :: Text
  }
  deriving (Eq, Ord, Show)


-- | Options for opening a subprocess.
data OpenOptions = OpenOptions
  { executable :: Text
  , arguments :: Array Text
  , directory :: Path
  }
  deriving (Eq, Ord, Show)


-- | Stream inheritance options.
data InheritStream
  = InheritSTDOUT
  | InheritSTDERR
  | InheritBOTH
  | InheritNONE
  deriving (Eq, Ord, Show)


-- | Subprocess errors.
data Error
  = ProcessError Text
  | TimeoutError Text
  | ToolNotFound Text
  deriving (Show, Eq)


-- | Run a subprocess and capture stdout/stderr.
--
-- This is the recommended function for most use cases:
--
-- @
-- result <- Subprocess.run "pdftotext" ["-layout", "input.pdf", "-"] "."
-- case result.exitCode of
--   0 -> -- success, use result.stdout
--   _ -> -- error, check result.stderr
-- @
run :: Text -> Array Text -> Path -> Task Error Completion
run executable arguments directory =
  openInherit executable arguments directory InheritNONE


-- | Run a subprocess with a timeout (in seconds).
--
-- Returns 'TimeoutError' if the process doesn't complete within the timeout.
--
-- @
-- result <- Subprocess.runWithTimeout 30 "pdftotext" ["-layout", "input.pdf", "-"] "."
-- @
runWithTimeout :: Int -> Text -> Array Text -> Path -> Task Error Completion
runWithTimeout timeoutSeconds executable arguments directory = do
  let exec = Text.toLinkedList executable
  let args = Array.map Text.toLinkedList arguments |> Array.toLinkedList
  let timeoutMicros = timeoutSeconds * 1000000
  let processToExecute =
        (System.Process.proc exec args)
          { System.Process.cwd = directory |> Path.toLinkedList |> Just
          , System.Process.std_out = System.Process.CreatePipe
          , System.Process.std_err = System.Process.CreatePipe
          }

  -- Run with timeout
  result <-
    ( do
        (_, mStdout, mStderr, ph) <-
          System.Process.createProcess processToExecute
            |> Task.fromFailableIO @Exception.IOError
            |> Task.mapError (\err -> ProcessError [fmt|#{err}|])

        -- Use async to read both streams and wait for process
        completionResult <- Task.fromIO do
          -- Start reading streams in background
          stdoutAsync <- case mStdout of
            Nothing -> GhcAsync.async (pure "")
            Just h -> GhcAsync.async (GhcHandle.hGetContents h)

          stderrAsync <- case mStderr of
            Nothing -> GhcAsync.async (pure "")
            Just h -> GhcAsync.async (GhcHandle.hGetContents h)

          -- Wait for process with timeout
          maybeResult <- GhcTimeout.timeout timeoutMicros do
            stdoutContent <- GhcAsync.wait stdoutAsync
            stderrContent <- GhcAsync.wait stderrAsync
            exitCode <- System.Process.waitForProcess ph
            pure (exitCode, stdoutContent, stderrContent)

          case maybeResult of
            Nothing -> do
              -- Timeout - terminate the process
              System.Process.terminateProcess ph
              _ <- System.Process.waitForProcess ph
              pure Nothing
            Just (ec, stdoutContent, stderrContent) -> do
              let code = case ec of
                    System.Exit.ExitSuccess -> 0
                    System.Exit.ExitFailure c -> c
              pure (Just (code, Text.fromLinkedList stdoutContent, Text.fromLinkedList stderrContent))

        Task.yield completionResult
    )
      |> Task.asResult

  case result of
    Err err -> Task.throw err
    Ok Nothing -> Task.throw (TimeoutError [fmt|Process timed out after #{timeoutSeconds} seconds|])
    Ok (Just (code, stdoutText, stderrText)) ->
      Task.yield
        Completion
          { exitCode = code
          , stdout = stdoutText
          , stderr = stderrText
          }


-- | Check if a tool is available in PATH.
--
-- Returns the full path to the executable if found.
--
-- @
-- maybePath <- Subprocess.which "pdftotext"
-- case maybePath of
--   Just path -> -- tool is available at path
--   Nothing -> -- tool not found
-- @
which :: Text -> Task Error (Maybe Text)
which toolName = do
  let name = Text.toLinkedList toolName
  result <-
    GhcDir.findExecutable name
      |> Task.fromFailableIO @Exception.IOError
      |> Task.mapError (\err -> ProcessError [fmt|#{err}|])
  case result of
    Nothing -> Task.yield Nothing
    Just path -> Task.yield (Just (Text.fromLinkedList path))


-- | Run a subprocess with stream inheritance options.
--
-- This is a lower-level function that allows controlling which streams
-- are captured vs inherited from the parent process.
openInherit :: Text -> Array Text -> Path -> InheritStream -> Task Error Completion
openInherit executable arguments directory inheritStream = do
  let (stdoutStream, stderrStream) = case inheritStream of
        InheritSTDOUT -> (System.Process.Inherit, System.Process.CreatePipe)
        InheritSTDERR -> (System.Process.CreatePipe, System.Process.Inherit)
        InheritBOTH -> (System.Process.Inherit, System.Process.Inherit)
        InheritNONE -> (System.Process.CreatePipe, System.Process.CreatePipe)
  let exec = Text.toLinkedList executable
  let args = Array.map Text.toLinkedList arguments |> Array.toLinkedList
  let processToExecute =
        (System.Process.proc exec args)
          { System.Process.cwd = directory |> Path.toLinkedList |> Just
          , System.Process.std_out = stdoutStream
          , System.Process.std_err = stderrStream
          }

  (_, mStdout, mStderr, ph) <-
    System.Process.createProcess processToExecute
      |> Task.fromFailableIO @Exception.IOError
      |> Task.mapError (\err -> ProcessError [fmt|#{err}|])

  -- Read stdout if we have a pipe
  stdoutContent <- case mStdout of
    Nothing -> Task.yield ""
    Just h ->
      GhcHandle.hGetContents h
        |> Task.fromFailableIO @Exception.IOError
        |> Task.mapError (\err -> ProcessError [fmt|#{err}|])
        |> Task.map Text.fromLinkedList

  -- Read stderr if we have a pipe
  stderrContent <- case mStderr of
    Nothing -> Task.yield ""
    Just h ->
      GhcHandle.hGetContents h
        |> Task.fromFailableIO @Exception.IOError
        |> Task.mapError (\err -> ProcessError [fmt|#{err}|])
        |> Task.map Text.fromLinkedList

  -- Wait for process to complete
  ec <-
    System.Process.waitForProcess ph
      |> Task.fromFailableIO @Exception.IOError
      |> Task.mapError (\err -> ProcessError [fmt|#{err}|])

  let exitCode = case ec of
        System.Exit.ExitSuccess -> 0
        System.Exit.ExitFailure code -> code

  Task.yield Completion {exitCode, stdout = stdoutContent, stderr = stderrContent}


-- | Run a subprocess and capture output (legacy name for compatibility).
open :: Text -> Array Text -> Path -> Task Error Completion
open executable arguments directory =
  openInherit executable arguments directory InheritNONE
