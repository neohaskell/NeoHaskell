{-# LANGUAGE CPP #-}

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
import Char (Char)
import Control.Concurrent.Async qualified as GhcAsync
import Data.Foldable qualified as GhcFoldable
import IO qualified
import GHC.IO.Exception qualified as Exception
import GHC.IO.Handle qualified as GhcHandle
import Maybe (Maybe (..))
import Path (Path)
import Path qualified
import Result (Result (..))
import System.Directory qualified as GhcDir
import System.Exit qualified
import System.Process qualified
#ifndef mingw32_HOST_OS
import System.Posix.Signals qualified as Posix
#endif
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

  result <- captureProcess timeoutMicros processToExecute
    |> Task.mapError (\err -> ProcessError [fmt|#{err}|])
  case result of
    Nothing -> Task.throw (TimeoutError [fmt|Process timed out after #{timeoutSeconds} seconds|])
    Just completion -> Task.yield completion


-- | The pipes that were created for a child, and the child itself.
type ProcessResources =
  (Maybe GhcHandle.Handle, Maybe GhcHandle.Handle, Maybe GhcHandle.Handle, System.Process.ProcessHandle)


-- | Acquire the child with cancellation deferred, run the deadline, and release the child on every exit.
captureProcess :: Int -> System.Process.CreateProcess -> Task Exception.IOError (Maybe Completion)
captureProcess timeoutMicros processToExecute = Task.withCancellationDeferred \restore -> do
  resources <-
    System.Process.createProcess processToExecute
      |> Task.fromFailableIO @Exception.IOError
  result <-
    GhcTimeout.timeout timeoutMicros (collectProcess resources)
      |> Task.fromFailableIO @Exception.IOError
      |> restore
      |> Task.onFailure (releaseAfterFailure resources)
  releaseReportingFailure resources
  Task.yield result


-- | Release the child after a failed capture without replacing the failure the caller must see.
-- A cancelled capture keeps its original cancellation. Any other failure gives way only to
-- cancellation that arrives during cleanup, because 'Task.onFailure' ignores the rest.
releaseAfterFailure :: ProcessResources -> Task.Failure Exception.IOError -> Task Exception.IOError Unit
releaseAfterFailure resources failure = case failure of
  Task.Interrupted -> releaseKeepingCancellation resources
  Task.TypedFailure _ -> releaseReportingFailure resources
  Task.RuntimeFailure -> releaseReportingFailure resources


-- | Release the child and report a cleanup failure or an exception delivered during cleanup.
releaseReportingFailure :: ProcessResources -> Task Exception.IOError Unit
releaseReportingFailure resources = do
  released <- releaseOwnedProcess resources |> Task.fromFailableIO @Exception.IOError
  case released of
    Ok _ -> Task.yield unit
    Err failure -> Task.throw failure


-- | Release the child and discard whatever cleanup reports, including later interruptions.
releaseKeepingCancellation :: ProcessResources -> Task Exception.IOError Unit
releaseKeepingCancellation resources =
  releaseOwnedProcess resources
    |> IO.map (\_ -> unit)
    |> IO.catchAny (\_ -> IO.yield unit)
    |> Task.fromIO


-- | Run cleanup in an owned worker and wait for it without abandoning it on any interruption.
-- Cleanup stays owned until it finishes, even if cancellation is repeated or any other
-- exception is delivered while waiting. The worker remains interruptible; no OS wait is
-- masked uninterruptibly.
releaseOwnedProcess :: ProcessResources -> IO.IO (Result Exception.IOError Unit)
releaseOwnedProcess resources =
  GhcAsync.withAsync (Task.runResult (releaseProcess resources)) awaitProcessCleanup


-- | Wait for the cleanup worker. The first exception delivered during the wait
-- continues only after cleanup has finished.
awaitProcessCleanup :: GhcAsync.Async (Result Exception.IOError Unit) -> IO.IO (Result Exception.IOError Unit)
awaitProcessCleanup worker =
  IO.finally (GhcAsync.wait worker) (finishProcessCleanup worker)


-- | Finish waiting for cleanup whatever is delivered meanwhile.
-- 'throwTo' can deliver any exception type to the waiting thread, so the handler
-- does not classify interruption by type.
finishProcessCleanup :: GhcAsync.Async (Result Exception.IOError Unit) -> IO.IO Unit
finishProcessCleanup worker =
  GhcAsync.waitCatch worker
    |> IO.map (\_ -> unit)
    |> IO.catchAny (\_ -> finishProcessCleanup worker)


-- | Drain both captured streams strictly before collecting the child exit status.
collectProcess :: ProcessResources -> IO.IO Completion
collectProcess resources = do
  let (_, stdoutHandle, stderrHandle, child) = resources
  (stdoutContent, stderrContent) <- GhcAsync.concurrently
    (readProcessPipe stdoutHandle) (readProcessPipe stderrHandle)
  status <- System.Process.waitForProcess child
  let code = case status of
        System.Exit.ExitSuccess -> 0
        System.Exit.ExitFailure value -> value
  pure Completion
    { exitCode = code
    , stdout = Text.fromLinkedList stdoutContent
    , stderr = Text.fromLinkedList stderrContent
    }


-- | Finish reading a captured stream in its scoped reader thread.
readProcessPipe :: Maybe GhcHandle.Handle -> IO.IO [Char]
readProcessPipe handle = case handle of
  Nothing -> pure ""
  Just stream -> GhcHandle.hGetContents' stream


-- | Stop and reap the direct child, then attempt every owned handle close.
releaseProcess :: ProcessResources -> Task Exception.IOError Unit
releaseProcess resources = do
  let (stdinHandle, stdoutHandle, stderrHandle, child) = resources
  let closePipes =
        IO.finally
          (closeProcessPipe stdinHandle)
          (IO.finally (closeProcessPipe stdoutHandle) (closeProcessPipe stderrHandle))
  stopProcess child
    |> Task.finally (Task.fromIO closePipes)


-- | Close a created pipe; inherited streams are represented by Nothing.
closeProcessPipe :: Maybe GhcHandle.Handle -> IO.IO Unit
closeProcessPipe handle = GhcFoldable.for_ handle GhcHandle.hClose


-- | Give a live direct child one second to terminate before forced termination and reap.
stopProcess :: System.Process.ProcessHandle -> Task Exception.IOError Unit
stopProcess child = do
  status <- System.Process.getProcessExitCode child |> Task.fromFailableIO @Exception.IOError
  case status of
    Just _ -> Task.yield unit
    Nothing -> do
      signalOwnedProcess child (System.Process.terminateProcess child)
      exited <-
        GhcTimeout.timeout 1000000 (System.Process.waitForProcess child)
          |> Task.fromFailableIO @Exception.IOError
      case exited of
        Just _ -> Task.yield unit
        Nothing -> do
          signalOwnedProcess child (forceProcess child)
          System.Process.waitForProcess child
            |> Task.fromFailableIO @Exception.IOError
            |> Task.map (\_ -> unit)


-- | Recheck child completion if a termination signal races with exit.
signalOwnedProcess :: System.Process.ProcessHandle -> IO.IO Unit -> Task Exception.IOError Unit
signalOwnedProcess child signal =
  signal
    |> Task.fromFailableIO @Exception.IOError
    |> Task.recover (handleProcessSignalError child)


-- | Accept an already reaped child, but propagate a genuine signalling failure.
handleProcessSignalError :: System.Process.ProcessHandle -> Exception.IOError -> Task Exception.IOError Unit
handleProcessSignalError child failure = do
  status <- System.Process.getProcessExitCode child |> Task.fromFailableIO @Exception.IOError
  case status of
    Just _ -> Task.yield unit
    Nothing -> Task.throw failure


-- | Force only the owned direct child using the platform termination API.
forceProcess :: System.Process.ProcessHandle -> IO.IO Unit
forceProcess child = do
#ifdef mingw32_HOST_OS
  System.Process.terminateProcess child
#else
  pid <- System.Process.getPid child
  GhcFoldable.for_ pid (Posix.signalProcess Posix.sigKILL)
#endif


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
