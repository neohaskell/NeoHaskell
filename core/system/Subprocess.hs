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
import Control.Exception qualified as GhcException
import Data.Foldable qualified as GhcFoldable
import IO qualified
import GHC.IO.Exception qualified as Exception
import GHC.IO.Handle qualified as GhcHandle
import Maybe (Maybe (..))
import Path (Path)
import Path qualified
import Result (Result (..))
import Result qualified
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
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\err -> ProcessError [fmt|#{err}|])
  case result of
    Nothing -> Task.throw (TimeoutError [fmt|Process timed out after #{timeoutSeconds} seconds|])
    Just completion -> Task.yield completion


-- | Acquire the child under masking, run the deadline, and preserve the primary exception.
captureProcess :: Int -> System.Process.CreateProcess -> IO.IO (Maybe Completion)
captureProcess timeoutMicros processToExecute = GhcException.mask \restore -> do
  resources <- System.Process.createProcess processToExecute
  outcome <- IO.try (restore (GhcTimeout.timeout timeoutMicros (collectProcess resources)))
  -- Cleanup must not replace the original asynchronous or reader exception.
  cleanup <- GhcAsync.withAsync (releaseProcess resources) awaitProcessCleanup
  case outcome of
    Err original -> preserveProcessException original cleanup
    Ok result -> case cleanup of
      Err failure -> GhcException.throwIO failure
      Ok () -> pure result


-- Keep cleanup owned until it finishes, even if cancellation is repeated or any
-- other exception is delivered while waiting. The worker remains interruptible;
-- no OS wait is masked uninterruptibly.
-- | Wait for the owned cleanup worker without abandoning it on any interruption.
awaitProcessCleanup :: GhcAsync.Async Unit -> IO.IO (Result GhcException.SomeException Unit)
awaitProcessCleanup worker =
  GhcException.catch
    (GhcAsync.waitCatch worker |> IO.map Result.fromEither)
    (finishProcessCleanup worker)


-- | Finish releasing resources before reporting an exception received during cleanup.
-- 'throwTo' can deliver any exception type to the waiting thread, while
-- 'waitCatch' already returns worker failures as values; the handler therefore
-- does not classify interruption by type.
finishProcessCleanup ::
  GhcAsync.Async Unit -> GhcException.SomeException -> IO.IO (Result GhcException.SomeException Unit)
finishProcessCleanup worker interruption = do
  _ <- awaitProcessCleanup worker
  pure (Err interruption)


-- | Give asynchronous cancellation precedence over synchronous cleanup failures.
preserveProcessException :: GhcException.SomeException -> Result GhcException.SomeException Unit -> IO.IO value
preserveProcessException original cleanup =
  case GhcException.fromException @GhcException.SomeAsyncException original of
    Just _ -> GhcException.throwIO original
    Nothing -> case cleanup of
      Ok () -> GhcException.throwIO original
      Err failure -> case GhcException.fromException @GhcException.SomeAsyncException failure of
        Just _ -> GhcException.throwIO failure
        Nothing -> GhcException.throwIO original


-- | Drain both captured streams strictly before collecting the child exit status.
collectProcess ::
  (Maybe GhcHandle.Handle, Maybe GhcHandle.Handle, Maybe GhcHandle.Handle, System.Process.ProcessHandle) ->
  IO.IO Completion
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
releaseProcess ::
  (Maybe GhcHandle.Handle, Maybe GhcHandle.Handle, Maybe GhcHandle.Handle, System.Process.ProcessHandle) ->
  IO.IO Unit
releaseProcess resources = do
  let (stdinHandle, stdoutHandle, stderrHandle, child) = resources
  stopProcess child `GhcException.finally`
    (closeProcessPipe stdinHandle `GhcException.finally`
      (closeProcessPipe stdoutHandle `GhcException.finally` closeProcessPipe stderrHandle))


-- | Close a created pipe; inherited streams are represented by Nothing.
closeProcessPipe :: Maybe GhcHandle.Handle -> IO.IO Unit
closeProcessPipe handle = GhcFoldable.for_ handle GhcHandle.hClose


-- | Give a live direct child one second to terminate before forced termination and reap.
stopProcess :: System.Process.ProcessHandle -> IO.IO Unit
stopProcess child = do
  status <- System.Process.getProcessExitCode child
  case status of
    Just _ -> pure ()
    Nothing -> do
      signalOwnedProcess child (System.Process.terminateProcess child)
      exited <- GhcTimeout.timeout 1000000 (System.Process.waitForProcess child)
      case exited of
        Just _ -> pure ()
        Nothing -> do
          signalOwnedProcess child (forceProcess child)
          _ <- System.Process.waitForProcess child
          pure ()


-- | Recheck child completion if a termination signal races with exit.
signalOwnedProcess :: System.Process.ProcessHandle -> IO.IO Unit -> IO.IO Unit
signalOwnedProcess child signal = GhcException.catch signal (handleProcessSignalError child)


-- | Accept an already reaped child, but propagate a genuine signalling failure.
handleProcessSignalError :: System.Process.ProcessHandle -> Exception.IOError -> IO.IO Unit
handleProcessSignalError child failure = do
  status <- System.Process.getProcessExitCode child
  case status of
    Just _ -> pure ()
    Nothing -> GhcException.throwIO failure


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
