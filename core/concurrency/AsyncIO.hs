module AsyncIO (AsyncIO, run, waitFor, sleep, process, waitAnyCancel, withRecovery, cancel) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent qualified as Ghc
import Control.Concurrent.Async qualified as GhcAsync
import Data.Either qualified as Either
import IO (IO)
import Result (Result)
import Result qualified


type AsyncIO result = GhcAsync.Async result


run :: IO result -> IO (AsyncIO result)
run = GhcAsync.async


waitFor :: AsyncIO result -> IO result
waitFor = GhcAsync.wait


process :: IO a -> (AsyncIO a -> IO b) -> IO b
process = GhcAsync.withAsync


sleep :: Int -> IO Unit
sleep milliseconds = Ghc.threadDelay (milliseconds * 1000)


withRecovery :: IO error -> IO result -> IO (Result error result)
withRecovery errorIO resultIO = do
  result <- GhcAsync.race errorIO resultIO
  case result of
    Either.Left a -> pure (Result.Err a)
    Either.Right a -> pure (Result.Ok a)


waitAnyCancel :: Array (AsyncIO a) -> IO (AsyncIO a, a)
waitAnyCancel arr = do
  let asyncList =
        Array.toLinkedList arr
  (async, result) <- GhcAsync.waitAnyCancel asyncList
  pure (async, result)


cancel :: AsyncIO a -> IO ()
cancel = GhcAsync.cancel
