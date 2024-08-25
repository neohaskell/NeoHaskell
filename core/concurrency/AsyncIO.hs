module AsyncIO (AsyncIO, run, waitFor, sleep, process, waitAnyCancel) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent qualified as Ghc
import Control.Concurrent.Async qualified as GhcAsync


type AsyncIO result = GhcAsync.Async result


run :: IO result -> IO (AsyncIO result)
run = GhcAsync.async


waitFor :: AsyncIO result -> IO result
waitFor = GhcAsync.wait


process :: IO a -> (AsyncIO a -> IO b) -> IO b
process = GhcAsync.withAsync


sleep :: Int -> IO Unit
sleep milliseconds = Ghc.threadDelay (milliseconds * 1000)


waitAnyCancel :: Array (AsyncIO a) -> IO (AsyncIO a, a)
waitAnyCancel arr = do
  let asyncList =
        Array.toLinkedList arr
  (async, result) <- GhcAsync.waitAnyCancel asyncList
  pure (async, result)