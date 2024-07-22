module AsyncIO (AsyncIO, run, waitFor, sleep) where

import Basics
import Control.Concurrent qualified as Ghc
import Control.Concurrent.Async qualified as GhcAsync

type AsyncIO result = GhcAsync.Async result

run :: IO result -> IO (AsyncIO result)
run = GhcAsync.async

waitFor :: AsyncIO result -> IO result
waitFor = GhcAsync.wait

sleep :: Int -> IO Unit
sleep microseconds = Ghc.threadDelay microseconds