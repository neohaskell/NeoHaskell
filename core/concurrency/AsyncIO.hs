module AsyncIO (AsyncIO, run, waitFor) where

import Control.Concurrent.Async qualified as GhcAsync
import Core


type AsyncIO result = GhcAsync.Async result


run :: IO result -> IO (AsyncIO result)
run = GhcAsync.async


waitFor :: AsyncIO result -> IO result
waitFor = GhcAsync.wait