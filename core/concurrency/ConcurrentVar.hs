module ConcurrentVar (
  ConcurrentVar,
  new,
  get,
  set,
  peek,
) where

import Basics
import Control.Concurrent.MVar qualified as GHC
import IO (IO)


newtype ConcurrentVar value = ConcurrentVar (GHC.MVar value)


new :: IO (ConcurrentVar value)
new = do
  ref <- GHC.newEmptyMVar
  pure (ConcurrentVar ref)


get :: ConcurrentVar value -> IO value
get (ConcurrentVar ref) = GHC.takeMVar ref


peek :: ConcurrentVar value -> IO value
peek (ConcurrentVar ref) = GHC.readMVar ref


set :: value -> ConcurrentVar value -> IO ()
set value (ConcurrentVar ref) = GHC.putMVar ref value
