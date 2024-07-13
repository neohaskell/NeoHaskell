module ConcurrentVar (
  ConcurrentVar,
  new,
  get,
  set,
) where

import Basics
import Control.Concurrent.MVar qualified as GHC


newtype ConcurrentVar value = ConcurrentVar (GHC.MVar value)


new :: IO (ConcurrentVar value)
new = do
  ref <- GHC.newEmptyMVar
  pure (ConcurrentVar ref)


get :: ConcurrentVar value -> IO value
get (ConcurrentVar ref) = GHC.readMVar ref


set :: value -> ConcurrentVar value -> IO ()
set value (ConcurrentVar ref) = GHC.putMVar ref value
