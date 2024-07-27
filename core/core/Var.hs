module Var (Var, new, get, set) where

import Basics
import Data.IORef qualified as GHC


newtype Var value = Var (GHC.IORef value)


new :: value -> IO (Var value)
new value = do
  ref <- GHC.newIORef value
  pure (Var ref)


get :: Var value -> IO value
get (Var ref) = GHC.readIORef ref


set :: value -> Var value -> IO ()
set value (Var ref) = GHC.writeIORef ref value
