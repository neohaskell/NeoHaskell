module Var (Var, new, get, set, increment, decrement) where

import Basics
import Data.IORef qualified as GHC
import Task (Task)
import Task qualified


newtype Var value = Var (GHC.IORef value)


new :: value -> Task err (Var value)
new value = Task.fromIO do
  ref <- GHC.newIORef value
  pure (Var ref)


get :: Var value -> Task err value
get (Var ref) =
  GHC.readIORef ref
    |> Task.fromIO


set :: value -> Var value -> Task err Unit
set value (Var ref) =
  GHC.writeIORef ref value
    |> Task.fromIO


increment ::
  (Num number) =>
  Var number -> Task err Unit
increment (Var ref) =
  GHC.modifyIORef ref (+ 1)
    |> Task.fromIO


decrement ::
  (Num number) =>
  Var number -> Task err Unit
decrement (Var ref) =
  GHC.modifyIORef ref (\n -> n - 1)
    |> Task.fromIO