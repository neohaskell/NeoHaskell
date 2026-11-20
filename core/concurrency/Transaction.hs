-- | Composable atomic operations on shared state.
--
-- Build a 'Transaction' from operations such as 'AtomicVar.peekSTM' and
-- 'AtomicVar.modifyWithResultSTM'. Run the combined operation with 'run', or
-- pass it to a 'ConcurrentMap' predicate to commit both objects together.
module Transaction (
  Transaction,
  run,
) where

import Basics
import Control.Concurrent.STM qualified as GhcSTM
import Task (Task)
import Task qualified
import Transaction.Internal (Transaction (..))


-- | Commit every operation in the transaction as one atomic update.
run ::
  forall err value.
  Transaction value ->
  Task err value
run (Transaction action) =
  GhcSTM.atomically action
    |> Task.fromIO
