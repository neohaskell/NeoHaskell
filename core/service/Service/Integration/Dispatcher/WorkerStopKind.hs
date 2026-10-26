
-- | Names the worker variant when a Stop message cannot be delivered.
--
-- The stop path calls 'label' when reporting an undelivered Stop message.
module Service.Integration.Dispatcher.WorkerStopKind (
  WorkerStopKind (..),
  label,
) where

import Text (Text)


data WorkerStopKind = StatelessWorker | LifecycleWorker


label :: WorkerStopKind -> Text
label kind = case kind of
  StatelessWorker -> "worker"
  LifecycleWorker -> "lifecycle worker"
