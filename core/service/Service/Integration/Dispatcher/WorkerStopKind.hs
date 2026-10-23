{-# LANGUAGE Strict #-}

-- | Names the worker variant when a Stop message cannot be delivered.
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
