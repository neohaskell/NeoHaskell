module DurableChannel (DurableChannel, new, read, write, last, checkAndWrite, getAndTransform, writeWithIndex, modify) where

import Array (Array)
import Array qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Lock (Lock)
import Lock qualified
import Maybe (Maybe)
import Task (Task)
import Task qualified


data DurableChannel value = DurableChannel
  { channel :: Channel value,
    values :: ConcurrentVar (Array value),
    lock :: Lock
  }


new :: Task _ (DurableChannel value)
new = do
  channel <- Channel.new
  values <- ConcurrentVar.containing Array.empty
  lock <- Lock.new
  Task.yield DurableChannel {channel, values, lock}


read :: DurableChannel value -> Task _ value
read self =
  Channel.read self.channel


write :: value -> DurableChannel value -> Task _ Unit
write value self =
  Lock.with self.lock do
    writeNoLock value self


-- | Like write, but passes the index of the value to the function
-- so that the function can return the indexed value.
writeWithIndex :: (Int -> value) -> DurableChannel value -> Task _ Int
writeWithIndex f self = do
  Lock.with self.lock do
    let modifier v = do
          let index = Array.length v
          Task.yield (v, index)
    index <-
      self.values
        |> ConcurrentVar.modifyReturning modifier
    let value = f index
    self |> writeNoLock value
    Task.yield index


writeNoLock :: value -> DurableChannel value -> Task _ Unit
writeNoLock value self = do
  -- Write to channel first to ensure consistency
  self.channel
    |> Channel.write value
  self.values
    |> ConcurrentVar.modify (Array.push value)


checkAndWrite ::
  (Array value -> Bool) ->
  value ->
  DurableChannel value ->
  Task _ Bool
checkAndWrite predicate value self =
  Lock.with self.lock do
    let modifier values = do
          if predicate values
            then do
              let newValues = Array.push value values
              Task.yield (newValues, True)
            else do
              Task.yield (values, False)

    wasWritten <-
      self.values
        |> ConcurrentVar.modifyReturning modifier

    if wasWritten
      then do
        self.channel
          |> Channel.write value
        Task.yield True
      else do
        Task.yield False


-- | Gets the last element in the channel.
-- Note: This operation is not synchronized with writes and may return
-- stale data if called during concurrent modifications.
last :: DurableChannel value -> Task _ (Maybe value)
last self = do
  values <- ConcurrentVar.peek self.values
  Task.yield (Array.last values)


-- | Allows transforming and returning the array inside a lock
getAndTransform :: (Array value -> Array value) -> DurableChannel value -> Task _ (Array value)
getAndTransform transform self =
  Lock.with self.lock do
    values <- ConcurrentVar.peek self.values
    values
      |> transform
      |> Task.yield


-- | Modifies the channel array atomically
modify :: (Array value -> Array value) -> DurableChannel value -> Task _ Unit
modify transform self =
  Lock.with self.lock do
    values <- ConcurrentVar.peek self.values
    self.values |> ConcurrentVar.set (transform values)