module DurableChannel (DurableChannel, new, read, write, last, checkAndWrite, getAndTransform) where

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


writeNoLock :: value -> DurableChannel value -> Task _ Unit
writeNoLock value self = do
  self.values
    |> ConcurrentVar.modify (Array.push value)
  self.channel
    |> Channel.write value


checkAndWrite ::
  (Array value -> Bool) ->
  value ->
  DurableChannel value ->
  Task _ Bool
checkAndWrite predicate value self =
  Lock.with self.lock do
    values <- ConcurrentVar.peek self.values
    if predicate values
      then do
        writeNoLock value self
        Task.yield True
      else do
        Task.yield False


-- | Gets the last element in the channel.
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
