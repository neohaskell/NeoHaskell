module DurableChannel (DurableChannel, new, read, write) where

import Array (Array)
import Array qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Lock (Lock)
import Lock qualified
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
write value self = Lock.with self.lock do
  self.values
    |> ConcurrentVar.modify (Array.push value)

  self.channel
    |> Channel.write value
