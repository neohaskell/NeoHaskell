module DurableChannel (DurableChannel, new, read, write) where

import Array (Array)
import Array qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Task (Task)
import Task qualified


data DurableChannel value = DurableChannel
  { channel :: Channel value,
    values :: ConcurrentVar (Array value)
  }


new :: Task _ (DurableChannel value)
new = do
  channel <- Channel.new
  values <- ConcurrentVar.containing Array.empty
  Task.yield DurableChannel {channel, values}


read :: DurableChannel value -> Task _ value
read self =
  Channel.read self.channel


write :: value -> DurableChannel value -> Task _ Unit
write value self = do
  self.values
    |> ConcurrentVar.modify (Array.append value)

  self.channel
    |> Channel.write value
