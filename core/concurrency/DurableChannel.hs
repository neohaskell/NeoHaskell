module DurableChannel (DurableChannel, new, read, write) where

import Array (Array, fromLinkedList)
import Basics
import Channel (Channel)
import Channel qualified
import Task (Task)
import Task qualified
import ToText (Show (..))


data DurableChannel value = DurableChannel
  { channel :: Channel value,
    values :: Array value
  }
  deriving (Show)


new :: Task _ (DurableChannel value)
new = do
  channel <- Channel.new
  let values = Array.fromLinkedList []
  Task.yield DurableChannel {channel, values}


read :: DurableChannel value -> Task _ value
read self =
  Channel.read self.channel


write :: value -> DurableChannel value -> Task _ Unit
write _ _ =
  panic "not defined yet"
