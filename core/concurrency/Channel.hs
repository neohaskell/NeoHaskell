module Channel (Channel, new, read, write) where

import Basics
import Control.Concurrent.Chan.Unagi qualified as Unagi
import Task (Task)
import Task qualified
import ToText (Show (..))
import TypeName qualified


data Channel value = Channel
  { outChannel :: Unagi.OutChan value,
    inChannel :: Unagi.InChan value
  }
  deriving (Show)


instance (TypeName.Inspectable value) => Show (Unagi.OutChan value) where
  show _ = do
    let typeName = TypeName.reflect @value
    [fmt|[OutChan {typeName}]|]


instance (TypeName.Inspectable value) => Show (Unagi.InChan value) where
  show _ = do
    let typeName = TypeName.reflect @value
    [fmt|[InChan {typeName}]|]


new :: Task _ (Channel value)
new = do
  (inChannel, outChannel) <- Task.fromIO Unagi.newChan
  Task.yield Channel {outChannel, inChannel}


read :: Channel value -> Task _ value
read self =
  Unagi.readChan (self.outChannel)
    |> Task.fromIO


write :: value -> Channel value -> Task _ Unit
write value self =
  Unagi.writeChan (self.inChannel) value
    |> Task.fromIO
