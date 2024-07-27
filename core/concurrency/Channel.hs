module Channel (Channel, new, read, write) where

import Basics
import Control.Concurrent.Chan.Unagi qualified as Unagi
import ToText (Show (..))


type Channel value =
  Record
    '[ "outChannel" := Unagi.OutChan value,
       "inChannel" := Unagi.InChan value
     ]


instance Show (Unagi.OutChan value) where
  show _ = "[OutChan]"


instance Show (Unagi.InChan value) where
  show _ = "[InChan]"


new :: IO (Channel value)
new = do
  (in_, out) <- Unagi.newChan
  pure ANON {outChannel = out, inChannel = in_}


read :: Channel value -> IO value
read self = Unagi.readChan (self.outChannel)


write :: value -> Channel value -> IO ()
write value self = Unagi.writeChan (self.inChannel) value
