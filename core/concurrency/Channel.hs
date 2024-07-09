module Channel (Channel, new, read, write) where

import Control.Concurrent.Chan.Unagi qualified as Unagi
import Core


type Channel value =
  Record
    '[ "outChannel" := Unagi.OutChan value,
       "inChannel" := Unagi.InChan value
     ]


new :: IO (Channel value)
new = do
  (in_, out) <- Unagi.newChan
  pure ANON {outChannel = out, inChannel = in_}


read :: Channel value -> IO value
read self = Unagi.readChan (self.outChannel)


write :: value -> Channel value -> IO ()
write value self = Unagi.writeChan (self.inChannel) value
