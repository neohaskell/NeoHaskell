module File (write, read) where

import Basics
import Data.Text.IO qualified
import Path
import Text (Text)


read :: Path to File -> IO Text
read path =
  Path.toLinkedList path
    |> Data.Text.IO.readFile


write :: Path to File -> Text -> IO Unit
write path text = do
  let fp = Path.toLinkedList path
  Data.Text.IO.writeFile fp text