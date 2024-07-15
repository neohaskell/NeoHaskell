module File (
  write,
  read,
  Error (..),
  readHandler,
  writeHandler,
) where

import Basics
import Command (Command)
import Command qualified
import Data.Text.IO qualified
import Path
import Result (Result (..))
import Text (Text)


data Error
  = NotFound
  | NotWritable
  | NotReadable


read :: Path -> Command (Result Error Text)
read path =
  path
    |> Command.named "File.read"


write :: Path -> Text -> Command (Result Error Unit)
write path text =
  (path, text)
    |> Command.named "File.write"


readHandler :: Path -> IO Text
readHandler path =
  Path.toLinkedList path
    |> Data.Text.IO.readFile


writeHandler :: Path -> Text -> IO Unit
writeHandler path text = do
  let fp = Path.toLinkedList path
  Data.Text.IO.writeFile fp text