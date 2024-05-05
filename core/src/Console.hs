module Console (
  print,
) where

import Data.Text.IO qualified as Text
import Pipe ((|>))
import Text (Text)
import Unit (Unit)


print :: Text -> IO Unit
print message =
  Text.putStrLn message
