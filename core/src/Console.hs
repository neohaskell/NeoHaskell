module Console (print, readLine) where

import Basics
import Data.Text.IO qualified
import Text (Text)


print :: Text -> IO Unit
print text = Data.Text.IO.putStrLn text


readLine :: IO Text
readLine = Data.Text.IO.getLine