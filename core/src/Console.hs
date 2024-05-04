module Console (
  print,
) where

import Data.Text.IO qualified as Text
import HaskellCompatibility.String (applyToText)
import Pipe ((|>))
import Promise (Promise)
import Promise qualified
import Text (Text)
import Void (Void)


print :: Text -> Promise Void
print message =
  message
    |> applyToText Text.putStrLn
    |> Promise.fromIO
