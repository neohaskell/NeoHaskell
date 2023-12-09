module Console (
  print,
) where

import Data.Text.IO qualified as Text
import HaskellCompatibility.String (applyToText)
import Pipe ((|>))
import Promise (Promise)
import Promise qualified
import String (String)
import Void (Void)


print :: String -> Promise Void
print message =
  message
    |> applyToText Text.putStrLn
    |> Promise.fromIO
