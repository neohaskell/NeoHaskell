module Console (
  print,
) where

import Data.Text.IO qualified as Text
import Pipe ((|>))
import Promise (Promise)
import Promise qualified
import Str (Str)
import Str.Internal qualified as StrInternal
import Void (Void)


print :: Str -> Promise Void
print message =
  message
    |> StrInternal.extractText
    |> Text.putStrLn
    |> Promise.fromIO
