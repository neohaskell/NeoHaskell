module Console (print, readLine) where

import Appendable ((++))
import Basics
import Data.Text.IO qualified
import GHC.Stack (HasCallStack, callStack, getCallStack, prettySrcLoc)
import LinkedList qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (toText)


print :: (HasCallStack) => Text -> IO Unit
print text = do
  let maybeLoc = callStack |> getCallStack |> LinkedList.get 0
  case maybeLoc of
    Just (_, loc) -> do
      let coolLoc = _
      let message = "[" ++ (prettySrcLoc loc |> toText) ++ "]: " ++ text
      Data.Text.IO.putStrLn message
    _ -> pure ()


readLine :: IO Text
readLine = Data.Text.IO.getLine