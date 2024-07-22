module Console (print, readLine) where

import Appendable ((++))
import Basics
import Data.Text.IO qualified
import GHC.Stack qualified as Stack
import LinkedList qualified
import Maybe (Maybe (..))
import Text (Text, fromLinkedList)
import ToText (toText)

print :: (Stack.HasCallStack) => Text -> IO Unit
print text = do
  let maybeLoc =
        Stack.callStack
          |> Stack.getCallStack
          |> LinkedList.get 0

  case maybeLoc of
    Just (_, loc) -> do
      let coolLoc =
            Text.fromLinkedList (Stack.srcLocPackage loc)
              ++ ":"
              ++ Text.fromLinkedList (Stack.srcLocFile loc)
              ++ ":"
              ++ (Stack.srcLocStartLine loc |> toText)
              ++ ":"
              ++ (Stack.srcLocStartCol loc |> toText)
      let message = "[" ++ (coolLoc) ++ "]: " ++ text
      Data.Text.IO.putStrLn message
    _ -> pure ()

readLine :: IO Text
readLine = Data.Text.IO.getLine