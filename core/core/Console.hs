module Console (print, readLine) where

import Appendable ((++))
import Basics
import Data.Text.IO qualified
import GHC.Stack qualified as Stack
import IO (IO)
import LinkedList qualified
import Maybe (Maybe (..))
import Maybe qualified
import System.Environment qualified
import Text (Text, fromLinkedList)
import ToText (toText)


-- TODO: Make this use a centralized monitoring thread
print :: (Stack.HasCallStack) => Text -> IO Unit
print text = do
  -- we check that the NEOHASKELL_DEBUG environment variable is set to true
  -- if it isn´t we dont do anything
  maybeDebug <- System.Environment.lookupEnv "NEOHASKELL_DEBUG"
  let debug = maybeDebug |> Maybe.withDefault "false"

  if debug == "true"
    then do
      -- we get the location of the call

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
    else pure ()


readLine :: IO Text
readLine = Data.Text.IO.getLine