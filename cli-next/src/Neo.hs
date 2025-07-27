module Neo where

import qualified Console
import Core
import qualified Neo.Model as Model


run :: Task Text ()
run = do
  Console.print [fmt|Hello, #{Model.model}|]
