module Neo where

import Console qualified
import Core
import Neo.Model qualified as Model


run :: Task Text ()
run = do
  Console.print [fmt|Hello, #{Model.model}|]