module Main where

import Core
import qualified Neo
import Service qualified


main :: IO Unit
main =
  Service.run
    Service.UserApp
      { init = Neo.init,
        view = Neo.view,
        triggers = Neo.triggers,
        update = Neo.update
      }
