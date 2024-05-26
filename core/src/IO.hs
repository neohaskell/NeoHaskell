module IO (IO, yield) where

import GHC.IO (IO)
import Prelude qualified


yield :: value -> IO value
yield = Prelude.pure