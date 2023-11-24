module HaskellCompatibility.IO (
  module Reexported,
  MainFunction,
) where

import GHC.Types as Reexported (IO)
import Void (Void)


-- | Type alias for the main function.
type MainFunction = IO Void
