-- | # Core
--
-- This module is automatically imported in all the NeoHaskell files,
module Core (
  module Reexported,
  module Record,
) where

import Console as Reexported
import Debug as Reexported (todo)
import HaskellCompatibility.IO as Reexported (MainFunction)
import Types as Reexported
import Record
