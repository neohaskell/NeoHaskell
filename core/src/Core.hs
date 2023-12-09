-- | # Core
--
-- This module is automatically imported in all the NeoHaskell files,
module Core (
  module Reexported,
  module Record,
  module Operators,
) where

import Console as Reexported
import Debug as Reexported (todo)
import HaskellCompatibility.IO as Reexported (MainFunction)
import HaskellCompatibility.Syntax as Reexported
import Operators
import Record
import Types as Reexported
