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
import Default as Reexported
import HaskellCompatibility.IO as Reexported (MainFunction)
<<<<<<< HEAD
import HaskellCompatibility.Syntax as Reexported
=======
import HaskellCompatibility.Generic as Reexported (Generic(..))
import Types as Reexported
import Kinds as Reexported
import Record
>>>>>>> e20f7b4 (Add kinds and generic)
import Operators
import Record
import Traits.Addable as Reexported
import Types as Reexported
