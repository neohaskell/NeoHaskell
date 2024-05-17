-- | # Core
--
-- This module is automatically imported in all the NeoHaskell files,
module Core (
  module Reexported,
) where

import Basics as Reexported
import Function as Reexported
import Traits.Appendable as Reexported (
  Appendable,
  (++),
 )
import Traits.Default as Reexported (Default, defaultValue)
import Traits.Mappable as Reexported (Mappable, map)
import Traits.Thenable as Reexported (Thenable, andThen)
