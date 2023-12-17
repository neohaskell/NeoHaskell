-- | This module is required for the GHC extension RebindableSyntax.
-- It is required in order to rebind all the syntax that is used in the
-- NeoHaskell core library, in order to provide sensible defaults to
-- types like the NeoHaskell Array type, or the NeoHaskell String type.
--
-- It also provides functions in order to rebind things like do blocks.
module HaskellCompatibility.Syntax (
  -- * List
  fromList,
  fromListN,
  toList,

  -- * Monad
  (>>),
  (>>=),

  -- * String
  fromString,

  -- * Numbers
  fromInteger,
) where

import HaskellCompatibility.List (fromList, fromListN, toList)
import HaskellCompatibility.Monad ((>>), (>>=))
import HaskellCompatibility.Numbers (fromInteger)
import HaskellCompatibility.String (fromString)
