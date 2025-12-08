{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.Definition.Validation (
  -- * Server API validation
  ValidateServers,
) where

import Basics
import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Service.Definition.TypeLevel (Difference)

-- | Type family that validates all required servers are provided
-- Either passes silently or produces a compile-time error
type family ValidateServers (required :: [Type]) (provided :: [Type]) :: Constraint where
  ValidateServers required provided =
    CheckMissingServers (Difference required provided) required provided

-- | Helper type family that checks if there are missing servers and generates appropriate error
type family CheckMissingServers (missing :: [Type]) (required :: [Type]) (provided :: [Type]) :: Constraint where
  CheckMissingServers '[] required provided = ()
  CheckMissingServers missing required provided =
    TypeError (
      'Text "Missing servers!" ':$$:
      'Text "" ':$$:
      'Text "Required servers: " ':<>: 'ShowType required ':$$:
      'Text "Provided servers: " ':<>: 'ShowType provided ':$$:
      'Text "Missing servers:  " ':<>: 'ShowType missing ':$$:
      'Text "" ':$$:
      'Text "To fix: Use 'useServer' to register the missing servers."
    )