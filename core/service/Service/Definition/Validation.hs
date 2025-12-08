{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.Definition.Validation (
  -- * Protocol validation
  ValidateProtocols,
) where

import Basics
import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Service.Definition.TypeLevel (Difference)

-- | Type family that validates all required protocols have adapters
-- Either passes silently or produces a compile-time error
type family ValidateProtocols (required :: [Symbol]) (provided :: [Symbol]) :: Constraint where
  ValidateProtocols required provided =
    CheckMissingProtocols (Difference required provided) required provided

-- | Helper type family that checks if there are missing protocols and generates appropriate error
type family CheckMissingProtocols (missing :: [Symbol]) (required :: [Symbol]) (provided :: [Symbol]) :: Constraint where
  CheckMissingProtocols '[] required provided = ()
  CheckMissingProtocols missing required provided =
    TypeError (
      'Text "Missing protocol adapters!" ':$$:
      'Text "" ':$$:
      'Text "Required protocols: " ':<>: 'ShowType required ':$$:
      'Text "Provided protocols: " ':<>: 'ShowType provided ':$$:
      'Text "Missing protocols:  " ':<>: 'ShowType missing ':$$:
      'Text "" ':$$:
      'Text "To fix: Use 'useServer' to add adapters for the missing protocols."
    )