module Default (
  Default (..),
) where

import Array qualified
import HaskellCompatibility.Syntax
import Types


-- | # Default
--
-- The `Default` trait is used to define the default value of a type.
-- It is useful for cases where we want to initialize a value of a type
-- without knowing the exact value.
--
-- All primitive types have a default value.
class Default value where
  defaultValue :: value


instance Default (Array item) where
  defaultValue = Array.empty


instance Default Bool where
  defaultValue = False


instance Default Char where
  defaultValue = '\0'


instance Default Int where
  defaultValue = 0


instance Default String where
  defaultValue = ""


instance Default Void where
  defaultValue = void