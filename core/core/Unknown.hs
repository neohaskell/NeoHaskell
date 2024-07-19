module Unknown (
  Unknown,
  fromValue,
  toValue,
  Convertible,
  apply,
) where

import Basics
import Data.Dynamic qualified
import Data.Typeable (Typeable)
import Maybe (Maybe)
import Maybe qualified
import ToText


-- | The 'Unknown' type represents a dynamically typed value.
newtype Unknown = Unknown Data.Dynamic.Dynamic
  deriving (Show, Typeable)


-- | The 'Convertible' type class represents types that can be converted to 'Unknown'.
type Convertible value = Typeable value


-- | Convert a value of any type to 'Unknown'.
--
-- >>> fromValue 42
-- Unknown (Data.Dynamic.toDyn 42)
--
-- >>> fromValue "hello"
-- Unknown (Data.Dynamic.toDyn "hello")
fromValue :: (Typeable value) => value -> Unknown
fromValue value =
  Unknown (Data.Dynamic.toDyn value)


-- | Convert an 'Unknown' value back to its original type, if possible.
--
-- >>> let unknown = fromValue 42
-- >>> toValue unknown :: Maybe Int
-- Just 42
--
-- >>> let unknown = fromValue "hello"
-- >>> toValue unknown :: Maybe Int
-- Nothing
toValue :: (Typeable value) => Unknown -> Maybe value
toValue (Unknown dynamic) =
  Data.Dynamic.fromDynamic dynamic


-- | Applies a function that is inside the 'Unknown' value.
apply :: Unknown -> Unknown -> Maybe Unknown
apply (Unknown f) (Unknown x) =
  Data.Dynamic.dynApply f x
    |> Maybe.map Unknown