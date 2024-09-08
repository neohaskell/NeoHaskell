{-# LANGUAGE AllowAmbiguousTypes #-}

module Unknown (
  Unknown,
  fromValue,
  toValue,
  Convertible,
  apply,
  getTypeName,
  getUnknownTypeName,
) where

import Appendable ((++))
import Basics
import Data.Dynamic qualified
import Data.Proxy qualified
import Data.Typeable (Typeable, typeRep)
import Maybe (Maybe)
import Maybe qualified
import Text (Text)
import Text qualified
import ToText


-- | The 'Unknown' type represents a dynamically typed value.
newtype Unknown = Unknown Data.Dynamic.Dynamic
  deriving (Show, Typeable)


-- | The 'Convertible' type class represents types that can be converted to 'Unknown'.
type Convertible value = Typeable value


-- | Convert a value of any type to 'Unknown'.
fromValue :: (Typeable value) => value -> Unknown
fromValue value =
  Unknown (Data.Dynamic.toDyn value)


-- | Convert an 'Unknown' value back to its original type, if possible.
toValue :: (Typeable value) => Unknown -> Maybe value
toValue (Unknown dynamic) =
  Data.Dynamic.fromDynamic dynamic


-- | Applies a function that is inside the 'Unknown' value.
apply :: Unknown -> Unknown -> Maybe Unknown
apply (Unknown f) (Unknown x) =
  Data.Dynamic.dynApply f x
    |> Maybe.map Unknown


-- | Returns the name of the type
getTypeName ::
  forall a.
  (Typeable a) =>
  Text
getTypeName =
  typeRep (Data.Proxy.Proxy @a) |> toText


-- | Gets the name of the type of an unknown value
getUnknownTypeName :: Unknown -> Text
getUnknownTypeName (Unknown dynamic) =
  Data.Dynamic.dynTypeRep dynamic |> toText


instance (Convertible a, Convertible b) => Show (a -> b) where
  show _ = do
    let t = "(" ++ Unknown.getTypeName @a ++ " -> " ++ Unknown.getTypeName @b ++ ")"
    Text.toLinkedList t