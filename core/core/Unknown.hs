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
import Data.Typeable (TypeRep, Typeable, typeRep)
import Maybe (Maybe)
import Maybe qualified
import Text (Text)
import Text qualified
import ToText


-- | The 'Unknown' type represents a dynamically typed value.
data Unknown = Unknown
  { value :: Data.Dynamic.Dynamic,
    typeRepresentation :: TypeRep
  }
  deriving (Show, Typeable)


-- | The 'Convertible' type class represents types that can be converted to 'Unknown'.
type Convertible value = Typeable value


-- | Convert a value of any type to 'Unknown'.
fromValue :: (Typeable value) => value -> Unknown
fromValue value = do
  let dynamicValue = Data.Dynamic.toDyn value
  Unknown dynamicValue (Data.Dynamic.dynTypeRep dynamicValue)


-- | Convert an 'Unknown' value back to its original type, if possible.
toValue :: (Typeable value) => Unknown -> Maybe value
toValue (Unknown dynamic _) =
  Data.Dynamic.fromDynamic dynamic


-- | Applies a function that is inside the 'Unknown' value.
apply :: Unknown -> Unknown -> Maybe Unknown
apply (Unknown f _) (Unknown x _) =
  Data.Dynamic.dynApply f x
    |> Maybe.map \result -> Unknown result (Data.Dynamic.dynTypeRep result)


-- | Returns the name of the type
getTypeName ::
  forall a.
  (Typeable a) =>
  Text
getTypeName =
  typeRep (Data.Proxy.Proxy @a) |> toPrettyText


-- | Gets the name of the type of an unknown value
getUnknownTypeName :: Unknown -> Text
getUnknownTypeName (Unknown _ typeRepresentation) =
  toPrettyText typeRepresentation


instance (Convertible a, Convertible b) => Show (a -> b) where
  show _ = do
    let t = "(" ++ Unknown.getTypeName @a ++ " -> " ++ Unknown.getTypeName @b ++ ")"
    Text.toLinkedList t
