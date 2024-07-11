module Unknown
  ( Unknown,
    fromValue,
    toValue,
  )
where

import Data.Dynamic qualified
import Data.Typeable (Typeable)
import Maybe (Maybe)
import ToText

newtype Unknown = Unknown Data.Dynamic.Dynamic
  deriving (Show, Typeable)

fromValue :: (Typeable value) => value -> Unknown
fromValue value =
  Unknown (Data.Dynamic.toDyn value)

toValue :: (Typeable value) => Unknown -> Maybe value
toValue (Unknown dynamic) =
  Data.Dynamic.fromDynamic dynamic