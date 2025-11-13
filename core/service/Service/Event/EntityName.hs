module Service.Event.EntityName (
  EntityName (..),
  toText,
) where

import Core hiding (toText)
import Json qualified


newtype EntityName = EntityName Text
  deriving (Eq, Show, Ord, Generic)


instance Json.FromJSON EntityName


toText :: EntityName -> Text
toText (EntityName text) = text