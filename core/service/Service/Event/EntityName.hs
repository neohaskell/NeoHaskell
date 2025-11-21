module Service.Event.EntityName (
  EntityName (..),
  toText,
) where

import Basics
import Json qualified
import Text (Text)


newtype EntityName = EntityName Text
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON EntityName


instance Json.FromJSON EntityName


toText :: EntityName -> Text
toText (EntityName text) = text