module Service.Event.EntityName (
  EntityName (..),
  toText,
) where

import Basics
import Data.Hashable (Hashable)
import Json qualified
import Text (Text)


newtype EntityName = EntityName Text
  deriving (Eq, Show, Ord, Generic)


instance Hashable EntityName


instance Json.ToJSON EntityName


instance Json.FromJSON EntityName


toText :: EntityName -> Text
toText (EntityName text) = text