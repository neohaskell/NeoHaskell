module Service.Event.EntityName (
  EntityName (..),
  toText,
) where

import Core hiding (toText)


newtype EntityName = EntityName Text
  deriving (Eq, Show, Ord, Generic)


toText :: EntityName -> Text
toText (EntityName text) = text