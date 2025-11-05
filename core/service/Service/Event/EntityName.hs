module Service.Event.EntityName (
  EntityName (..),
) where

import Core


newtype EntityName = EntityName Text
  deriving (Eq, Show, Ord, Generic)
