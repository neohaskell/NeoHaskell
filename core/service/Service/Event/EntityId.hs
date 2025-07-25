module Service.Event.EntityId (
  EntityId (..),
  new,
) where

import Core
import Task qualified
import Uuid qualified


newtype EntityId = EntityId Uuid
  deriving (Eq, Show, Ord, Generic)


new :: Task _ EntityId
new = do
  uuid <- Uuid.generate
  EntityId uuid |> Task.yield
