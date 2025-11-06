module Uuid (
  Uuid,
  generate,
  toLegacy,
  fromLegacy,
) where

import Basics
import Data.Default (Default (..))
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as V4
import Task (Task)
import Task qualified
import ToText (Show)


newtype Uuid = Uuid (UUID.UUID)
  deriving (Eq, Ord, Show)


instance Default Uuid where
  def = UUID.nil |> Uuid


generate :: Task _ Uuid
generate = do
  uuid <- Task.fromIO V4.nextRandom
  Task.yield (Uuid uuid)


toLegacy :: Uuid -> UUID.UUID
toLegacy (Uuid uuid) = uuid


fromLegacy :: UUID.UUID -> Uuid
fromLegacy uuid = Uuid uuid