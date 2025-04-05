module Uuid (
  Uuid,
  generate,
) where

import Basics
import ToText(Show)
import Task (Task)
import Task qualified
import Data.UUID.V4 qualified as V4
import Data.UUID qualified as UUID

newtype Uuid = Uuid (UUID.UUID)
  deriving (Eq, Ord, Show)


generate :: Task _ Uuid
generate = do
  uuid <- Task.fromIO V4.nextRandom
  Task.yield (Uuid uuid)
