module Uuid (
  Uuid,
  generate,
  toLegacy,
  fromLegacy,
  toText,
) where

import Basics
import Data.Default (Default (..))
import Data.Text (Text)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as V4
import Json (FromJSON, ToJSON)
import Task (Task)
import Task qualified


newtype Uuid = Uuid (UUID.UUID)
  deriving (Eq, Ord, Generic)


instance ToJSON Uuid


instance FromJSON Uuid


instance Show Uuid where
  show (Uuid uuid) = show uuid


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


toText :: Uuid -> Text
toText (Uuid uuid) = do
  UUID.toText uuid