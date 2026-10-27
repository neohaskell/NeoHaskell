-- | Shared event and command types for outbound payload dispatch tests.
-- Handler fixtures use these types to exercise the generated event mapping.
module Service.OutboundIntegration.PayloadFixtures where

import Core
import Json qualified
import Service.Entity.Core qualified as Entity


data PayloadEntity = PayloadEntity {total :: Int}
  deriving (Generic)


instance Json.FromJSON PayloadEntity


instance Json.ToJSON PayloadEntity


instance Default PayloadEntity where
  def = PayloadEntity {total = 0}


data PayloadAdded = PayloadAdded {addedId :: Uuid, addedAmount :: Int}
  deriving (Eq)


deriveEvent ''PayloadAdded


type instance EntityOf PayloadAdded = PayloadEntity


data PayloadRemoved = PayloadRemoved {removedId :: Uuid}
  deriving (Eq)


deriveEvent ''PayloadRemoved


type instance EntityOf PayloadRemoved = PayloadEntity


deriveEntityEvents ''PayloadEntity [''PayloadAdded, ''PayloadRemoved]


instance Entity PayloadEntity where
  initialStateImpl = PayloadEntity {total = 0}
  updateImpl eventValue entity = case eventValue of
    PayloadEventPayloadAdded payload -> entity {total = entity.total + payload.addedAmount}
    PayloadEventPayloadRemoved _ -> entity


instance Entity.Event PayloadAdded where
  getEventEntityIdImpl payload = payload.addedId


instance Entity.Event PayloadRemoved where
  getEventEntityIdImpl payload = payload.removedId


data PayloadCommand = PayloadCommand {commandValue :: Int}
  deriving (Generic, Eq)


instance Json.ToJSON PayloadCommand


instance Json.FromJSON PayloadCommand


type instance NameOf PayloadCommand = "PayloadCommand"
