module Testbed.ExternalEffect.Core (
  ExternalEffectEntity (..),
  ExternalEffectEvent (..),
  ExternalEffectState (..),
  initialState,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified


data ExternalEffectEntity = ExternalEffectEntity
  { effectId :: Uuid,
    description :: Text,
    amount :: Int,
    status :: ExternalEffectState
  }
  deriving (Generic)


instance Json.FromJSON ExternalEffectEntity


instance Json.ToJSON ExternalEffectEntity


instance Default ExternalEffectEntity where
  def = initialState


type instance NameOf ExternalEffectEntity = "ExternalEffectEntity"


instance Entity ExternalEffectEntity where
  initialStateImpl = initialState
  updateImpl = update


data ExternalEffectState
  = NotRequested
  | Requested
  | Delivered Text
  | Failed
  deriving (Eq, Generic, Show)


instance Json.FromJSON ExternalEffectState


instance Json.ToJSON ExternalEffectState


data ExternalEffectEvent
  = ExternalEffectRequested
      { entityId :: Uuid,
        description :: Text,
        amount :: Int
      }
  | ExternalEffectDelivered
      { entityId :: Uuid,
        externalId :: Text
      }
  | ExternalEffectFailed
      { entityId :: Uuid
      }
  deriving (Eq, Generic, Ord, Show)


instance Json.FromJSON ExternalEffectEvent


instance Json.ToJSON ExternalEffectEvent


type instance EventOf ExternalEffectEntity = ExternalEffectEvent


type instance EntityOf ExternalEffectEvent = ExternalEffectEntity


instance Event ExternalEffectEvent where
  getEventEntityIdImpl = (.entityId)


initialState :: ExternalEffectEntity
initialState =
  ExternalEffectEntity
    { effectId = Uuid.nil,
      description = "",
      amount = 0,
      status = NotRequested
    }


update :: ExternalEffectEvent -> ExternalEffectEntity -> ExternalEffectEntity
update event entity = case event of
  ExternalEffectRequested {entityId, description, amount} ->
    ExternalEffectEntity
      { effectId = entityId,
        description = description,
        amount = amount,
        status = Requested
      }
  ExternalEffectDelivered {externalId} ->
    entity {status = Delivered externalId}
  ExternalEffectFailed {} ->
    entity {status = Failed}
