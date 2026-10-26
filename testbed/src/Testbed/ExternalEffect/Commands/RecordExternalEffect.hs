module Testbed.ExternalEffect.Commands.RecordExternalEffect (
  RecordExternalEffect (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Internal (InternalTransport)
import Testbed.ExternalEffect.Core


data RecordExternalEffect
  = RecordEffectSuccess
      { effectId :: Uuid,
        externalId :: Text
      }
  | RecordEffectFailure
      { effectId :: Uuid
      }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON RecordExternalEffect


instance Json.ToJSON RecordExternalEffect


getEntityId :: RecordExternalEffect -> Maybe Uuid
getEntityId cmd = Just cmd.effectId


decide :: RecordExternalEffect -> Maybe ExternalEffectEntity -> RequestContext -> Decision ExternalEffectEvent
decide cmd entity _ctx = case (cmd, entity) of
  (RecordEffectSuccess {effectId, externalId}, Just _) ->
    Decider.acceptExisting
      [ExternalEffectDelivered {entityId = effectId, externalId = externalId}]
  (RecordEffectFailure {effectId}, Just _) ->
    Decider.acceptExisting [ExternalEffectFailed {entityId = effectId}]
  (_, Nothing) ->
    Decider.reject "External effect request was not found"


type instance EntityOf RecordExternalEffect = ExternalEffectEntity


type instance TransportsOf RecordExternalEffect = '[InternalTransport]


command ''RecordExternalEffect
