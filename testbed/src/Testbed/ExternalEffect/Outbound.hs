{-# LANGUAGE TemplateHaskell #-}

module Testbed.ExternalEffect.Outbound (
  ExternalEffectOutbound (..),
  handleEvent,
) where

import Core
import Integration qualified
import Integration.Http qualified as Http
import Json qualified
import Result qualified as Result
import Service.OutboundIntegration.TH (outboundIntegration)
import Testbed.ExternalEffect.Commands.RecordExternalEffect (RecordExternalEffect (..))
import Testbed.ExternalEffect.Core
import Testbed.Integrations.JsonSideEffect qualified as JsonSideEffect
import Text qualified
import Uuid qualified


data ExternalEffectOutbound = ExternalEffectOutbound
  deriving (Generic, Typeable, Show)


type instance EntityOf ExternalEffectOutbound = ExternalEffectEntity


data ExternalEffectPayload = ExternalEffectPayload
  { effectId :: Text,
    description :: Text,
    amount :: Int
  }
  deriving (Generic, Show)


instance Json.ToJSON ExternalEffectPayload


data ExternalEffectResponse = ExternalEffectResponse
  { externalId :: Text
  }
  deriving (Generic, Show)


instance Json.FromJSON ExternalEffectResponse


handleEvent :: ExternalEffectEntity -> ExternalEffectEvent -> Integration.Outbound
handleEvent _ event = case event of
  ExternalEffectRequested {entityId, description, amount} ->
    Integration.batch
      [ Integration.outbound
          JsonSideEffect.JsonSideEffect
            { method = Http.POST,
              url = "http://127.0.0.1:8181/external/effects",
              headers = [("Idempotency-Key", Uuid.toText entityId)],
              payload =
                ExternalEffectPayload
                  { effectId = Uuid.toText entityId,
                    description = description,
                    amount = amount
                  },
              auth = Http.NoAuth,
              retry = Http.noRetry,
              timeoutSeconds = 5,
              onSuccess = mapSuccess entityId,
              onError = \_ -> RecordEffectFailure {effectId = entityId}
            }
      ]
  _ -> Integration.none


mapSuccess :: Uuid -> Http.Response -> RecordExternalEffect
mapSuccess effectId response =
  if response.statusCode < 200 || response.statusCode >= 300
    then RecordEffectFailure {effectId = effectId}
    else case Json.decode @ExternalEffectResponse response.body of
      Result.Ok decoded ->
        if Text.isEmpty decoded.externalId
          then RecordEffectFailure {effectId = effectId}
          else RecordEffectSuccess {effectId = effectId, externalId = decoded.externalId}
      Result.Err _ ->
        RecordEffectFailure {effectId = effectId}


outboundIntegration ''ExternalEffectOutbound
