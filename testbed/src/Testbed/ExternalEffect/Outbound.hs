{-# LANGUAGE TemplateHaskell #-}

module Testbed.ExternalEffect.Outbound (
  ExternalEffectOutbound (..),
  handleEvent,
  deliverEffect,
) where

import Core
import Environment qualified
import Integration qualified
import Integration.Http qualified as Http
import Json qualified
import Result qualified as Result
import Service.OutboundIntegration.TH (outboundIntegration)
import Task qualified
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


handleEvent :: ExternalEffectEntity -> ExternalEffectRequestedPayload -> Integration.Outbound
handleEvent _ payload = deliverEffect "primary" True payload


deliverEffect :: Text -> Bool -> ExternalEffectRequestedPayload -> Integration.Outbound
deliverEffect handlerName emitOutcome payload =
  Integration.batch
    [ Integration.action \context -> do
        configured <- Environment.getVariable "TESTBED_FIXTURE_PORT" |> Task.asResult
        let fixturePort = case configured of
              Err _ -> "8181"
              Ok value -> value
        result <-
          Integration.runAction
            context
            ( Integration.outbound
                JsonSideEffect.JsonSideEffect
                  { method = Http.POST,
                    url = [fmt|http://127.0.0.1:#{fixturePort}/external/effects|],
                    headers = [("Idempotency-Key", Uuid.toText payload.requestedEntityId), ("X-Event-Model-Handler", handlerName)],
                    payload =
                      ExternalEffectPayload
                        { effectId = Uuid.toText payload.requestedEntityId,
                          description = payload.requestedDescription,
                          amount = payload.requestedAmount
                        },
                    auth = Http.NoAuth,
                    retry = Http.noRetry,
                    timeoutSeconds = 5,
                    onSuccess = mapSuccess payload.requestedEntityId,
                    onError = \_ -> RecordEffectFailure {effectId = payload.requestedEntityId}
                  }
            )
        if emitOutcome then Task.yield result else Task.yield Nothing
    ]


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
