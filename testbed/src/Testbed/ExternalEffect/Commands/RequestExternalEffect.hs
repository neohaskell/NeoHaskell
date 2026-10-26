module Testbed.ExternalEffect.Commands.RequestExternalEffect (
  RequestExternalEffect (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.ExternalEffect.Core


data RequestExternalEffect = RequestExternalEffect
  { effectId :: Uuid,
    description :: Text,
    amount :: Int
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON RequestExternalEffect


instance Json.ToJSON RequestExternalEffect


getEntityId :: RequestExternalEffect -> Maybe Uuid
getEntityId cmd = Just cmd.effectId


decide :: RequestExternalEffect -> Maybe ExternalEffectEntity -> RequestContext -> Decision ExternalEffectEvent
decide cmd entity _ctx = case entity of
  Nothing ->
    Decider.acceptNew
      [ ExternalEffectRequested
          { entityId = cmd.effectId,
            description = cmd.description,
            amount = cmd.amount
          }
      ]
  Just _ ->
    Decider.acceptExisting
      [ ExternalEffectRequested
          { entityId = cmd.effectId,
            description = cmd.description,
            amount = cmd.amount
          }
      ]


type instance EntityOf RequestExternalEffect = ExternalEffectEntity


type instance TransportsOf RequestExternalEffect = '[WebTransport]


command ''RequestExternalEffect
