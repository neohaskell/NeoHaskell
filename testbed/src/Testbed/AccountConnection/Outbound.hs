{-# LANGUAGE TemplateHaskell #-}

module Testbed.AccountConnection.Outbound (
  AccountTokenPresenceObserver (..),
  handleEvent,
  observeTokenStorage,
) where

import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Basics
import Core
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)
import Task qualified
import Testbed.AccountConnection.Commands.RecordAccountConnection (
  RecordAccountConnection (..),
  RecordAccountConnectionAction (..),
 )
import Testbed.AccountConnection.Core


data AccountTokenPresenceObserver = AccountTokenPresenceObserver
  deriving (Generic, Typeable, Show)


type instance EntityOf AccountTokenPresenceObserver = AccountConnectionEntity


handleEvent :: AccountConnectionEntity -> AccountConnectedPayload -> Integration.Outbound
handleEvent _ payload = observeTokenStorage payload.connectedUserId


observeTokenStorage :: Text -> Integration.Outbound
observeTokenStorage userId =
  Integration.batch
    [ Integration.action \ctx -> do
        let tokenKey = TokenKey [fmt|oauth:test-provider:#{userId}|]
        storedTokens <-
          ctx.secretStore.get tokenKey
            |> Task.mapError Integration.UnexpectedError
        let tokenStored = case storedTokens of
              Nothing -> False
              Just _ -> True
        Integration.emitCommand
          RecordAccountConnection
            { action = RecordTokenPresence {userId = userId, tokenStored = tokenStored}
            }
    ]


outboundIntegration ''AccountTokenPresenceObserver
