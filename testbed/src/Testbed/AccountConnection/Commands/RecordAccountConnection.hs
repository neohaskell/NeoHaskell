module Testbed.AccountConnection.Commands.RecordAccountConnection (
  RecordAccountConnection (..),
  RecordAccountConnectionAction (..),
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
import Testbed.AccountConnection.Core


data RecordAccountConnection = RecordAccountConnection
  { action :: RecordAccountConnectionAction
  }
  deriving (Generic, Typeable, Show)


data RecordAccountConnectionAction
  = RecordConnected
      { userId :: Text,
        tokenKeyMatches :: Bool
      }
  | RecordFailed
      { userId :: Text
      }
  | RecordDisconnected
      { userId :: Text
      }
  | RecordTokenPresence
      { userId :: Text,
        tokenStored :: Bool
      }
  deriving (Generic, Show)


instance Json.FromJSON RecordAccountConnectionAction


instance Json.ToJSON RecordAccountConnectionAction


instance ToSchema RecordAccountConnectionAction


instance Json.FromJSON RecordAccountConnection


instance Json.ToJSON RecordAccountConnection


getEntityId :: RecordAccountConnection -> Maybe Uuid
getEntityId cmd = case cmd.action of
  RecordConnected {userId} -> Just (accountConnectionIdFor userId)
  RecordFailed {userId} -> Just (accountConnectionIdFor userId)
  RecordDisconnected {userId} -> Just (accountConnectionIdFor userId)
  RecordTokenPresence {userId} -> Just (accountConnectionIdFor userId)


decide :: RecordAccountConnection -> Maybe AccountConnectionEntity -> RequestContext -> Decision AccountConnectionEvent
decide cmd entity _ctx = case (cmd.action, entity) of
  (RecordTokenPresence {}, Nothing) ->
    Decider.reject "Account connection was not found"
  (action, maybeEntity) ->
    let events = case action of
          RecordConnected {userId, tokenKeyMatches} ->
            [ AccountConnectionSucceeded
                { entityId = accountConnectionIdFor userId,
                  userId = userId,
                  tokenKeyMatches = tokenKeyMatches
                }
            ]
          RecordFailed {userId} ->
            [ AccountConnectionFailedEvent
                { entityId = accountConnectionIdFor userId,
                  userId = userId
                }
            ]
          RecordDisconnected {userId} ->
            [ AccountConnectionRemoved
                { entityId = accountConnectionIdFor userId,
                  userId = userId
                }
            ]
          RecordTokenPresence {userId, tokenStored} ->
            [ AccountTokenPresenceObserved
                { entityId = accountConnectionIdFor userId,
                  tokenStored = tokenStored
                }
            ]
    in  case maybeEntity of
          Nothing -> Decider.acceptNew events
          Just _ -> Decider.acceptExisting events


type instance EntityOf RecordAccountConnection = AccountConnectionEntity


type instance TransportsOf RecordAccountConnection = '[InternalTransport]


command ''RecordAccountConnection
