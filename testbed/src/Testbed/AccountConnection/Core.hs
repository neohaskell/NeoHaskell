module Testbed.AccountConnection.Core (
  AccountConnectionEntity (..),
  AccountConnectionEvent (..),
  AccountConnectionState (..),
  accountConnectionIdFor,
  initialState,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified


data AccountConnectionEntity = AccountConnectionEntity
  { accountConnectionId :: Uuid,
    userId :: Text,
    state :: AccountConnectionState
  }
  deriving (Generic)


instance Json.FromJSON AccountConnectionEntity


instance Json.ToJSON AccountConnectionEntity


instance Default AccountConnectionEntity where
  def = initialState


type instance NameOf AccountConnectionEntity = "AccountConnectionEntity"


instance Entity AccountConnectionEntity where
  initialStateImpl = initialState
  updateImpl = update


data AccountConnectionState
  = NeverConnected
  | Connected
      { tokenKeyMatches :: Bool,
        tokenStored :: Maybe Bool
      }
  | Disconnected
      { tokenStored :: Maybe Bool
      }
  | ConnectionFailed
  deriving (Eq, Generic, Show)


instance Json.FromJSON AccountConnectionState


instance Json.ToJSON AccountConnectionState


data AccountConnectionEvent
  = AccountConnectionSucceeded
      { entityId :: Uuid,
        userId :: Text,
        tokenKeyMatches :: Bool
      }
  | AccountConnectionFailedEvent
      { entityId :: Uuid,
        userId :: Text
      }
  | AccountConnectionRemoved
      { entityId :: Uuid,
        userId :: Text
      }
  | AccountTokenPresenceObserved
      { entityId :: Uuid,
        tokenStored :: Bool
      }
  deriving (Eq, Generic, Ord, Show)


instance Json.FromJSON AccountConnectionEvent


instance Json.ToJSON AccountConnectionEvent


type instance EventOf AccountConnectionEntity = AccountConnectionEvent


type instance EntityOf AccountConnectionEvent = AccountConnectionEntity


instance Event AccountConnectionEvent where
  getEventEntityIdImpl = (.entityId)


accountConnectionIdFor :: Text -> Uuid
accountConnectionIdFor = Uuid.generateV5 Uuid.nil


initialState :: AccountConnectionEntity
initialState =
  AccountConnectionEntity
    { accountConnectionId = Uuid.nil,
      userId = "",
      state = NeverConnected
    }


update :: AccountConnectionEvent -> AccountConnectionEntity -> AccountConnectionEntity
update event entity = case event of
  AccountConnectionSucceeded {entityId, userId, tokenKeyMatches} ->
    AccountConnectionEntity
      { accountConnectionId = entityId,
        userId = userId,
        state = Connected {tokenKeyMatches = tokenKeyMatches, tokenStored = Nothing}
      }
  AccountConnectionFailedEvent {entityId, userId} ->
    AccountConnectionEntity
      { accountConnectionId = entityId,
        userId = userId,
        state = ConnectionFailed
      }
  AccountConnectionRemoved {entityId, userId} ->
    AccountConnectionEntity
      { accountConnectionId = entityId,
        userId = userId,
        state = Disconnected {tokenStored = Nothing}
      }
  AccountTokenPresenceObserved {tokenStored} ->
    entity
      { state = case entity.state of
          Connected {tokenKeyMatches} ->
            Connected {tokenKeyMatches = tokenKeyMatches, tokenStored = Just tokenStored}
          Disconnected {} ->
            Disconnected {tokenStored = Just tokenStored}
          currentState -> currentState
      }
