{-# LANGUAGE TemplateHaskell #-}

-- | Fixture: CommandWithPublicAccess
-- Defines canAccess = publicAccess so TH wires canAccessImpl to the stub.
module Service.Command.CanAccess.PublicAccessFixture (
  CommandWithPublicAccess (..),
  canAccess,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.Query.Auth (AccessError, publicAccess)
import Service.Command.Core (UserClaims)
import Service.CommandExecutor.TH (command)
import Uuid qualified


data CanAccessPubEntity = CanAccessPubEntity
  { entityId :: Uuid
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON CanAccessPubEntity
instance Json.FromJSON CanAccessPubEntity


data CanAccessPubEvent = CanAccessPubEventCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON CanAccessPubEvent
instance Json.FromJSON CanAccessPubEvent


type instance EventOf CanAccessPubEntity = CanAccessPubEvent


instance Entity CanAccessPubEntity where
  initialStateImpl = CanAccessPubEntity {entityId = Uuid.nil}
  updateImpl ev entity = case ev of
    CanAccessPubEventCreated uid -> entity {entityId = uid}


data CommandWithPublicAccess = CommandWithPublicAccess
  { cmdPublicId :: Uuid
  }
  deriving (Generic)


type instance EntityOf CommandWithPublicAccess = CanAccessPubEntity


-- | Public access override — any caller may invoke this command.
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = publicAccess


getEntityId :: CommandWithPublicAccess -> Maybe Uuid
getEntityId _ = Nothing


decide :: CommandWithPublicAccess -> Maybe CanAccessPubEntity -> RequestContext -> Decision CanAccessPubEvent
decide _ _ _ = Decider.acceptNew [CanAccessPubEventCreated Uuid.nil]


command ''CommandWithPublicAccess
