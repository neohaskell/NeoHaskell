{-# LANGUAGE TemplateHaskell #-}

-- | Fixture: CommandWithAdminDelete
-- Defines canAccess = requirePermission "admin:delete".
module Service.Command.CanAccess.PermissionFixture (
  CommandWithAdminDelete (..),
  CanAccessPermEntity (..),
  CanAccessPermEvent (..),
  canAccess,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.Command.Auth (CommandAuthError, requirePermission)
import Service.Command.Core (Command (..), UserClaims)
import Service.CommandExecutor.TH (command)
import Uuid qualified


data CanAccessPermEntity = CanAccessPermEntity
  { entityId :: Uuid
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON CanAccessPermEntity
instance Json.FromJSON CanAccessPermEntity


data CanAccessPermEvent = CanAccessPermEventCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON CanAccessPermEvent
instance Json.FromJSON CanAccessPermEvent


type instance EventOf CanAccessPermEntity = CanAccessPermEvent


instance Entity CanAccessPermEntity where
  initialStateImpl = CanAccessPermEntity {entityId = Uuid.nil}
  updateImpl ev entity = case ev of
    CanAccessPermEventCreated uid -> entity {entityId = uid}


data CommandWithAdminDelete = CommandWithAdminDelete
  { cmdAdminId :: Uuid
  }
  deriving (Generic)


type instance EntityOf CommandWithAdminDelete = CanAccessPermEntity


-- | Require "admin:delete" permission to invoke this command.
canAccess :: Maybe UserClaims -> Maybe CommandAuthError
canAccess = requirePermission "admin:delete"


getEntityId :: CommandWithAdminDelete -> Maybe Uuid
getEntityId _ = Nothing


decide :: CommandWithAdminDelete -> Maybe CanAccessPermEntity -> RequestContext -> Decision CanAccessPermEvent
decide _ _ _ = Decider.acceptNew [CanAccessPermEventCreated Uuid.nil]


command ''CommandWithAdminDelete
