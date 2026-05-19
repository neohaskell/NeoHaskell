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
import Service.Query.Auth (AccessError)
import Service.Query.Auth qualified as QueryAuth
import Service.Command.Core (UserClaims)
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
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = QueryAuth.requirePermission "admin:delete" claims


getEntityId :: CommandWithAdminDelete -> Maybe Uuid
getEntityId _command = Nothing


decide :: CommandWithAdminDelete -> Maybe CanAccessPermEntity -> RequestContext -> Decision CanAccessPermEvent
decide _command _entity _context = Decider.acceptNew [CanAccessPermEventCreated Uuid.nil]


command ''CommandWithAdminDelete
