{-# LANGUAGE TemplateHaskell #-}

-- | Fixture: CommandWithoutCanAccess
-- No canAccess defined — typeclass default (error stub) applies.
module Service.Command.CanAccess.NoCanAccessFixture (
  CommandWithoutCanAccess (..),
  CanAccessNoEntity (..),
  CanAccessNoEvent (..),
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (command)
import Uuid qualified


data CanAccessNoEntity = CanAccessNoEntity
  { entityId :: Uuid
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON CanAccessNoEntity
instance Json.FromJSON CanAccessNoEntity


data CanAccessNoEvent = CanAccessNoEventCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON CanAccessNoEvent
instance Json.FromJSON CanAccessNoEvent


type instance EventOf CanAccessNoEntity = CanAccessNoEvent


instance Entity CanAccessNoEntity where
  initialStateImpl = CanAccessNoEntity {entityId = Uuid.nil}
  updateImpl ev entity = case ev of
    CanAccessNoEventCreated uid -> entity {entityId = uid}


data CommandWithoutCanAccess = CommandWithoutCanAccess
  { cmdNoAccessId :: Uuid
  }
  deriving (Generic)


type instance EntityOf CommandWithoutCanAccess = CanAccessNoEntity


getEntityId :: CommandWithoutCanAccess -> Maybe Uuid
getEntityId _ = Nothing


decide :: CommandWithoutCanAccess -> Maybe CanAccessNoEntity -> RequestContext -> Decision CanAccessNoEvent
decide _ _ _ = Decider.acceptNew [CanAccessNoEventCreated Uuid.nil]


command ''CommandWithoutCanAccess
