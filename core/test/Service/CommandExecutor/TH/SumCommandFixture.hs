{-# LANGUAGE TemplateHaskell #-}

-- | Test fixture: SumCommand is a sum type with multiple constructors.
-- Proves that the emitted Show instance handles all constructors correctly.
module Service.CommandExecutor.TH.SumCommandFixture (
  SumCommand (..),
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (command)
import Uuid qualified


-- Supporting entity --------------------------------------------------------

data SumTestEntity = SumTestEntity {sumEntityId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON SumTestEntity
instance Json.FromJSON SumTestEntity


data SumTestEvent = SumTestEntityCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON SumTestEvent
instance Json.FromJSON SumTestEvent


type instance EventOf SumTestEntity = SumTestEvent


instance Entity SumTestEntity where
  initialStateImpl = SumTestEntity {sumEntityId = Uuid.nil}
  updateImpl ev entity = case ev of
    SumTestEntityCreated uid -> entity {sumEntityId = uid}


-- Sum-type command ----------------------------------------------------------

data SumCommand
  = SumCreate {sumCreateId :: Uuid}
  | SumUpdate {sumUpdateId :: Uuid, sumUpdateLabel :: Text}
  deriving (Eq, Show, Generic)


instance Json.ToJSON SumCommand
instance Json.FromJSON SumCommand


type instance EntityOf SumCommand = SumTestEntity


getEntityId :: SumCommand -> Maybe Uuid
getEntityId sc = case sc of
  SumCreate uid -> Just uid
  SumUpdate uid _ -> Just uid


decide :: SumCommand -> Maybe SumTestEntity -> RequestContext -> Decision SumTestEvent
decide _ _ _ = Decider.acceptNew [SumTestEntityCreated Uuid.nil]


command ''SumCommand
