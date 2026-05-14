{-# LANGUAGE TemplateHaskell #-}

-- | Test fixture: AlreadyDerivedCommand already has Show / Generic / FromJSON /
-- ToJSON in scope BEFORE the @command@ call.
--
-- If the module compiles without a GHC "Duplicate instance" error, the marker
-- correctly detected the pre-existing instances and emitted nothing.  This is
-- a regression test (green against phase-9 stub and against the final impl).
module Service.CommandExecutor.TH.AlreadyDerivedCommandFixture (
  AlreadyDerivedCommand (..),
  hasAlreadyDerivedCommandShow,
) where

import Core
import Decider qualified
import Json qualified
import Language.Haskell.TH.Syntax qualified as TH
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (command)
import Uuid qualified


-- Supporting entity (imported from FreshCommandFixture to avoid duplication
-- but declared here to keep the fixture self-contained) ---------------------

data ADTestEntity = ADTestEntity {adEntityId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON ADTestEntity
instance Json.FromJSON ADTestEntity


data ADTestEvent = ADTestEntityCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON ADTestEvent
instance Json.FromJSON ADTestEvent


type instance EventOf ADTestEntity = ADTestEvent


instance Entity ADTestEntity where
  initialStateImpl = ADTestEntity {adEntityId = Uuid.nil}
  updateImpl ev entity = case ev of
    ADTestEntityCreated uid -> entity {adEntityId = uid}


-- Command type WITH all four instances already in scope --------------------

data AlreadyDerivedCommand = AlreadyDerivedCommand {cmdId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON AlreadyDerivedCommand
instance Json.FromJSON AlreadyDerivedCommand


type instance EntityOf AlreadyDerivedCommand = ADTestEntity


getEntityId :: AlreadyDerivedCommand -> Maybe Uuid
getEntityId _ = Nothing


decide :: AlreadyDerivedCommand -> Maybe ADTestEntity -> RequestContext -> Decision ADTestEvent
decide _ _ _ = Decider.acceptNew [ADTestEntityCreated Uuid.nil]


-- [regression] Successful compilation of this splice proves idempotency.
command ''AlreadyDerivedCommand


-- TH probe: Show is still present after the marker call --------------------

$(do
    is <- TH.reifyInstances ''Show [TH.ConT ''AlreadyDerivedCommand]
    case is of
      [] -> [d| hasAlreadyDerivedCommandShow :: Bool; hasAlreadyDerivedCommandShow = False |]
      _ : _ -> [d| hasAlreadyDerivedCommandShow :: Bool; hasAlreadyDerivedCommandShow = True |])
