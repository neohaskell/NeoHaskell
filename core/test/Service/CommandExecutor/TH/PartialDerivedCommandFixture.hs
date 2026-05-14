{-# LANGUAGE TemplateHaskell #-}

-- | Test fixture: PartialDerivedCommand has Show + Generic already in scope
-- but NO FromJSON or ToJSON before the @command@ call.
--
-- After phase 10 the marker should emit only the missing JSON instances,
-- leaving Show and Generic untouched (and producing no duplicate).
module Service.CommandExecutor.TH.PartialDerivedCommandFixture (
  PartialDerivedCommand (..),
  hasPartialToJSON,
  hasPartialFromJSON,
  hasPartialShow,
) where

import Core
import Decider qualified
import Json qualified
import Language.Haskell.TH.Syntax qualified as TH
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (command)
import Uuid qualified


-- Supporting entity --------------------------------------------------------

data PDTestEntity = PDTestEntity {pdEntityId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON PDTestEntity
instance Json.FromJSON PDTestEntity


data PDTestEvent = PDTestEntityCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON PDTestEvent
instance Json.FromJSON PDTestEvent


type instance EventOf PDTestEntity = PDTestEvent


instance Entity PDTestEntity where
  initialStateImpl = PDTestEntity {pdEntityId = Uuid.nil}
  updateImpl ev entity = case ev of
    PDTestEntityCreated uid -> entity {pdEntityId = uid}


-- Command type: Show + Generic in scope, no JSON ----------------------------

data PartialDerivedCommand = PartialDerivedCommand {partialId :: Uuid}
  deriving (Show, Generic)


type instance EntityOf PartialDerivedCommand = PDTestEntity


getEntityId :: PartialDerivedCommand -> Maybe Uuid
getEntityId _ = Nothing


decide :: PartialDerivedCommand -> Maybe PDTestEntity -> RequestContext -> Decision PDTestEvent
decide _ _ _ = Decider.acceptNew [PDTestEntityCreated Uuid.nil]


command ''PartialDerivedCommand


-- TH probes -----------------------------------------------------------------

$(do
    is <- TH.reifyInstances ''Json.ToJSON [TH.ConT ''PartialDerivedCommand]
    case is of
      [] -> [d| hasPartialToJSON :: Bool; hasPartialToJSON = False |]
      _ : _ -> [d| hasPartialToJSON :: Bool; hasPartialToJSON = True |])


$(do
    is <- TH.reifyInstances ''Json.FromJSON [TH.ConT ''PartialDerivedCommand]
    case is of
      [] -> [d| hasPartialFromJSON :: Bool; hasPartialFromJSON = False |]
      _ : _ -> [d| hasPartialFromJSON :: Bool; hasPartialFromJSON = True |])


-- Regression: Show was present before the marker; must still be present.
$(do
    is <- TH.reifyInstances ''Show [TH.ConT ''PartialDerivedCommand]
    case is of
      [] -> [d| hasPartialShow :: Bool; hasPartialShow = False |]
      _ : _ -> [d| hasPartialShow :: Bool; hasPartialShow = True |])
