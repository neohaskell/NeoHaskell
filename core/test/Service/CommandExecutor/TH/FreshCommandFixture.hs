{-# LANGUAGE TemplateHaskell #-}

-- | Test fixture: FreshCommand has NO pre-existing Show / Generic / FromJSON / ToJSON.
-- The @command@ marker is expected (after phase 10) to emit all four.
--
-- TH probes evaluate to False before phase 10 (stub emits nothing) and to
-- True after phase 10 (real implementation emits the instances).  The test
-- file asserts True, so these tests are red until phase 10 lands.
module Service.CommandExecutor.TH.FreshCommandFixture (
  FreshCommand (..),
  hasFreshCommandShow,
  hasFreshCommandGeneric,
  hasFreshCommandToJSON,
  hasFreshCommandFromJSON,
) where

import Core
import Decider qualified
import Json qualified
import Language.Haskell.TH.Syntax qualified as TH
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (command)
import Uuid qualified


-- Supporting entity --------------------------------------------------------

data THTestEntity = THTestEntity {thEntityId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON THTestEntity
instance Json.FromJSON THTestEntity


data THTestEvent = THTestEntityCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON THTestEvent
instance Json.FromJSON THTestEvent


type instance EventOf THTestEntity = THTestEvent


instance Entity THTestEntity where
  initialStateImpl = THTestEntity {thEntityId = Uuid.nil}
  updateImpl ev entity = case ev of
    THTestEntityCreated uid -> entity {thEntityId = uid}


-- Fresh command type: HAS Generic (required by existing ToSchema emission),
-- but NO Show / FromJSON / ToJSON.  Phase 10 will emit those three.
-- The Generic probe will report True (pre-existing) — see note in THSpec.

data FreshCommand = FreshCommand {freshId :: Uuid, freshLabel :: Text}
  deriving (Generic)


type instance EntityOf FreshCommand = THTestEntity


getEntityId :: FreshCommand -> Maybe Uuid
getEntityId _ = Nothing


decide :: FreshCommand -> Maybe THTestEntity -> RequestContext -> Decision THTestEvent
decide _ _ _ = Decider.acceptNew [THTestEntityCreated Uuid.nil]


command ''FreshCommand


-- TH probes -----------------------------------------------------------------

$(do
    is <- TH.reifyInstances ''Show [TH.ConT ''FreshCommand]
    case is of
      [] -> [d| hasFreshCommandShow :: Bool; hasFreshCommandShow = False |]
      _ : _ -> [d| hasFreshCommandShow :: Bool; hasFreshCommandShow = True |])


$(do
    is <- TH.reifyInstances ''Generic [TH.ConT ''FreshCommand]
    case is of
      [] -> [d| hasFreshCommandGeneric :: Bool; hasFreshCommandGeneric = False |]
      _ : _ -> [d| hasFreshCommandGeneric :: Bool; hasFreshCommandGeneric = True |])


$(do
    is <- TH.reifyInstances ''Json.ToJSON [TH.ConT ''FreshCommand]
    case is of
      [] -> [d| hasFreshCommandToJSON :: Bool; hasFreshCommandToJSON = False |]
      _ : _ -> [d| hasFreshCommandToJSON :: Bool; hasFreshCommandToJSON = True |])


$(do
    is <- TH.reifyInstances ''Json.FromJSON [TH.ConT ''FreshCommand]
    case is of
      [] -> [d| hasFreshCommandFromJSON :: Bool; hasFreshCommandFromJSON = False |]
      _ : _ -> [d| hasFreshCommandFromJSON :: Bool; hasFreshCommandFromJSON = True |])
