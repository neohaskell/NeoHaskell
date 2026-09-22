module Service.Entity.TH.MissingUpdateFixture where

import Core
import Language.Haskell.TH.Syntax qualified as TH
import Uuid qualified

data MissingEntity = MissingEntity
data MissingEvent = MissingEvent

deriveEvent ''MissingEvent

initialState :: MissingEntity
initialState = MissingEntity

getEventEntityId :: MissingEvent -> Uuid
getEventEntityId _ = Uuid.nil

$(TH.recover
    [d| rejectedMissingCompanion :: Bool; rejectedMissingCompanion = True |]
    do
      _ <- deriveEntity ''MissingEntity ''MissingEvent -- HOOK-ALLOW: inspect TH failure without splicing the generated declarations.
      [d| rejectedMissingCompanion :: Bool; rejectedMissingCompanion = False |])
