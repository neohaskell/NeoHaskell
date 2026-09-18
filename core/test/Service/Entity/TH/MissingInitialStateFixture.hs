module Service.Entity.TH.MissingInitialStateFixture where

import Core
import Language.Haskell.TH.Syntax qualified as TH
import Uuid qualified

data MissingEntity = MissingEntity
data MissingEvent = MissingEvent

deriveEvent ''MissingEvent

update :: MissingEvent -> MissingEntity -> MissingEntity
update _ entity = entity

getEventEntityId :: MissingEvent -> Uuid
getEventEntityId _ = Uuid.nil

$(TH.recover
    [d| rejectedMissingCompanion :: Bool; rejectedMissingCompanion = True |]
    do
      _ <- deriveEntity ''MissingEntity ''MissingEvent -- HOOK-ALLOW: inspect TH failure without splicing the generated declarations.
      [d| rejectedMissingCompanion :: Bool; rejectedMissingCompanion = False |])
