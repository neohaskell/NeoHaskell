module Service.Entity.TH.MissingRoutingFixture where

import Core
import Language.Haskell.TH.Syntax qualified as TH

data MissingEntity = MissingEntity
data MissingEvent = MissingEvent

deriveEvent ''MissingEvent

initialState :: MissingEntity
initialState = MissingEntity

update :: MissingEvent -> MissingEntity -> MissingEntity
update _ entity = entity

$(TH.recover
    [d| rejectedMissingCompanion :: Bool; rejectedMissingCompanion = True |]
    do
      _ <- deriveEntity ''MissingEntity ''MissingEvent -- HOOK-ALLOW: inspect TH failure without splicing the generated declarations.
      [d| rejectedMissingCompanion :: Bool; rejectedMissingCompanion = False |])
