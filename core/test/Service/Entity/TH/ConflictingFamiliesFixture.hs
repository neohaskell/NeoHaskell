module Service.Entity.TH.ConflictingFamiliesFixture where

import Core
import Language.Haskell.TH.Syntax qualified as TH
import Service.Entity.Core qualified as Entity
import Uuid qualified

data FamilyEntity = FamilyEntity
data DeclaredEvent = DeclaredEvent
data RequestedEvent = RequestedEvent

type instance EventOf FamilyEntity = DeclaredEvent
type instance EntityOf RequestedEvent = FamilyEntity

instance Entity FamilyEntity where
  initialStateImpl = FamilyEntity
  updateImpl _ entity = entity

instance Default FamilyEntity where
  def = FamilyEntity

instance Entity.Event RequestedEvent where
  getEventEntityIdImpl _ = Uuid.nil

$(TH.recover
    [d| rejectedEventOfConflict :: Bool; rejectedEventOfConflict = True |]
    do
      _ <- deriveEntity ''FamilyEntity ''RequestedEvent -- HOOK-ALLOW: inspect TH failure without splicing the generated declarations.
      [d| rejectedEventOfConflict :: Bool; rejectedEventOfConflict = False |])

data SecondEntity = SecondEntity
data SecondEvent = SecondEvent

type instance EventOf SecondEntity = SecondEvent
type instance EntityOf SecondEvent = FamilyEntity

instance Entity SecondEntity where
  initialStateImpl = SecondEntity
  updateImpl _ entity = entity

instance Default SecondEntity where
  def = SecondEntity

instance Entity.Event SecondEvent where
  getEventEntityIdImpl _ = Uuid.nil

$(TH.recover
    [d| rejectedEntityOfConflict :: Bool; rejectedEntityOfConflict = True |]
    do
      _ <- deriveEntity ''SecondEntity ''SecondEvent -- HOOK-ALLOW: inspect TH failure without splicing the generated declarations.
      [d| rejectedEntityOfConflict :: Bool; rejectedEntityOfConflict = False |])
