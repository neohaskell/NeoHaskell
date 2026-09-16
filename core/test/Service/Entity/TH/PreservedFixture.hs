module Service.Entity.TH.PreservedFixture where

import Core
import Json qualified
import Service.Entity.Core qualified as Entity

-- Custom behavior deliberately has no marker companion functions.
data CustomEntity = CustomEntity {count :: Int}
  deriving (Generic)

data CustomEvent = CustomChanged {customId :: Text, delta :: Int}

deriveEvent ''CustomEvent

type instance NameOf CustomEntity = "custom-counter"
type instance EventOf CustomEntity = CustomEvent
type instance EntityOf CustomEvent = CustomEntity

instance Json.ToJSON CustomEntity where
  toJSON entity = Json.encode entity.count

instance Json.FromJSON CustomEntity where
  parseJSON value = do
    count <- Json.parseJSON value
    Json.yield CustomEntity {count}

instance Default CustomEntity where
  def = CustomEntity {count = 77}

instance Entity CustomEntity where
  type EntityIdType CustomEntity = Text
  initialStateImpl = CustomEntity {count = 11}
  updateImpl change entity = entity {count = entity.count + change.delta + 100}

instance Entity.Event CustomEvent where
  getEventEntityIdImpl change = change.customId

deriveEntity ''CustomEntity ''CustomEvent

-- Default can reuse an existing Entity implementation without local companions.
data ExistingEntity = ExistingEntity {count :: Int}
data ExistingEvent = ExistingChanged {customId :: Text, delta :: Int}

deriveEvent ''ExistingEvent

type ExistingEventAlias = ExistingEvent
type ExistingEntityAlias = ExistingEntity

type instance EventOf ExistingEntity = ExistingEventAlias
type instance EntityOf ExistingEvent = ExistingEntityAlias

instance Entity ExistingEntity where
  type EntityIdType ExistingEntity = Text
  initialStateImpl = ExistingEntity {count = 33}
  updateImpl change entity = entity {count = entity.count + change.delta}

instance Entity.Event ExistingEvent where
  getEventEntityIdImpl change = change.customId

deriveEntity ''ExistingEntity ''ExistingEvent
