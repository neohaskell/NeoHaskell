module Starter.Counter.Entity (
  CounterEntity (..),
  initialState,
  update,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified
import Starter.Counter.Event (CounterEvent (..), getEventEntityId)
import Starter.Counter.Events.CounterCreated qualified as CounterCreated
import Starter.Counter.Events.CounterDecremented qualified as CounterDecremented
import Starter.Counter.Events.CounterIncremented qualified as CounterIncremented


-- | Current state of a single counter aggregate, rebuilt from its events.
data CounterEntity = CounterEntity
  { counterId :: Uuid
  , label :: Text
  , value :: Int
  }
  deriving (Generic)


instance Json.FromJSON CounterEntity


instance Json.ToJSON CounterEntity


instance Default CounterEntity where
  def = initialState


initialState :: CounterEntity
initialState =
  CounterEntity
    { counterId = Uuid.nil
    , label = ""
    , value = 0
    }


type instance NameOf CounterEntity = "CounterEntity"


type instance EventOf CounterEntity = CounterEvent


type instance EntityOf CounterEvent = CounterEntity


instance Entity CounterEntity where
  initialStateImpl = initialState
  updateImpl = update


instance Event CounterEvent where
  getEventEntityIdImpl = getEventEntityId


-- | Apply one event to the current state.
update :: CounterEvent -> CounterEntity -> CounterEntity
update event entity = case event of
  CounterCreated e ->
    CounterEntity
      { counterId = e.entityId
      , label = e.label
      , value = 0
      }
  CounterIncremented e ->
    entity {value = entity.value + e.amount}
  CounterDecremented e ->
    entity {value = entity.value - e.amount}
