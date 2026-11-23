module Starter.Counter.Entity (
  CounterEntity (..),
  CounterEvent (..),
  initialState,
  update,
) where

import Core
import Derive
import Starter.Counter.Events.CounterCreated (CounterCreated (..))
import Starter.Counter.Events.CounterDecremented (CounterDecremented (..))
import Starter.Counter.Events.CounterIncremented (CounterIncremented (..))
import Uuid qualified

data CounterEntity = CounterEntity
  { counterId :: Uuid,
    label :: Text,
    value :: Int
  }


derive ''CounterEntity entity {
  events = [''CounterCreated, ''CounterIncremented, ''CounterDecremented]
}


initialState :: CounterEntity
initialState =
  CounterEntity
    { counterId = Uuid.nil,
      label = "",
      value = 0
    }


update :: CounterEvent -> CounterEntity -> CounterEntity
update event entity = case event of
  CounterEventCounterCreated e ->
    CounterEntity
      { counterId = e.entityId,
        label = e.label,
        value = 0
      }
  CounterEventCounterIncremented e ->
    entity {value = entity.value + e.amount}
  CounterEventCounterDecremented e ->
    entity {value = entity.value - e.amount}
