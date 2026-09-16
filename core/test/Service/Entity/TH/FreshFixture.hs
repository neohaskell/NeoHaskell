module Service.Entity.TH.FreshFixture where

import Core
import Uuid qualified

-- This local type deliberately shadows the event-routing class's short name.
data Event = Changed {eventId :: Uuid, delta :: Int}

deriveEvent ''Event

data Counter = Counter {entityId :: Uuid, count :: Int}

initialState :: Counter
initialState = Counter {entityId = Uuid.nil, count = 10}

update :: Event -> Counter -> Counter
update change counter =
  counter {entityId = change.eventId, count = counter.count + change.delta}

getEventEntityId :: Event -> Uuid
getEventEntityId change = change.eventId

deriveEntity ''Counter ''Event

-- Repeated application must leave the existing generated instances intact.
deriveEntity ''Counter ''Event

injectedEvent :: Event
injectedEvent = event (Changed {eventId = Uuid.nil, delta = 3})
