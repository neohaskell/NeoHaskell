module Starter.Counter.Commands.IncrementCounter (
  IncrementCounter (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Derive
import Service.Auth (RequestContext)
import Starter.Counter.Entity (CounterEntity (..), CounterEvent)
import Starter.Counter.Events.CounterIncremented (CounterIncremented (..))

data IncrementCounter = IncrementCounter
  { entityId :: Uuid
  , amount :: Int
  }

getEntityId :: IncrementCounter -> Maybe Uuid
getEntityId cmd = Just cmd.entityId


decide :: IncrementCounter -> Maybe CounterEntity -> RequestContext -> Decision CounterEvent
decide cmd entity _ctx = case entity of
  Nothing ->
    Decider.reject "Counter not found"
  Just existing ->
    if cmd.amount <= 0
      then Decider.reject "Amount must be positive"
      else
        Decider.acceptExisting
          [ CounterIncremented
              { entityId = existing.counterId
              , amount = cmd.amount
              }
          ]


derive ''IncrementCounter command { entity = ''CounterEntity }
