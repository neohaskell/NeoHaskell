module Starter.Counter.Commands.DecrementCounter (
  DecrementCounter (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Derive
import Service.Auth (RequestContext)
import Starter.Counter.Entity (CounterEntity (..), CounterEvent)
import Starter.Counter.Events.CounterDecremented (CounterDecremented (..))

data DecrementCounter = DecrementCounter
  { entityId :: Uuid
  , amount :: Int
  }


getEntityId :: DecrementCounter -> Maybe Uuid
getEntityId cmd = Just cmd.entityId


decide :: DecrementCounter -> Maybe CounterEntity -> RequestContext -> Decision CounterEvent
decide cmd entity _ctx = case entity of
  Nothing ->
    Decider.reject "Counter not found"
  Just existing ->
    if cmd.amount <= 0
      then Decider.reject "Amount must be positive"
      else
        Decider.acceptExisting
          [ CounterDecremented
              { entityId = existing.counterId
              , amount = cmd.amount
              }
          ]


derive ''DecrementCounter command { entity = ''CounterEntity }
