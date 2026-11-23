module Starter.Counter.Commands.CreateCounter (
  CreateCounter (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Derive
import Service.Auth (RequestContext)
import Starter.Counter.Entity (CounterEntity, CounterEvent)
import Starter.Counter.Events.CounterCreated (CounterCreated (..))

data CreateCounter = CreateCounter
  { label :: Text
  }

getEntityId :: CreateCounter -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCounter -> Maybe CounterEntity -> RequestContext -> Decision CounterEvent
decide cmd entity _ctx = case entity of
  Just _ ->
    Decider.reject "Counter already exists"
  Nothing ->
    if cmd.label == ""
      then Decider.reject "Label cannot be empty"
      else do
        newId <- Decider.generateUuid
        Decider.acceptNew
          [ CounterCreated
              { entityId = newId
              , label = cmd.label
              }
          ]


derive ''CreateCounter command { entity = ''CounterEntity }
