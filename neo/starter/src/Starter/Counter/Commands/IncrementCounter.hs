module Starter.Counter.Commands.IncrementCounter (
  IncrementCounter (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Starter.Counter.Core
import Starter.Counter.Events.CounterIncremented qualified as CounterIncremented


-- | Increment an existing counter by `amount`.
data IncrementCounter = IncrementCounter
  { entityId :: Uuid
  , amount :: Int
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON IncrementCounter


-- | Returning `Just` tells the framework to load this entity before calling `decide`.
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
              CounterIncremented.Event
                { entityId = existing.counterId
                , amount = cmd.amount
                }
          ]


type instance EntityOf IncrementCounter = CounterEntity


type instance TransportsOf IncrementCounter = '[WebTransport]


command ''IncrementCounter
