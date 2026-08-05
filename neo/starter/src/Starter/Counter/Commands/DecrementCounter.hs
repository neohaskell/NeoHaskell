module Starter.Counter.Commands.DecrementCounter (
  DecrementCounter (..),
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
import Starter.Counter.Events.CounterDecremented qualified as CounterDecremented


-- | Decrement an existing counter by `amount`.
data DecrementCounter = DecrementCounter
  { entityId :: Uuid
  , amount :: Int
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON DecrementCounter


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
              CounterDecremented.Event
                { entityId = existing.counterId
                , amount = cmd.amount
                }
          ]


type instance EntityOf DecrementCounter = CounterEntity


type instance TransportsOf DecrementCounter = '[WebTransport]


command ''DecrementCounter
