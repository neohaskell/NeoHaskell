module Starter.Counter.Commands.CreateCounter (
  CreateCounter (..),
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
import Starter.Counter.Events.CounterCreated qualified as CounterCreated


-- | Create a new counter. The entity id is generated in `decide`.
data CreateCounter = CreateCounter
  { label :: Text
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON CreateCounter


-- | `Nothing` means "creation" — no existing entity should be loaded.
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
              CounterCreated.Event
                { entityId = newId
                , label = cmd.label
                }
          ]


type instance EntityOf CreateCounter = CounterEntity


type instance TransportsOf CreateCounter = '[WebTransport]


command ''CreateCounter
