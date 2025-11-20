module Test.Service.CommandHandler.Execute.Context (
  Context (..),
  initialize,
) where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Test.Service.Command.Core (CartEntity, CartEvent)
import Uuid qualified


data Context = Context
  { -- Event Store and Fetchers
    cartStore :: EventStore CartEvent,
    cartFetcher :: EntityFetcher CartEntity CartEvent,
    -- CommandHandler (to be implemented)
    -- executeCommand :: forall cmd. Command cmd => cmd -> Task Text CommandHandlerResult
    -- Test data
    cartStreamId :: Event.StreamId,
    cartEntityName :: Event.EntityName,
    cartId :: Uuid,
    itemId1 :: Uuid,
    itemId2 :: Uuid
  }


initialize ::
  Task Text (EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text Context
initialize newCartStoreAndFetcher = do
  (cartStore, cartFetcher) <- newCartStoreAndFetcher

  cartStreamId <- StreamId.new

  let cartEntityName = Event.EntityName "Cart"

  cartId <- Uuid.generate
  itemId1 <- Uuid.generate
  itemId2 <- Uuid.generate

  pure
    Context
      { cartStore,
        cartFetcher,
        cartStreamId,
        cartEntityName,
        cartId,
        itemId1,
        itemId2
      }
