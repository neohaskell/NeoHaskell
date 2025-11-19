module Test.Service.CommandHandler.Execute.Context (
  Context (..),
  initialize,
) where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Test.Service.Command.Core (CartEntity, CartEvent, OrderEntity, OrderEvent)
import Test.Service.CommandHandler.Core (CommandHandlerResult)


data Context = Context
  { -- Event Store and Fetchers
    cartStore :: EventStore CartEvent,
    cartFetcher :: EntityFetcher CartEntity CartEvent,
    orderStore :: EventStore OrderEvent,
    orderFetcher :: EntityFetcher OrderEntity OrderEvent,
    -- CommandHandler (to be implemented)
    -- executeCommand :: forall cmd. Command cmd => cmd -> Task Text CommandHandlerResult
    -- Test data
    cartStreamId :: Event.StreamId,
    orderStreamId :: Event.StreamId,
    cartEntityName :: Event.EntityName,
    orderEntityName :: Event.EntityName,
    cartId :: Uuid,
    itemId1 :: Uuid,
    itemId2 :: Uuid,
    orderId :: Uuid,
    customerId :: Uuid,
    tenantId :: Uuid
  }


initialize ::
  Task Text (EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Task Text Context
initialize newCartStoreAndFetcher newOrderStoreAndFetcher = do
  (cartStore, cartFetcher) <- newCartStoreAndFetcher
  (orderStore, orderFetcher) <- newOrderStoreAndFetcher

  cartStreamId <- StreamId.new
  orderStreamId <- StreamId.new

  let cartEntityName = Event.EntityName "Cart"
  let orderEntityName = Event.EntityName "Order"

  cartId <- Uuid.generate
  itemId1 <- Uuid.generate
  itemId2 <- Uuid.generate
  orderId <- Uuid.generate
  customerId <- Uuid.generate
  tenantId <- Uuid.generate

  pure
    Context
      { cartStore,
        cartFetcher,
        orderStore,
        orderFetcher,
        cartStreamId,
        orderStreamId,
        cartEntityName,
        orderEntityName,
        cartId,
        itemId1,
        itemId2,
        orderId,
        customerId,
        tenantId
      }
