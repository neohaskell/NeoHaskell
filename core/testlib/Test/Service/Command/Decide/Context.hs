module Test.Service.Command.Decide.Context (
  Context (..),
  initialize,
) where

import Core
import Test.Service.Command.Core (
  AddItemToCart,
  CancelOrder,
  CartEntity,
  CheckoutCart,
  CreateOrder,
  OrderEntity,
  RemoveItemFromCart,
 )


data Context = Context
  { -- Test data
    cartId :: Uuid,
    itemId1 :: Uuid,
    itemId2 :: Uuid,
    orderId :: Uuid,
    customerId :: Uuid,
    tenantId :: Uuid
  }


initialize :: Task Text Context
initialize = do
  cartId <- Uuid.generate
  itemId1 <- Uuid.generate
  itemId2 <- Uuid.generate
  orderId <- Uuid.generate
  customerId <- Uuid.generate
  tenantId <- Uuid.generate

  pure Context {cartId, itemId1, itemId2, orderId, customerId, tenantId}
