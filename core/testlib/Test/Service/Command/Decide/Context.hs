module Test.Service.Command.Decide.Context (
  Context (..),
  initialize,
) where

import Core
import Uuid qualified


data Context = Context
  { -- Test data
    cartId :: Uuid,
    itemId1 :: Uuid,
    itemId2 :: Uuid
  }


initialize :: Task Text Context
initialize = do
  cartId <- Uuid.generate
  itemId1 <- Uuid.generate
  itemId2 <- Uuid.generate

  pure Context {cartId, itemId1, itemId2}
