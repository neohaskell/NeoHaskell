{-# LANGUAGE TemplateHaskell #-}

module Testbed.Cart.Queries.CartSummary (
  CartSummary (..),
) where

import Array qualified
import Core
import Json qualified
import Service.Query.Core (EntitiesOf, Query, QueryAction (..), QueryOf (..))
import Service.Query.TH (deriveQuery)
import Testbed.Cart.Core (CartEntity (..))


-- | Query (read model) that summarizes a cart's state.
--
-- This denormalizes cart data for fast reads, tracking:
-- - The cart ID
-- - Total number of items in the cart
-- - Whether the cart is empty
data CartSummary = CartSummary
  { cartSummaryId :: Uuid,
    itemCount :: Int,
    isEmpty :: Bool
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON CartSummary


instance Json.FromJSON CartSummary


-- | Use TH to derive Query instances.
-- Generates: Query instance, NameOf "cart-summary", EntitiesOf '[CartEntity], KnownHash
deriveQuery ''CartSummary [''CartEntity]


-- | Define how CartEntity contributes to CartSummary.
instance QueryOf CartEntity CartSummary where
  -- | The query is keyed by the cart ID
  queryId cart = cart.cartId

  -- | When a cart changes, update the summary
  combine cart _maybeExisting = do
    let count = cart.items |> Array.length
    Update
      CartSummary
        { cartSummaryId = cart.cartId,
          itemCount = count,
          isEmpty = count == 0
        }
