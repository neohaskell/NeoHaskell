{-# LANGUAGE TemplateHaskell #-}

module Testbed.Cart.Queries.CartSummary (
  CartSummary (..),
  canAccess,
  canView,
) where

import Array qualified
import Core
import Json qualified
import Service.Query.Auth (QueryAuthError, UserClaims, publicAccess, publicView)
import Service.Query.TH (deriveQuery)
import Testbed.Cart.Core (CartEntity (..))


-- | Query (read model) that summarizes a cart's state.
--
-- This denormalizes cart data for fast reads, tracking:
-- - The cart ID
-- - The owner ID (for authorization, matches UserClaims.sub)
-- - Total number of items in the cart
-- - Whether the cart is empty
data CartSummary = CartSummary
  { cartSummaryId :: Uuid,
    ownerId :: Text,
    itemCount :: Int,
    isEmpty :: Bool
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON CartSummary


instance Json.FromJSON CartSummary


-- | Authorization: Public access for demo/testing purposes
-- In a real app, you might use authenticatedAccess here
canAccess :: Maybe UserClaims -> Maybe QueryAuthError
canAccess = publicAccess


-- | Authorization: Public view for demo/testing purposes
-- In a real app, you might use ownerOnly (.ownerId) here
canView :: Maybe UserClaims -> CartSummary -> Maybe QueryAuthError
canView = publicView


-- | Use TH to derive Query instances.
-- Generates: Query instance, NameOf "cart-summary", EntitiesOf '[CartEntity], KnownHash
-- Wires canAccess -> canAccessImpl, canView -> canViewImpl
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
          ownerId = cart.ownerId,
          itemCount = count,
          isEmpty = count == 0
        }
