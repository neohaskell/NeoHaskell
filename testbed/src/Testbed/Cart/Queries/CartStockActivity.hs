{-# LANGUAGE TemplateHaskell #-}

-- | Project declared cart and stock events into one shared view.
-- Register 'CartStockActivity' in a query Slice with both entity sources.
module Testbed.Cart.Queries.CartStockActivity (CartStockActivity (..), canAccess, canView) where

import Array qualified
import Core
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.Cart.Core (CartEntity (..))
import Testbed.Stock.Core (StockEntity (..))
import Uuid qualified


data CartStockActivity = CartStockActivity
  { activityId :: Uuid,
    cartSeen :: Bool,
    stockSeen :: Bool,
    cartItemCount :: Int,
    stockReserved :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = AccessControl.publicAccess claims


canView :: Maybe UserClaims -> CartStockActivity -> Maybe AccessError
canView claims activity = AccessControl.publicView claims activity


deriveQuery ''CartStockActivity [''CartEntity, ''StockEntity]


instance QueryOf CartEntity CartStockActivity where
  queryId _ = Uuid.nil

  combine cart previous = do
    let current = case previous of
          Just activity -> activity
          Nothing -> emptyActivity
    Update current {cartSeen = True, cartItemCount = Array.length cart.items}


instance QueryOf StockEntity CartStockActivity where
  queryId _ = Uuid.nil

  combine stock previous = do
    let current = case previous of
          Just activity -> activity
          Nothing -> emptyActivity
    Update current {stockSeen = True, stockReserved = stock.reserved}


emptyActivity :: CartStockActivity
emptyActivity = CartStockActivity {activityId = Uuid.nil, cartSeen = False, stockSeen = False, cartItemCount = 0, stockReserved = 0}
