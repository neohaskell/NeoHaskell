{-# LANGUAGE TemplateHaskell #-}

-- | Project declared cart and stock events into one shared view.
-- Register 'CartStockActivity' in a query Slice with both entity sources.
module Testbed.Cart.Queries.CartStockActivity (CartStockActivity (..), canAccess, canView) where

import Array qualified
import Core
import Derive
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


projectionIdFromCartEntity :: CartEntity -> Uuid
projectionIdFromCartEntity _ = Uuid.nil


combineCartEntity :: CartEntity -> Maybe CartStockActivity -> QueryAction CartStockActivity
combineCartEntity cart previous = do
  let current = case previous of
        Just activity -> activity
        Nothing -> emptyActivity
  Update current {cartSeen = True, cartItemCount = Array.length cart.items}


projectionIdFromStockEntity :: StockEntity -> Uuid
projectionIdFromStockEntity _ = Uuid.nil


combineStockEntity :: StockEntity -> Maybe CartStockActivity -> QueryAction CartStockActivity
combineStockEntity stock previous = do
  let current = case previous of
        Just activity -> activity
        Nothing -> emptyActivity
  Update current {stockSeen = True, stockReserved = stock.reserved}


emptyActivity :: CartStockActivity
emptyActivity = CartStockActivity {activityId = Uuid.nil, cartSeen = False, stockSeen = False, cartItemCount = 0, stockReserved = 0}


derive ''CartStockActivity query { entities = [''CartEntity, ''StockEntity] }
