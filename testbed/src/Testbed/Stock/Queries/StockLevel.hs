{-# LANGUAGE TemplateHaskell #-}

module Testbed.Stock.Queries.StockLevel (
  StockLevel (..),
  canAccess,
  canView,
) where

import Core
import Json qualified
import Service.Query.Auth (QueryAuthError, UserClaims, publicAccess, publicView)
import Service.Query.TH (deriveQuery)
import Testbed.Stock.Core (StockEntity (..))


data StockLevel = StockLevel
  { stockLevelId :: Uuid
  , productId :: Uuid
  , available :: Int
  , reserved :: Int
  }
  deriving (Generic, Eq, Show)


instance Json.FromJSON StockLevel


instance Json.ToJSON StockLevel


instance ToSchema StockLevel


-- | Authorization: Anyone can access stock levels (public catalog data)
canAccess :: Maybe UserClaims -> Maybe QueryAuthError
canAccess = publicAccess


-- | Authorization: Anyone can view any stock level
canView :: Maybe UserClaims -> StockLevel -> Maybe QueryAuthError
canView = publicView


-- | Use TH to derive Query instances.
-- Wires canAccess -> canAccessImpl, canView -> canViewImpl
deriveQuery ''StockLevel [''StockEntity]


instance QueryOf StockEntity StockLevel where
  queryId stock = stock.stockId

  combine stock _maybeExisting =
    Update
      StockLevel
        { stockLevelId = stock.stockId
        , productId = stock.productId
        , available = stock.available
        , reserved = stock.reserved
        }
