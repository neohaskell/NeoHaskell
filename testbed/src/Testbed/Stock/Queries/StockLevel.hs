{-# LANGUAGE TemplateHaskell #-}

module Testbed.Stock.Queries.StockLevel (
  StockLevel (..),
) where

import Core
import Json qualified
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
