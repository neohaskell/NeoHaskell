module Testbed.Stock.Queries.StockLevel (
  StockLevel (..),
) where

import Core
import Json qualified
import Service.Query (Query (..))
import Testbed.Stock.Core (StockEntity (..), StockEvent (..))


data StockLevel = StockLevel
  { stockLevelId :: Uuid
  , productId :: Uuid
  , available :: Int
  , reserved :: Int
  }
  deriving (Generic, Eq, Show)


instance Json.FromJSON StockLevel


instance Json.ToJSON StockLevel


type instance NameOf StockLevel = "StockLevel"


instance Query StockLevel where
  type QueryEvent StockLevel = StockEvent


  emptyState =
    StockLevel
      { stockLevelId = Uuid.nil
      , productId = Uuid.nil
      , available = 0
      , reserved = 0
      }


  update event query = case event of
    StockInitialized {entityId, productId, available} ->
      query
        { stockLevelId = entityId
        , productId = productId
        , available = available
        , reserved = 0
        }
    StockReserved {quantity} ->
      query
        { available = query.available - quantity
        , reserved = query.reserved + quantity
        }


  getId query = query.stockLevelId
