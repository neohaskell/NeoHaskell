module Testbed.Stock.Commands.ReserveStock (
  ReserveStock (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Command.Core (TransportOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Stock.Core


-- | Command to reserve stock for a cart.
-- This is triggered by the Cart integration when an item is added.
data ReserveStock = ReserveStock
  { stockId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON ReserveStock


instance Json.ToJSON ReserveStock


getEntityId :: ReserveStock -> Maybe Uuid
getEntityId cmd = Just cmd.stockId


decide :: ReserveStock -> Maybe StockEntity -> Decision StockEvent
decide cmd entity = case entity of
  Nothing ->
    Decider.reject "Stock not found!"
  Just stock ->
    if cmd.quantity <= 0
      then Decider.reject "Quantity must be positive"
      else
        if stock.available >= cmd.quantity
          then
            Decider.acceptExisting
              [ StockReserved
                  { entityId = stock.stockId
                  , quantity = cmd.quantity
                  , cartId = cmd.cartId
                  }
              ]
          else Decider.reject "Insufficient stock available!"


type instance EntityOf ReserveStock = StockEntity


-- NOTE: Using WebTransport for testability. In production, this would use InternalTransport
-- to indicate it's only triggered by integrations, not exposed via web.
type instance TransportOf ReserveStock = WebTransport


command ''ReserveStock
