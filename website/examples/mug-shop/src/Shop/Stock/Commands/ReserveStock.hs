module Shop.Stock.Commands.ReserveStock (
  ReserveStock (..),
  getEntityId,
  decide,
) where

import Core
import Shop.Stock.Events.StockReserved qualified as StockReserved
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.Transport.Internal (InternalTransport)
import Shop.Stock.Core

-- | Command to reserve stock for a cart.
-- Keep reservation internal; the integration lesson supplies its trigger.
data ReserveStock = ReserveStock
  { stockId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }

getEntityId :: ReserveStock -> Maybe Uuid
getEntityId cmd = Just cmd.stockId

decide :: ReserveStock -> Maybe StockEntity -> RequestContext -> Decision StockEvent
decide request existing _context = case existing of
  Nothing -> Decider.reject "Stock not found!"
  Just stock -> reservePositiveQuantity request stock

reservePositiveQuantity :: ReserveStock -> StockEntity -> Decision StockEvent
reservePositiveQuantity request stock =
  if request.quantity <= 0
    then Decider.reject "Quantity must be positive"
    else reserveAvailableStock request stock

reserveAvailableStock :: ReserveStock -> StockEntity -> Decision StockEvent
reserveAvailableStock request stock =
  if request.quantity > stock.available
    then Decider.reject "Insufficient stock available!"
    else Decider.acceptExisting
      [StockReserved (StockReserved.Event {entityId = stock.stockId, quantity = request.quantity, cartId = request.cartId})]

type instance EntityOf ReserveStock = StockEntity

type instance TransportsOf ReserveStock = '[InternalTransport]

deriveCommand ''ReserveStock
