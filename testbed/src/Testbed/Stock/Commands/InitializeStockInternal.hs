-- | Initialize stock through an internal command emitted by the typed inbound example.
module Testbed.Stock.Commands.InitializeStockInternal (
  InitializeStockInternal (..),
  getEntityId,
  decide,
) where

import Core
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Internal (InternalTransport)
import Testbed.Stock.Commands.InitializeStock (InitializeStock (..))
import Testbed.Stock.Commands.InitializeStock qualified as InitializeStock
import Testbed.Stock.Core (StockEntity, StockEvent)


data InitializeStockInternal = InitializeStockInternal
  { productId :: Uuid,
    available :: Int
  }


getEntityId :: InitializeStockInternal -> Maybe Uuid
getEntityId _ = Nothing


decide :: InitializeStockInternal -> Maybe StockEntity -> RequestContext -> Decision StockEvent
decide cmd entity context =
  InitializeStock.decide
    InitializeStock
      { productId = cmd.productId,
        available = cmd.available
      }
    entity
    context


type instance EntityOf InitializeStockInternal = StockEntity


type instance TransportsOf InitializeStockInternal = '[InternalTransport]


command ''InitializeStockInternal
