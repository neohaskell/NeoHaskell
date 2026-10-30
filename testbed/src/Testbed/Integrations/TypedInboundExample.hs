-- | Emit cart and stock commands from one typed inbound worker.
-- Register 'TypedInboundExample' with 'Application.withInboundHandler'.
module Testbed.Integrations.TypedInboundExample (
  TypedInboundExample (..),
  workerImpl,
) where

import AsyncTask qualified
import Core
import Integration qualified
import Testbed.Cart.Commands.CreateTypedInboundCart (CreateTypedInboundCart (..))
import Testbed.Cart.Core (CartEntity)
import Testbed.Stock.Commands.InitializeStockInternal (InitializeStockInternal (..))
import Testbed.Stock.Core (StockEntity)
import Uuid qualified


data TypedInboundExample = TypedInboundExample


workerImpl ::
  () -> Integration.TypedInbound '[ '(CreateTypedInboundCart, CartEntity), '(InitializeStockInternal, StockEntity)]
workerImpl _ =
  Integration.typedInbound
    Integration.TypedInboundConfig
      { Integration.run = \emit -> do
          let loop = do
                emit CreateTypedInboundCart
                emit InitializeStockInternal {productId = Uuid.nil, available = 579}
                AsyncTask.sleep 3000
                loop
          loop
      }


inboundIntegration ''TypedInboundExample
