module Testbed.Cart.Integrations.ReserveStockOnItemAdded (
  ReserveStockOnItemAdded (..),
  handleEvent,
) where

import Core
import Integration qualified
import Integration.Command qualified as Command
import Service.OutboundIntegration.TH (outboundIntegration)
import Testbed.Cart.Core (CartEntity (..), ItemAddedPayload (..))
import Testbed.Stock.Commands.ReserveStock (ReserveStock (..))


data ReserveStockOnItemAdded = ReserveStockOnItemAdded


type instance EntityOf ReserveStockOnItemAdded = CartEntity


handleEvent :: CartEntity -> ItemAddedPayload -> Integration.Outbound
handleEvent cart payload =
  Integration.batch
    [ Integration.outbound
        Command.Emit
          { command =
              ReserveStock
                { stockId = payload.itemAddedStockId
                , quantity = payload.itemAddedQuantity
                , cartId = cart.cartId
                }
          }
    ]


outboundIntegration ''ReserveStockOnItemAdded
