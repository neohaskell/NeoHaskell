module Testbed.Cart.Integrations (
  cartIntegrations,
  periodicCartCreator,
) where

import Integration qualified
import Integration.Command qualified as Command
import Integration.Timer qualified as Timer
import Testbed.Cart.Commands.CreateCart (CreateCart (..))
import Testbed.Cart.Core (CartEntity (..), CartEvent (..))
import Testbed.Stock.Commands.ReserveStock (ReserveStock (..))


-- | Outbound integration: reacts to cart events and triggers cross-domain commands.
-- When an item is added to a cart, reserve stock in the Stock domain.
-- This is a Process Manager pattern - coordinating across aggregate boundaries.
cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
cartIntegrations cart event = case event of
  CartCreated {} -> Integration.none
  ItemAdded {stockId, quantity} -> Integration.batch
    [ Integration.outbound Command.Emit
        { command = ReserveStock
            { stockId = stockId
            , quantity = quantity
            , cartId = cart.cartId
            }
        }
    ]


-- | Inbound integration: periodically creates new carts.
-- Every 30 seconds, submits a CreateCart command.
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.every Timer.Every
    { interval = Timer.seconds 30
    , toCommand = \_ -> CreateCart
    }
