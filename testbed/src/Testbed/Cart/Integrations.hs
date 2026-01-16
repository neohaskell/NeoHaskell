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
--
-- Note: The entity parameter is currently not fully reconstructed (placeholder).
-- We use the event's entityId directly instead of cart.cartId since they're equivalent.
cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
cartIntegrations _cart event = case event of
  CartCreated {} -> Integration.none
  ItemAdded {entityId, stockId, quantity} -> Integration.batch
    [ Integration.outbound Command.Emit
        { command = ReserveStock
            { stockId = stockId
            , quantity = quantity
            , cartId = entityId
            }
        }
    ]


-- | Inbound integration: periodically creates new carts.
-- Every 3 seconds, submits a CreateCart command.
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.every Timer.Every
    { interval = Timer.seconds 3
    , toCommand = \_ -> CreateCart
    }
