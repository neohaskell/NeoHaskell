module Testbed.Cart.Integrations (
  cartIntegrations,
  periodicCartCreator,
) where

import Core
import Integration qualified
import Integration.Printer qualified as Printer
import Integration.Timer qualified as Timer
import Testbed.Cart.Commands.CreateCart (CreateCart (..))
import Testbed.Cart.Core (CartEntity, CartEvent (..))


-- | Outbound integration: reacts to cart events and triggers external actions.
-- Logs a message whenever a new cart is created.
cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
cartIntegrations cart event = case event of
  CartCreated {entityId} -> Integration.batch
    [ Integration.outbound Printer.Print {message = [fmt|New cart created with ID: {entityId}|]}
    ]
  _ -> Integration.none


-- | Inbound integration: periodically creates new carts.
-- Every 30 seconds, submits a CreateCart command.
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.every
    { interval = Timer.seconds 30
    , toCommand = \_ -> CreateCart
    }
