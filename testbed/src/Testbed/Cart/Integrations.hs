module Testbed.Cart.Integrations (
  periodicCartCreator,
) where

import Integration qualified
import Integration.Timer qualified as Timer
import Testbed.Cart.Commands.CreateCart (CreateCart (..))


-- | Inbound integration: periodically creates new carts.
-- Every 3 seconds, submits a CreateCart command.
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.every Timer.Every
    { interval = Timer.seconds 3
    , toCommand = \_ -> CreateCart
    }
