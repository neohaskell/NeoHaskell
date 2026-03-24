module Testbed.Cart.Integrations (
  periodicCartCreator,
) where

import Integration qualified
import Integration.Timer qualified as Timer
import Testbed.Cart.Commands.CreateCartInternal (CreateCartInternal (..))


-- | Inbound integration: periodically creates new carts.
-- Every 3 seconds, submits a CreateCartInternal command.
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.every Timer.Every
    { interval = Timer.seconds 3
    , toCommand = \_ -> CreateCartInternal
    }
