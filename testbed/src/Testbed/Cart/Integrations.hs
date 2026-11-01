-- | Create carts from the timer worker declared by the cart Feature.
module Testbed.Cart.Integrations (PeriodicCartCreator (..), workerImpl) where

import AsyncTask qualified
import Core
import Integration qualified
import Testbed.Cart.Commands.CreateCartInternal (CreateCartInternal (..))
import Testbed.Cart.Core (CartEntity)


data PeriodicCartCreator = PeriodicCartCreator


workerImpl :: () -> Integration.TypedInbound '[ '(CreateCartInternal, CartEntity)]
workerImpl _ =
  Integration.typedInbound
    Integration.TypedInboundConfig
      { Integration.run = \emit -> do
          let loop = do
                emit CreateCartInternal
                AsyncTask.sleep 3000
                loop
          loop
      }


inboundIntegration ''PeriodicCartCreator
