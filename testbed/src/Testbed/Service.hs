module Testbed.Service (
  service,
) where

import Service.ServiceDefinition.Core qualified as Service
import Testbed.Cart.Service qualified as CartService


service :: Service.Service _ _
service = CartService.service