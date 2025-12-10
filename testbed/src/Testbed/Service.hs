module Testbed.Service (
  service,
) where

import Core
import Testbed.Cart.Service qualified as CartService


service :: Service _
service = CartService.service