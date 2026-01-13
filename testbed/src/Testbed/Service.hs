module Testbed.Service (
  cartService,
  stockService,
) where

import Testbed.Cart.Service qualified as CartService
import Testbed.Stock.Service qualified as StockService


cartService :: _
cartService = CartService.service


stockService :: _
stockService = StockService.service