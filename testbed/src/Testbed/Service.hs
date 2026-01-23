module Testbed.Service (
  cartService,
  stockService,
  documentService,
) where

import Testbed.Cart.Service qualified as CartService
import Testbed.Document.Service qualified as DocumentService
import Testbed.Stock.Service qualified as StockService


cartService :: _
cartService = CartService.service


stockService :: _
stockService = StockService.service


documentService :: _
documentService = DocumentService.service