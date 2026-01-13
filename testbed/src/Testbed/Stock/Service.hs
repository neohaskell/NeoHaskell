module Testbed.Stock.Service (
  service,
) where

import Core
import Service qualified
import Testbed.Stock.Commands.InitializeStock (InitializeStock)
import Testbed.Stock.Commands.ReserveStock (ReserveStock)
import Testbed.Stock.Core ()


service :: Service _ _
service =
  Service.new
    |> Service.command @InitializeStock
    |> Service.command @ReserveStock
