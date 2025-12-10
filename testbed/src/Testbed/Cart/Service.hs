module Testbed.Cart.Service (
  service,
) where

import Core
import Record qualified
import Service qualified
import Testbed.Cart.Commands.CreateCart (CreateCart)


service :: Service
service =
  Service.new
    |> Service.command (Record.Proxy @CreateCart)