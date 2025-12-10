module Testbed.Cart.Service (
  service,
) where

import Core
import Record qualified
import Service qualified
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Commands.CreateCart (CreateCart)


service :: Service _
service =
  Service.new
    |> Service.command (Record.Proxy @CreateCart)
    |> Service.command (Record.Proxy @AddItem)