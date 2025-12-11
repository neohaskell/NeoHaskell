module Testbed.Cart.Service (
  service,
) where

import Core
import Service qualified
import Service.Command.Core (webApiServer)
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Commands.CreateCart (CreateCart)


service :: Service _ _
service =
  Service.new
    |> Service.useServer webApiServer
    |> Service.command @CreateCart
    |> Service.command @AddItem