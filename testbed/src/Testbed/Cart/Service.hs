module Testbed.Cart.Service (
  service,
) where

import Core
import Service qualified
import Service.Api.WebApi qualified as WebApi
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Commands.CreateCart (CreateCart)


service :: Service _ _
service =
  Service.new
    |> Service.useServer WebApi.server
    |> Service.command @AddItem
    |> Service.command @CreateCart