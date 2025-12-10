module Testbed.Cart.Service (
  service,
) where

import Core
import Service qualified
import Service.Apis.WebApi qualified as WebApi
import Testbed.Cart.Commands.CreateCart (CreateCart)


service :: Service
service =
  Service.new
    |> Service.useServer WebApi.server
    |> Service.command @CreateCart