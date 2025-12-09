module Testbed.Cart (
  service,
) where

import Core
import Testbed.Cart.Commands.CreateCart (CreateCart)
import Service qualified
import Service.Apis.WebApi qualified as WebApi


service :: Service _ _ _ _
service =
  Service.new
    |> Service.useServer WebApi.server
    |> Service.command @CreateCart