module Integration.App.Cart (
  service,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)
import Service qualified
import Service.Apis.WebApi qualified as WebApi


service :: Service _ _ _ _
service =
  Service.new
    |> Service.useServer WebApi.server
    |> Service.command @CreateCart