module Integration.App.Cart (
  service,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)
import Service.ServiceDefinition qualified as Service


service :: Service _ _ _ _
service =
  Service.new
    |> Service.command @CreateCart