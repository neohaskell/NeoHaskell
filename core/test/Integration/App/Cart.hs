module Integration.App.Cart (
  service,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)


service :: Service _ _ _ _
service =
  Service.new
    |> Service.command @CreateCart