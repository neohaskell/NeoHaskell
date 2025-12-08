module Integration.App.Cart (
  service,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)


service :: Service _ _ _ _
service = do
  use WebApi.server
  command @CreateCart