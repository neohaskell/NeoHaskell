module Integration.App.Cart (
  service,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)


service :: Service _
service = do
  command @CreateCart