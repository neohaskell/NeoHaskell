module Integration.App.Cart (
  serviceDefinition,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)


serviceDefinition :: ServiceDefinition _ Unit
serviceDefinition = do
  command @CreateCart