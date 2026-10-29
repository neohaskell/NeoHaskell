{-| Register the protected-value command in the maintained testbed application.
Use the query in 'Testbed.Erasable.Query' to inspect its public response.
-}
module Testbed.Erasable.Service (service) where

import Core
import Service qualified
import Testbed.Erasable.Commands.CreateProtected (CreateProtected)
import Testbed.Erasable.Core ()


service :: Service _ _
service = Service.new |> Service.command @CreateProtected
