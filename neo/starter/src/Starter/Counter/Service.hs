module Starter.Counter.Service (service) where

import Core
import Service qualified
import Starter.Counter.Commands.CreateCounter (CreateCounter)
import Starter.Counter.Commands.DecrementCounter (DecrementCounter)
import Starter.Counter.Commands.IncrementCounter (IncrementCounter)
import Starter.Counter.Core ()


-- | All commands in the Counter bounded context share `CounterEntity` +
-- `CounterEvent`. The compiler enforces this at the call-site of
-- `Service.command`.
service :: Service _ _
service =
  Service.new
    |> Service.command @CreateCounter
    |> Service.command @IncrementCounter
    |> Service.command @DecrementCounter
