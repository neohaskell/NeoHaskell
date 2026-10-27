module Testbed.ExternalEffect.Service (
  service,
) where

import Core
import Service qualified
import Testbed.ExternalEffect.Commands.RecordExternalEffect (RecordExternalEffect)
import Testbed.ExternalEffect.Commands.RequestExternalEffect (RequestExternalEffect)
import Testbed.ExternalEffect.Core ()


service :: Service _ _
service =
  Service.new
    |> Service.command @RequestExternalEffect
    |> Service.command @RecordExternalEffect
