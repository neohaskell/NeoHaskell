module Testbed.AccountConnection.Service (
  service,
) where

import Core
import Service qualified
import Testbed.AccountConnection.Commands.RecordAccountConnection (RecordAccountConnection)
import Testbed.AccountConnection.Core ()


service :: Service _ _
service =
  Service.new
    |> Service.command @RecordAccountConnection
