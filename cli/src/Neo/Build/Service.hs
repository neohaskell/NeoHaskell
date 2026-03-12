module Neo.Build.Service (
  service,
) where

import Core
import Neo.Build.Commands.StartBuild (StartBuild)
import Service qualified


service :: Service _ _
service =
  Service.new
    |> Service.command @StartBuild
