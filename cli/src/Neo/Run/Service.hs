module Neo.Run.Service (
  service,
) where

import Core
import Neo.Run.Commands.RunProject (RunProject)
import Service qualified


service :: Service _ _
service =
  Service.new
    |> Service.command @RunProject
