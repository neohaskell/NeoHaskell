module Neo.Project.Service (
  service,
) where

import Core
import Neo.Project.Commands.InitProject (InitProject)
import Service qualified


service :: Service _ _
service =
  Service.new
    |> Service.command @InitProject
