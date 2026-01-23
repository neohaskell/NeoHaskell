module Testbed.Document.Service (
  service,
) where

import Core
import Service qualified
import Testbed.Document.Commands.CreateDocument (CreateDocument)
import Testbed.Document.Core ()


service :: Service _ _
service =
  Service.new
    |> Service.command @CreateDocument
