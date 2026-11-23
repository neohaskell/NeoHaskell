-- | Start a declared application from its executable entry point.
module Service.EventModel.Launcher (run) where

import Core
import Result qualified
import Service.Application qualified as Application
import Service.EventModel qualified as EventModel


-- | Report declaration errors before acquiring any application resources.
run :: EventModel -> Task Text Unit
run model = Application.runValidated (Result.map EventModel.toApplication (EventModel.validate model))
