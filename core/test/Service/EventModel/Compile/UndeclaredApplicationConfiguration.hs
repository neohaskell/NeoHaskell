-- | Reject a public callback that bypasses Event Model configuration checks.
module Service.EventModel.Compile.UndeclaredApplicationConfiguration where

import Service.Application qualified as Application
import Service.EventModel qualified as EventModel
import Testbed.Config (TestbedConfig)


invalid :: EventModel.EventModelAction
invalid = EventModel.withApplicationConfiguration (Application.withConfig @TestbedConfig)
