-- | Define a config-dependent inbound handler for the startup-order test.
-- The marker gives Application a typed worker factory.
module Service.InboundIntegration.ConfiguredFixture where

import Config (defineConfig)
import Config qualified
import Core
import Integration qualified
import Service.Transport.InternalSpec qualified as First
import Task qualified
import Uuid qualified


defineConfig
  "InboundWorkerSettings"
  [ Config.field @Bool "enabled"
      |> Config.doc "Whether the inbound worker emits its command"
      |> Config.defaultsTo True
  ]


data ConfiguredWorker = ConfiguredWorker


workerImpl :: InboundWorkerSettings -> Integration.TypedInbound '[ '(First.CreateInternalRecord, First.InternalEntity)]
workerImpl settings =
  if settings.enabled
    then
      Integration.typedInbound Integration.TypedInboundConfig
        { Integration.run = \emit -> emit First.CreateInternalRecord {First.recordId = Uuid.nil}
        }
    else
      Integration.typedInbound Integration.TypedInboundConfig
        { Integration.run = \_ -> Task.yield unit
        }


inboundIntegration ''ConfiguredWorker
