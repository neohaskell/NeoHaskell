-- | Second payload handler fixture for outbound dispatch tests.
-- It emits an independent command for the same matched payload.
module Service.OutboundIntegration.SecondHandler (SecondHandler (..), handleEvent) where

import Core
import Integration qualified
import Integration.Command qualified as Command
import Service.OutboundIntegration.PayloadFixtures
import Service.OutboundIntegration.TH (outboundIntegration)


data SecondHandler = SecondHandler


type instance EntityOf SecondHandler = PayloadEntity


handleEvent :: PayloadEntity -> PayloadAdded -> Integration.Outbound
handleEvent _ payload =
  Integration.batch
    [ Integration.outbound
        Command.Emit {command = PayloadCommand {commandValue = payload.addedAmount + 100}}
    ]


outboundIntegration ''SecondHandler
