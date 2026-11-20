-- | First payload handler fixture for outbound dispatch tests.
-- It emits a command containing the matched payload's amount.
module Service.OutboundIntegration.FirstHandler (FirstHandler (..), handleEvent) where

import Core
import Integration qualified
import Integration.Command qualified as Command
import Service.OutboundIntegration.PayloadFixtures
import Service.OutboundIntegration.TH (outboundIntegration)


data FirstHandler = FirstHandler


type instance EntityOf FirstHandler = PayloadEntity


handleEvent :: PayloadEntity -> PayloadAdded -> Integration.Outbound
handleEvent _ payload =
  Integration.batch
    [ Integration.outbound
        Command.Emit {command = PayloadCommand {commandValue = payload.addedAmount}}
    ]


outboundIntegration ''FirstHandler
