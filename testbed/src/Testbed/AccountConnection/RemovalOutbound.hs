-- | Observe token storage after an account connection is removed.
-- Keep this handler separate from the connection handler's payload.
module Testbed.AccountConnection.RemovalOutbound (AccountTokenRemovalObserver (..), handleEvent) where

import Core
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)
import Testbed.AccountConnection.Core (AccountConnectionEntity, AccountRemovedPayload (..))
import Testbed.AccountConnection.Outbound (observeTokenStorage)


data AccountTokenRemovalObserver = AccountTokenRemovalObserver


type instance EntityOf AccountTokenRemovalObserver = AccountConnectionEntity


handleEvent :: AccountConnectionEntity -> AccountRemovedPayload -> Integration.Outbound
handleEvent _ payload = observeTokenStorage payload.removedUserId


outboundIntegration ''AccountTokenRemovalObserver
