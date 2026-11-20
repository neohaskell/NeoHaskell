-- | Declare account connection changes and token observation.
-- Compose 'accountConnectionFlow' in the OAuth testbed model.
module Testbed.AccountConnection.Feature (accountConnectionFlow) where

import Service.EventModel
import Testbed.AccountConnection.Commands.RecordAccountConnection (RecordAccountConnection)
import Testbed.AccountConnection.Core (
  AccountConnectedPayload,
  AccountConnectionEntity,
  AccountConnectionEvent,
  AccountRemovedPayload,
 )
import Testbed.AccountConnection.Outbound (AccountTokenPresenceObserver)
import Testbed.AccountConnection.Query (AccountConnectionSummary)
import Testbed.AccountConnection.RemovalOutbound (AccountTokenRemovalObserver)


accountConnectionFlow :: FeatureAction
accountConnectionFlow = feature "Account connection flow" do
  chapter "Track account connections" do
    slice "Record an account connection" do
      entity @AccountConnectionEntity do
        command @RecordAccountConnection do
          produces @AccountConnectionEvent

    slice "Show account connection status" do
      query @AccountConnectionSummary do
        readsFrom @AccountConnectionEntity do
          onEvent @AccountConnectionEvent

    slice "Observe token presence" do
      outboundIntegration @AccountTokenPresenceObserver do
        onEvent @AccountConnectedPayload
        triggersCommand @RecordAccountConnection do
          onTargetEntity @AccountConnectionEntity

    slice "Observe token removal" do
      outboundIntegration @AccountTokenRemovalObserver do
        onEvent @AccountRemovedPayload
        triggersCommand @RecordAccountConnection do
          onTargetEntity @AccountConnectionEntity
