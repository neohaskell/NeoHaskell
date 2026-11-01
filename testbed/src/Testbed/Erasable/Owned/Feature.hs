-- | Declare owned protected records and the erasure callback.
-- Compose 'ownedErasureFlow' in either testbed application model.
module Testbed.Erasable.Owned.Feature (ownedErasureFlow) where

import Service.EventModel
import Testbed.Erasable.Owned.Commands.Create (CreateOwnedProtected)
import Testbed.Erasable.Owned.Commands.Record (RecordErasureResult)
import Testbed.Erasable.Owned.Commands.Request (RequestErasure)
import Testbed.Erasable.Owned.Core (ErasureRequestedPayload, OwnedProtectedEntity, OwnedProtectedEvent)
import Testbed.Erasable.Owned.Outbound (OwnedErasureOutbound)
import Testbed.Erasable.Owned.Query (OwnedProtectedView)


ownedErasureFlow :: FeatureAction
ownedErasureFlow = feature "Owned erasure flow" do
  chapter "Erase protected values" do
    slice "Create an owned protected record" do
      entity @OwnedProtectedEntity do
        command @CreateOwnedProtected do
          produces @OwnedProtectedEvent

    slice "Request erasure" do
      entity @OwnedProtectedEntity do
        command @RequestErasure do
          produces @ErasureRequestedPayload

    slice "Record erasure result" do
      entity @OwnedProtectedEntity do
        command @RecordErasureResult do
          produces @OwnedProtectedEvent

    slice "Show owned protected values" do
      query @OwnedProtectedView do
        readsFrom @OwnedProtectedEntity do
          onEvent @OwnedProtectedEvent

    slice "Erase a requested value" do
      outboundIntegration @OwnedErasureOutbound do
        onEvent @ErasureRequestedPayload
        triggersCommand @RecordErasureResult do
          onTargetEntity @OwnedProtectedEntity
