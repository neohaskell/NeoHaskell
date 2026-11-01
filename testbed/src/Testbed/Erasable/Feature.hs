-- | Declare protected record creation and its public projection.
-- Compose 'protectedFlow' in the application model.
module Testbed.Erasable.Feature (protectedFlow) where

import Service.EventModel
import Testbed.Erasable.Commands.CreateProtected (CreateProtected)
import Testbed.Erasable.Core (ProtectedCreated, ProtectedEntity)
import Testbed.Erasable.Query (ProtectedView)


protectedFlow :: FeatureAction
protectedFlow = feature "Protected record flow" do
  chapter "Store protected values" do
    slice "Create a protected record" do
      entity @ProtectedEntity do
        command @CreateProtected do
          produces @ProtectedCreated

    slice "Show a protected record" do
      query @ProtectedView do
        readsFrom @ProtectedEntity do
          onEvent @ProtectedCreated
