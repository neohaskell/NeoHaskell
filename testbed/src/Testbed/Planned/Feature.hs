-- | Keep a planned flow visible beside the running testbed flows.
-- Compose 'plannedFlow' in the application's Event Model to inspect its planned components.
module Testbed.Planned.Feature (plannedFlow) where

import Core
import Service.EventModel


data PlannedEntity


data PlannedCommand


data PlannedPayload


data PlannedView


type instance EntityOf PlannedCommand = PlannedEntity


type instance EntityOf PlannedPayload = PlannedEntity


type instance EntitiesOf PlannedView = '[PlannedEntity]


plannedFlow :: FeatureAction
plannedFlow = feature "Planned flow" do
  chapter "Review a future flow" do
    slice "Record a planned change" do
      plannedEntity @PlannedEntity do
        plannedCommand @PlannedCommand do
          plannedProduces @PlannedPayload

    slice "Show a planned view" do
      plannedQuery @PlannedView do
        plannedReadsFrom @PlannedEntity do
          plannedOnEvent @PlannedPayload
