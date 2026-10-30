-- | Reject a planned command assigned to another entity.
-- Compile this module with the Event Model rejection runner.
module Service.EventModel.Compile.WrongPlannedCommandTarget where

import Core
import Service.EventModel


data FirstEntity


data SecondEntity


data PlannedCommand


type instance EntityOf PlannedCommand = SecondEntity


invalid :: FeatureAction
invalid = feature "Planned" do
  chapter "Change" do
    slice "Wrong planned target" do
      plannedEntity @FirstEntity do
        plannedCommand @PlannedCommand do
          pass
