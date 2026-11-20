{-| Register neoctl's placeholder query as its only Event Model Feature.
The Feature satisfies model validation without exposing business commands.
-}
module Neoctl.Feature (flow) where

import Core
import Neoctl.Placeholder (Placeholder)
import Service.EventModel


flow :: FeatureAction
flow = feature "neoctl" do
  chapter "Startup" do
    slice "Placeholder query" do
      query @Placeholder pass
