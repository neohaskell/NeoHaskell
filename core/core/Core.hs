-- | Language vocabulary and application configuration for NeoHaskell projects.
module Core (
  derive,
  module Authoring,
  module Core.Prelude,
) where

import Core.Prelude
import Derive (derive)
import Service.EventModel.Authoring as Authoring
