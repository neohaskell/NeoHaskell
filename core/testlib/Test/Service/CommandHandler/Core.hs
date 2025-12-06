module Test.Service.CommandHandler.Core (
  -- Re-export command test helpers
  module Test.Service.Command.Core,
  -- Re-export CommandHandler types from the real implementation
  module Service.CommandHandler.Core,
) where

import Service.CommandHandler.Core
import Test.Service.Command.Core
