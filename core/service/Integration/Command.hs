-- | # Integration.Command
--
-- Built-in integration action for emitting commands to other services.
-- This is the foundation of the Process Manager pattern - coordinating
-- workflows across aggregate boundaries.
--
-- == Usage
--
-- @
-- import Integration qualified
-- import Integration.Command qualified as Command
--
-- cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
-- cartIntegrations cart event = case event of
--   ItemAdded {stockId, quantity} -> Integration.batch
--     [ Integration.outbound Command.Emit
--         { command = ReserveStock
--             { stockId = stockId
--             , quantity = quantity
--             , cartId = cart.cartId
--             }
--         }
--     ]
--   _ -> Integration.none
-- @
module Integration.Command (
  Emit (..),
) where

import Basics
import Integration qualified
import Json qualified
import TypeName qualified


-- | Config record for emitting a command as an integration action.
--
-- This is the simplest integration action - it just dispatches a command
-- to another service without any external HTTP calls or side effects.
--
-- @
-- Integration.outbound Command.Emit
--   { command = ReserveStock { stockId, quantity, cartId }
--   }
-- @
data Emit command = Emit
  { command :: command
  }
  deriving (Generic)


instance
  (Json.ToJSON command, TypeName.Inspectable command) =>
  Integration.ToAction (Emit command)
  where
  toAction config = Integration.action do
    Integration.emitCommand config.command
