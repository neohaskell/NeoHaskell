module Integration.Exit (
  onEvent,
) where

import Integration qualified
import System.Exit qualified as GhcExit
import Task qualified


-- | Create an outbound integration that exits the process when any event fires.
--
-- Use this as the 'handleEvent' function in an OutboundIntegration handler.
onEvent :: entity -> event -> Integration.Outbound
onEvent _entity _event =
  Integration.batch
    [ Integration.outbound ExitAction
    ]


data ExitAction = ExitAction


instance Integration.ToAction ExitAction where
  toAction _ = Integration.action \_ctx -> do
    _ <- Task.fromIO GhcExit.exitSuccess
    Integration.noCommand
