module Service.RuntimeState (
  RuntimeState,
  Reference,
  get,
  set,
  modify,
  registerActionHandler,
  registerDefaultActionHandlers,
) where

import Action (Action)
import Action qualified
import Basics
import Channel (Channel)
import Command qualified
import Console (print)
import File qualified
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (ToText, toText)
import Unknown qualified
import Var (Var)
import Var qualified


type RuntimeState (event :: Type) =
  Record
    '[ "actionHandlers" := Action.HandlerRegistry,
       "actionsQueue" := Channel (Action event),
       "shouldExit" := Bool
     ]


type Reference event = Var (Maybe (RuntimeState event))


get ::
  forall (event :: Type).
  Reference event ->
  IO (RuntimeState event)
get runtimeState = do
  maybeService <- Var.get runtimeState
  case maybeService of
    Nothing -> dieWith "Service is not initialized"
    Just service -> pure service


set ::
  forall (event :: Type).
  RuntimeState event ->
  Reference event ->
  IO ()
set value runtimeState = do
  runtimeState |> Var.set (Just value)


modify ::
  forall (event :: Type).
  (RuntimeState event -> RuntimeState event) ->
  Reference event ->
  IO ()
modify f runtimeState = do
  currentState <- get runtimeState
  let newState = f currentState
  runtimeState |> set newState


registerActionHandler ::
  forall payload event.
  ( Unknown.Convertible payload,
    Unknown.Convertible event,
    ToText payload,
    ToText event
  ) =>
  Text ->
  (payload -> IO event) ->
  Reference event ->
  IO ()
registerActionHandler actionHandlerName handler runtimeState = do
  print "Getting state"
  service <- runtimeState |> get
  print [fmt|Got state: {toText service}|]
  let actionHandler payload =
        case (Unknown.toValue payload) of
          Nothing -> do
            print "Payload is Nothing"
            pure Nothing
          Just pl -> do
            print [fmt|Payload was Just {toText pl}|]
            event <- handler pl
            pure (Unknown.fromValue (event :: event) |> Just)
  let newRegistry =
        service.actionHandlers
          |> Map.set actionHandlerName actionHandler
  let newService = service {actionHandlers = newRegistry}
  print "Setting state"
  runtimeState
    |> set newService


-- TODO: Action Handlers should come in the user app record as a map of
-- action names to handlers. This way, the user app can register its own
-- action handlers. Ideally also the user could omit the action handlers
-- and the service would still work with the default action handlers.
registerDefaultActionHandlers ::
  forall (event :: Type).
  ( Unknown.Convertible event,
    ToText event
  ) =>
  Reference event ->
  IO ()
registerDefaultActionHandlers runtimeState = do
  runtimeState
    |> registerActionHandler @(File.ReadOptions event) @event "File.readText" (File.readTextHandler @event)

  runtimeState
    |> registerActionHandler @(Command.CommandOptions event) @event "Command.parse" (Command.parseHandler @event)

  runtimeState
    |> registerActionHandler "continueWith" (Action.continueWithHandler @event)
