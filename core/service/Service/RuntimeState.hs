module Service.RuntimeState (
  RuntimeState (..),
  Reference,
  get,
  set,
  modify,
  registerActionHandler,
  registerDefaultActionHandlers,
) where

import Action qualified
import Basics
import Channel (Channel)
import Command qualified
import Console (print)
import File qualified
import Http qualified
import IO (IO)
import Map qualified
import Maybe (Maybe (..))
import ToText (Show (..), toText)
import Unknown qualified
import Var (Var)
import Var qualified


data RuntimeState = RuntimeState
  { actionHandlers :: Action.HandlerRegistry,
    actionsQueue :: Channel Action.Untyped,
    shouldExit :: Bool
  }
  deriving (Show)


type Reference = Var (Maybe (RuntimeState))


get ::
  Reference ->
  IO (RuntimeState)
get runtimeState = do
  maybeService <- Var.get runtimeState
  case maybeService of
    Nothing -> dieWith "Service is not initialized"
    Just service -> pure service


set ::
  RuntimeState ->
  Reference ->
  IO ()
set value runtimeState = do
  runtimeState |> Var.set (Just value)


modify ::
  (RuntimeState -> RuntimeState) ->
  Reference ->
  IO ()
modify f runtimeState = do
  currentState <- get runtimeState
  let newState = f currentState
  runtimeState |> set newState


registerActionHandler ::
  forall (payload :: Type) (event :: Type).
  ( Unknown.Convertible payload,
    Unknown.Convertible event,
    Show payload
  ) =>
  (payload -> IO event) ->
  Reference ->
  IO ()
registerActionHandler handler runtimeState = do
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
  let actionHandlerName = Unknown.getTypeName @(payload -> IO event)
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
registerDefaultActionHandlers :: Reference -> IO ()
registerDefaultActionHandlers runtimeState = do
  runtimeState
    |> registerActionHandler File.readTextHandler

  runtimeState
    |> registerActionHandler Command.parseHandler

  runtimeState
    |> registerActionHandler Action.continueWithHandler

  runtimeState
    |> registerActionHandler Http.getActionHandler
