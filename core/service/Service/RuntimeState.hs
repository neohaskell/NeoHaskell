module Service.RuntimeState (
  RuntimeState (..),
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
import Directory qualified
import File qualified
import Http qualified
import IO (IO)
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (Show (..), ToText, toText)
import Unknown (Unknown)
import Unknown qualified
import Var (Var)
import Var qualified


data RuntimeState (event :: Type) = RuntimeState
  { actionHandlers :: Action.HandlerRegistry,
    actionsQueue :: Channel (Action event),
    shouldExit :: Bool
  }
  deriving (Show)


type Reference (event :: Type) = Var (Maybe (RuntimeState event))


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
  forall (payload :: Type) (event :: Type).
  ( Unknown.Convertible payload,
    Unknown.Convertible event,
    Show payload
  ) =>
  Text ->
  (payload -> IO Unknown) ->
  Reference event ->
  IO ()
registerActionHandler actionHandlerName handler runtimeState = do
  print "Getting state"
  service <- runtimeState |> get
  print [fmt|Got state: {toText service}|]
  let actionHandler payload = do
        print [fmt|Handling action {actionHandlerName} with payload {toText payload}|]
        case (Unknown.toValue payload) of
          Nothing -> do
            print "Payload is Nothing"
            pure Nothing
          Just pl -> do
            print [fmt|Payload was Just {toText pl}|]
            result <- handler pl
            pure (Just result)
  -- let actionHandlerName = Unknown.getTypeName @(payload -> IO event)
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
  (Unknown.Convertible event, ToText event) =>
  Reference event ->
  IO ()
registerDefaultActionHandlers runtimeState = do
  let mkHandler :: forall (p :: Type) (e :: Type). (Unknown.Convertible e) => (p -> IO e) -> p -> IO Unknown
      mkHandler h input = do
        res <- h input
        pure (Unknown.fromValue res)

  runtimeState
    |> registerActionHandler "File.readText" (mkHandler File.readTextHandler)

  runtimeState
    |> registerActionHandler "Directory.create" (mkHandler Directory.createHandler)

  runtimeState
    |> registerActionHandler "Command.parse" (mkHandler @(Command.CommandOptions event) @event Command.parseHandler)

  runtimeState
    |> registerActionHandler "continueWith" (mkHandler @event @event Action.continueWithHandler)

  runtimeState
    |> registerActionHandler "Http.get" (mkHandler @(Http.Request event) @event Http.getActionHandler)
