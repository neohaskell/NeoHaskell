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
import qualified Action
import Basics
import Channel (Channel)
import qualified Command
import Console (log)
import qualified Directory
import qualified File
import qualified Http
import IO (IO)
import qualified Map
import Maybe (Maybe (..))
import qualified Subprocess
import Text (Text)
import ToText (Show (..), ToText, toPrettyText)
import Unknown (Unknown)
import qualified Unknown
import Var (Var)
import qualified Var


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
    Nothing -> panic "Service is not initialized"
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
  log "Getting state"
  service <- runtimeState |> get
  log [fmt|Got state: {toPrettyText service}|]
  let actionHandler payload = do
        log [fmt|Handling action {actionHandlerName} with payload {toPrettyText payload}|]
        case (Unknown.toValue payload) of
          Nothing -> do
            log "Payload is Nothing"
            pure Nothing
          Just pl -> do
            log [fmt|Payload was Just {toPrettyText pl}|]
            result <- handler pl
            pure (Just result)
  -- let actionHandlerName = Unknown.getTypeName @(payload -> IO event)
  let newRegistry =
        service
          . actionHandlers
          |> Map.set actionHandlerName actionHandler
  let newService = service {actionHandlers = newRegistry}
  log "Setting state"
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

  runtimeState
    |> registerActionHandler "Subprocess.open" (mkHandler Subprocess.openHandler)
