{-# LANGUAGE AllowAmbiguousTypes #-}

module Platform (
  runtimeState,
  init,
  Platform,
  registerCommandHandler,
) where

import Basics
import Channel (Channel)
import Channel qualified
import Command (Command)
import Command qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Control.Monad qualified as Monad
import File qualified
import IO qualified
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import Unknown qualified
import Var qualified


data View


type Platform (msg :: Type) =
  Record
    '[ "commandHandlers" := Command.HandlerRegistry,
       "commandsQueue" := Channel (Command msg)
     ]


type UserApp (model :: Type) (msg :: Type) =
  Record
    '[ "init" := (model, Command msg),
       "view" := (model -> View),
       "update" := (msg -> model -> (model, Command msg))
     ]


runtimeState :: forall (msg :: Type). ConcurrentVar (Platform msg)
{-# NOINLINE runtimeState #-}
runtimeState = IO.dangerouslyRun ConcurrentVar.new


init ::
  forall (model :: Type) (msg :: Type).
  (Unknown.Convertible msg) =>
  UserApp model msg ->
  IO ()
init userApp = do
  initializePlatform userApp
  pure ()


registerCommandHandler ::
  forall payload result.
  (Unknown.Convertible payload, Unknown.Convertible result) =>
  Text ->
  (payload -> IO result) ->
  IO ()
registerCommandHandler commandHandlerName handler = do
  platform <- ConcurrentVar.get runtimeState
  let commandHandler payload =
        case (Unknown.toValue payload) of
          Nothing -> pure Nothing
          Just pl -> do
            result <- handler pl
            pure (Unknown.fromValue (result :: result) |> Just)
  let newRegistry =
        platform.commandHandlers
          |> Map.set commandHandlerName commandHandler
  let newPlatform = platform {commandHandlers = newRegistry}
  runtimeState
    |> ConcurrentVar.set newPlatform


-- PRIVATE

initializePlatform ::
  forall (model :: Type) (msg :: Type).
  (Unknown.Convertible msg) =>
  UserApp model msg ->
  IO ()
initializePlatform userApp = do
  commandsQueue <- Channel.new
  let initialPlatform =
        ANON
          { commandHandlers = Command.emptyRegistry,
            commandsQueue = commandsQueue
          }
  ConcurrentVar.set initialPlatform runtimeState
  registerDefaultCommandHandlers @msg
  let (initModel, initCmd) = userApp.init
  let update = userApp.update
  modelRef <- Var.new initModel
  commandsQueue |> Channel.write initCmd
  Monad.forever do
    model <- Var.get modelRef
    nextCommandBatch <- Channel.read commandsQueue
    state <- ConcurrentVar.get runtimeState
    processed <- Command.processBatch state.commandHandlers nextCommandBatch
    let (newModel, newCmd) = update processed model
    modelRef |> Var.set newModel
    commandsQueue |> Channel.write newCmd
    pure ()


-- TODO: Command Handlers should come in the user app record as a map of
-- command names to handlers. This way, the user app can register its own
-- command handlers. Ideally also the user could omit the command handlers
-- and the platform would still work with the default command handlers.
registerDefaultCommandHandlers :: forall (msg :: Type). (Unknown.Convertible msg) => IO ()
registerDefaultCommandHandlers = do
  registerCommandHandler @(File.ReadOptions msg) @msg "File.readText" (File.readTextHandler @msg)
