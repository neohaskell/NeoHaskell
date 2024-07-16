{-# LANGUAGE AllowAmbiguousTypes #-}

module Platform (
  runtimeState,
  init,
  Platform,
  registerCommandHandler,
) where

import Basics
import Command qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Data.Kind (Type)
import File qualified
import IO qualified
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import Unknown qualified


type Platform =
  Record
    '[ "commandHandlers" := Command.HandlerRegistry
     ]


runtimeState :: ConcurrentVar Platform
{-# NOINLINE runtimeState #-}
runtimeState = IO.dangerouslyRun ConcurrentVar.new


init :: forall (msg :: Type). (Unknown.Convertible msg) => IO ()
init = do
  ConcurrentVar.set uninitializedPlatform runtimeState
  registerCommandHandler "File.readText" (File.readTextHandler @msg)
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

uninitializedPlatform :: Platform
uninitializedPlatform = ANON {commandHandlers = Command.emptyRegistry}
