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
import IO qualified
import Map qualified
import Text (Text)


-- The platform loop should need to initialize the registry as an ioref or smth

type Platform msg =
  Record
    '[ "commandHandlers" := Command.HandlerRegistry msg
     ]


-- TODO: This probably should be a concurrent var
runtimeState :: ConcurrentVar (Platform msg)
{-# NOINLINE runtimeState #-}
runtimeState = IO.dangerouslyRun ConcurrentVar.new


init :: IO ()
init = do
  ConcurrentVar.set uninitializedPlatform runtimeState
  pure ()


registerCommandHandler :: Text -> Command.Handler msg -> IO ()
registerCommandHandler commandHandlerName handler = do
  platform <- ConcurrentVar.get runtimeState
  let newRegistry =
        platform.commandHandlers
          |> Map.set commandHandlerName handler
  let newPlatform = platform {commandHandlers = newRegistry}
  runtimeState
    |> ConcurrentVar.set newPlatform


-- PRIVATE

uninitializedPlatform :: Platform msg
uninitializedPlatform = ANON {commandHandlers = Command.emptyRegistry}
