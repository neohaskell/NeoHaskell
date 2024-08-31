module Service.RuntimeState (
  RuntimeState,
  Reference,
  get,
  set,
  modify,
) where

import Action (Action)
import Action qualified
import Basics
import Channel (Channel)
import Maybe (Maybe (..))
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