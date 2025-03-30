module Service.RenderWorker (
  run,
) where

import qualified AsyncIO
import Basics
import qualified Brick
import Brick.BChan (BChan)
import qualified Brick.BChan
import ConcurrentVar (ConcurrentVar)
import qualified ConcurrentVar
import Console (log)
import qualified Graphics.Vty as Vty
import IO (IO)
import qualified IO
import Maybe (Maybe (..))
import Service.Core
import qualified Service.RuntimeState as RuntimeState
import ToText (Show)


data RenderTag = RenderTag
  deriving (Show, Ord, Eq)


data ServiceEvent (model :: Type)
  = ModelUpdated model
  | ExitRender
  deriving (Show, Ord, Eq)


run ::
  forall (model :: Type) (event :: Type).
  UserApp model event ->
  ConcurrentVar model ->
  RuntimeState.Reference event ->
  IO ()
run userApp modelRef runtimeState = do
  -- We wait a little bit to give the other workers a chance to start
  -- before we start rendering the view. This is also useful for when
  -- the action worker might exit the program on first execution.
  -- E.g. when the settings parser fails to parse the command line
  -- arguments.
  AsyncIO.sleep 500
  log "Getting state"
  state <- RuntimeState.get runtimeState
  if state . shouldExit
    then do
      log "Exiting"
      IO.exitSuccess
    else do
      log "Getting model"
      model <- ConcurrentVar.peek modelRef

      log "Setting up event channel"
      eventChannel <- Brick.BChan.newBChan 1

      let handleEvent event = case event of
            Brick.AppEvent (ModelUpdated newModel) -> do
              Brick.put newModel
            Brick.AppEvent ExitRender -> do
              Brick.halt
            Brick.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> do
              Brick.halt
            _ -> Brick.continueWithoutRedraw

      let brickApp =
            Brick.App
              { Brick.appDraw = \s -> [userApp . view s |> Brick.txt @RenderTag],
                Brick.appChooseCursor = \_ _ -> Nothing,
                Brick.appHandleEvent = handleEvent,
                Brick.appStartEvent = pure (),
                Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr []
              }

      log "Running render model worker"
      renderModelWorker @model modelRef eventChannel runtimeState
        |> AsyncIO.run
        |> discard

      log "Rendering view"
      (_, vty) <-
        Brick.customMainWithDefaultVty
          (Just eventChannel)
          brickApp
          model
      Vty.shutdown vty
      log "Done rendering"


renderModelWorker ::
  forall (model :: Type) (event :: Type).
  ConcurrentVar model ->
  BChan (ServiceEvent model) ->
  RuntimeState.Reference event ->
  IO ()
renderModelWorker modelRef eventChannel runtimeState =
  forever do
    state <- RuntimeState.get runtimeState
    if state . shouldExit
      then do
        log "[renderModelWorker] Exiting"
        Brick.BChan.writeBChan eventChannel ExitRender
      else do
        -- log "[renderModelWorker] Peeking model"
        model <- ConcurrentVar.peek modelRef
        -- log "[renderModelWorker] Sending model update event"
        Brick.BChan.writeBChan eventChannel (ModelUpdated model)
