module Action (
  Action,
  Handler,
  HandlerRegistry,
  emptyRegistry,
  none,
  batch,
  map,
  named,
  processBatch,
  continueWith,
  continueWithHandler,
) where

import Appendable ((++))
import Array (Array)
import Array qualified
import Basics
import Console (print)
import Map (Map)
import Map qualified
import Maybe (Maybe (..), withDefault)
import Text (Text)
import ToText (Show (..), toText)
import Unknown (Unknown)
import Unknown qualified
import Var qualified


{-
Actions (Action) are essentially a callback that is called after some side effect
happens. First, the platform, or the user, will register action handlers that will
act as effectful functions that will be triggered when the action is submitted
into the Action queue (this is hidden from the user, and handled by the platform),
then, when the user returns a action of this kind either in the init function or
the update function, the platform will submit the action to the queue, and the
handler will be called with the action as an argument. Usually the action constructors
are basically functions that configure everything to be ready to be executed.

For example, one might want to read a file right when the app starts, so one would do
this in the init function by using a hypothetical FileSystem.readFile action, configured
with the path to the file. If the user desires to handle the file contents in a certain
event, they would have to use Action.map to transform the file contents into a different
action that will be called when the event happens.

Example:

type Model = [
    "someText" := Text
    ]

data Event
    = AppStarted String

init :: (Model, Action Event)
init = (
    emptyModel,
    FileSystem.readFile "someFile.txt"
      |> Action.map AppStarted
    )

The advantage of defining side effects through the usage of actions is that it
allows the side effects to be tracked in a queue, which can be inspected in many different
ways, and also test them in a controlled environment.

This allows for even more interesting features, like allowing to replay the actions without
actually executing them, or even to serialize them and send them to another platform to be
executed there. This is the basis for the time-travel debugging feature that Elm has, and
a great inspiration for the NeoHaskell platform.
-}
newtype Action event
  = Action
      ( Array
          ( Record
              '[ "name" := ActionName,
                 "payload" := Unknown
               ]
          )
      )
  deriving (Show)


data ActionName
  = Custom Text
  | MapAction
  deriving (Show)


type Handler = Unknown -> IO (Maybe Unknown)


instance Show Handler where
  show _ = "Handler"


type HandlerRegistry = Map Text Handler


instance Show HandlerRegistry where
  show _ = "HandlerRegistry"


emptyRegistry :: Action.HandlerRegistry
emptyRegistry = Map.empty


none :: Action event
none = Action (Array.empty)


batch :: Array (Action event) -> Action event
batch actions =
  actions
    |> Array.flatMap (\(Action action) -> action)
    |> Action


map :: (Unknown.Convertible a, Unknown.Convertible b) => (a -> b) -> Action a -> Action b
map f (Action actions) =
  actions
    |> Array.push (ANON {name = MapAction, payload = Unknown.fromValue f})
    |> Action


processBatch ::
  forall (value :: Type).
  (Unknown.Convertible value) =>
  HandlerRegistry ->
  Action value ->
  IO (Maybe value)
processBatch registry (Action actionBatch) = do
  print "[processBatch] Processing batch"
  print "[processBatch] Creating output var"
  currentOutput <- Var.new Nothing

  -- TODO: Refactor this
  print ("[processBatch] Starting action loop with " ++ toText (Array.length actionBatch) ++ " actions")
  actionBatch |> Array.forEach \action -> do
    print ("[processBatch] Matching action " ++ toText action)
    case action.name of
      Custom name' -> do
        print ("[processBatch] Custom action " ++ name')
        case Map.get name' registry of
          Just handler -> do
            print "[processBatch] Handler found, executing"
            result <- handler action.payload
            case result of
              Nothing -> do
                print "[processBatch] Handler returned Nothing"
                pure ()
              Just result' -> do
                print "[processBatch] Handler returned Just, setting output"
                currentOutput |> Var.set (Just result')
          Nothing -> do
            print "[processBatch] Action handler not found"
            pure ()
      MapAction -> do
        print "[processBatch] Map action"
        maybeOut <- Var.get currentOutput

        print "[processBatch] Getting output"
        let out = maybeOut |> Maybe.withDefault (dieWith "[processBatch] No output")

        print "[processBatch] Applying mapping function"
        let result = Unknown.apply action.payload out

        case result of
          Nothing -> do
            print "[processBatch] Couldn't apply mapping function"
            pure ()
          Just output -> do
            print "[processBatch] Setting output"
            currentOutput |> Var.set (Just output)

  print "[processBatch] Getting final output"
  out <- Var.get currentOutput
  case out of
    Nothing -> do
      pure Nothing
    Just out' -> case Unknown.toValue out' of
      Nothing -> dieWith "Couldn't convert output"
      Just out'' -> pure (Just out'')


-- results <-
--   batch
--     |> Array.mapM
--       ( \(ANON {name, payload}) ->
--           case name of
--             Custom name' ->
--               case Map.get name' registry of
--                 Just handler -> handler payload
--                 Nothing -> pure Nothing
--             MapAction -> pure (Just payload)
--       )
-- results
--   |> Array.mapMaybe identity

named ::
  (Unknown.Convertible value, Unknown.Convertible result) =>
  Text ->
  value ->
  Action result
named name value =
  Array.fromLinkedList [(ANON {name = Custom name, payload = Unknown.fromValue value})]
    |> Action


continueWith ::
  (Unknown.Convertible event) =>
  event ->
  Action event
continueWith event =
  named "continueWith" event


continueWithHandler ::
  forall event.
  (Unknown.Convertible event) =>
  event ->
  IO event
continueWithHandler event =
  pure event