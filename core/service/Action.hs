module Action (
  Action (..),
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
  ActionResult (..),
) where

import Array (Array)
import Array qualified
import Basics
import Console (print)
import IO (IO)
import IO qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (Show (..), ToText, toText)
import Unknown (Unknown)
import Unknown qualified
import Var (Var)
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
newtype Action event = Action (Array ActionRecord)
  deriving (Show)


data ActionRecord = ActionRecord
  { name :: ActionName,
    payload :: Unknown
  }
  deriving (Show)


data ActionName
  = Custom Text
  | MapAction
  deriving (Show)


type Handler = Unknown -> IO (Maybe Unknown)


type HandlerRegistry = Map Text Handler


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
    |> Array.push (ActionRecord {name = MapAction, payload = Unknown.fromValue f})
    |> Action


data ActionResult
  = Continue (Maybe Unknown)
  | Error Text


processBatch ::
  HandlerRegistry ->
  Action anyEvent ->
  IO (ActionResult)
processBatch registry (Action actionBatch) = do
  print "[processBatch] Processing batch"
  currentOutput <- Var.new Nothing

  print [fmt|[processBatch] Starting action loop with {Array.length actionBatch} actions|]
  result <- actionBatch |> Array.foldM (processAction currentOutput) (Continue Nothing)

  case result of
    Continue _ -> do
      print "[processBatch] Getting final output"
      out <- Var.get currentOutput
      pure (Continue out) -- Output is still Unknown
    Error msg -> pure (Error msg)
 where
  processAction :: Var (Maybe Unknown) -> ActionResult -> ActionRecord -> IO (ActionResult)
  processAction _ (Error msg) _ = pure (Error msg)
  processAction currentOutput (Continue _) action = do
    if shouldExit action.name
      then IO.exitSuccess
      else case action.name of
        Custom name' -> handleCustomAction name' action.payload currentOutput
        MapAction -> handleMapAction action.payload currentOutput

  handleCustomAction :: Text -> Unknown -> Var (Maybe Unknown) -> IO (ActionResult)
  handleCustomAction name' payload currentOutput = do
    case Map.get name' registry of
      Just handler -> do
        print [fmt|Found handler for {name'}, calling with {toText payload}|]
        result <- handler payload
        print [fmt|Handler {name'} returned: {toText result}|]
        case result of
          Nothing -> do
            print [fmt|Handler {name'} returned nothing|]
            pure (Continue Nothing)
          Just result' -> do
            print [fmt|Setting output to {toText result'}|]
            currentOutput |> Var.set (Just result')
            pure (Continue (Just result'))
      Nothing -> pure (Error [fmt|Action handler not found for: {name'}|])

  handleMapAction :: Unknown -> Var (Maybe Unknown) -> IO (ActionResult)
  handleMapAction payload currentOutput = do
    print "[processBatch] Map action"
    maybeOut <- Var.get currentOutput
    case maybeOut of
      Nothing -> pure (Error "[processBatch] No output to map")
      Just out -> do
        print "[processBatch] Applying mapping function"
        -- Apply the mapping function directly to the Unknown values
        case Unknown.apply payload out of
          Nothing -> do
            print "[processBatch] Couldn't apply mapping function"
            pure (Error "Couldn't apply mapping function")
          Just output -> do
            print "[processBatch] Setting output"
            currentOutput |> Var.set (Just output)
            pure (Continue (Just output))

  shouldExit :: ActionName -> Bool
  shouldExit actionName =
    case actionName of
      Custom "exit" -> True
      _ -> False


named ::
  (Unknown.Convertible value) =>
  Text ->
  value ->
  Action result
named name value =
  Array.fromLinkedList [(ActionRecord {name = Custom name, payload = Unknown.fromValue value})]
    |> Action


continueWith ::
  (Unknown.Convertible event) =>
  event ->
  Action event
continueWith event =
  named "continueWith" event


continueWithHandler ::
  forall event.
  (Unknown.Convertible event, ToText event) =>
  event ->
  IO event
continueWithHandler event = do
  print [fmt|Continuing with {toText event}|]
  pure event