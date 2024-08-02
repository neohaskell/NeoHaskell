module Command
  ( Command,
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
  )
where

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
Commands (Command) are essentially a callback that is called after some side effect
happens. First, the platform, or the user, will register command handlers that will
act as effectful functions that will be triggered when the command is submitted
into the Command queue (this is hidden from the user, and handled by the platform),
then, when the user returns a command of this kind either in the init function or
the update function, the platform will submit the command to the queue, and the
handler will be called with the command as an argument. Usually the command constructors
are basically functions that configure everything to be ready to be executed.

For example, one might want to read a file right when the app starts, so one would do
this in the init function by using a hypothetical FileSystem.readFile command, configured
with the path to the file. If the user desires to handle the file contents in a certain
event, they would have to use Command.map to transform the file contents into a different
command that will be called when the event happens.

Example:

type Model = [
    "someText" := Text
    ]

data Msg
    = AppStarted String

init :: (Model, Command Msg)
init = (
    emptyModel,
    FileSystem.readFile "someFile.txt"
      |> Command.map AppStarted
    )

The advantage of defining side effects through the usage of commands is that it
allows the side effects to be tracked in a queue, which can be inspected in many different
ways, and also test them in a controlled environment.

This allows for even more interesting features, like allowing to replay the commands without
actually executing them, or even to serialize them and send them to another platform to be
executed there. This is the basis for the time-travel debugging feature that Elm has, and
a great inspiration for the NeoHaskell platform.
-}
newtype Command msg
  = Command
      ( Array
          ( Record
              '[ "name" := CommandName,
                 "payload" := Unknown
               ]
          )
      )
  deriving (Show)

data CommandName
  = Custom Text
  | MapCommand
  deriving (Show)

type Handler = Unknown -> IO (Maybe Unknown)

instance Show Handler where
  show _ = "Handler"

type HandlerRegistry = Map Text Handler

instance Show HandlerRegistry where
  show _ = "HandlerRegistry"

emptyRegistry :: Command.HandlerRegistry
emptyRegistry = Map.empty

none :: Command msg
none = Command (Array.empty)

batch :: Array (Command msg) -> Command msg
batch commands =
  commands
    |> Array.flatMap (\(Command command) -> command)
    |> Command

map :: (Unknown.Convertible a, Unknown.Convertible b) => (a -> b) -> Command a -> Command b
map f (Command commands) =
  commands
    |> Array.push (ANON {name = MapCommand, payload = Unknown.fromValue f})
    |> Command

-- FIXME: Rather than applying this complex mapping, we should just setup a trigger for each command and apply the mapping
-- when the command is triggered, handled, and passed through that trigger
processBatch ::
  forall (value :: Type).
  (Unknown.Convertible value) =>
  HandlerRegistry ->
  Command value ->
  IO (Maybe value)
processBatch registry (Command commandBatch) = do
  print "Processing batch"
  print "Creating output var"
  currentOutput <- Var.new Nothing

  print ("Starting command loop with " ++ toText (Array.length commandBatch) ++ " commands")
  commandBatch |> Array.forEach \command -> do
    print ("Matching command " ++ toText command)
    case command.name of
      Custom name' -> do
        print ("Custom command " ++ name')
        case Map.get name' registry of
          Just handler -> do
            print "Handler found, executing"
            result <- handler command.payload
            case result of
              Nothing -> do
                print "Handler returned Nothing"
                pure ()
              Just result' -> do
                print "Handler returned Just, setting output"
                currentOutput |> Var.set (Just result')
          Nothing -> do
            print "Command handler not found"
            pure ()
      MapCommand -> do
        print "Map command"
        maybeOut <- Var.get currentOutput

        print "Getting output"
        let out = maybeOut |> Maybe.withDefault (dieWith "No output")

        print "Applying mapping function"
        let result = Unknown.apply command.payload out

        case result of
          Nothing -> do
            print "Couldn't apply mapping function"
            pure ()
          Just output -> do
            print "Setting output"
            currentOutput |> Var.set (Just output)

  print "Getting final output"
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
--             MapCommand -> pure (Just payload)
--       )
-- results
--   |> Array.mapMaybe identity

named ::
  (Unknown.Convertible value, Unknown.Convertible result) =>
  Text ->
  value ->
  Command result
named name value =
  Array.fromLinkedList [(ANON {name = Custom name, payload = Unknown.fromValue value})]
    |> Command

continueWith ::
  (Unknown.Convertible msg) =>
  msg ->
  Command msg
continueWith msg =
  named "continueWith" msg

continueWithHandler ::
  forall msg.
  (Unknown.Convertible msg) =>
  msg ->
  IO msg
continueWithHandler msg =
  pure msg