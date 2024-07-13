module Platform
  ( Command,
    CommandHandler,
    CommandHandlerRegistry,
    globalState,
  )
where

import Core
import IO qualified
import Map qualified
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

--- implementation details ---

To implement the above, we will use the Dynamic type as the type of the contents of the Command queue
as it allows to decode conditionally depending on the type of the command handler.

-}

type Command msg =
  [ "type" := Text,
    "payload" := Unknown
  ]

type CommandHandler msg = Unknown -> IO msg

type CommandHandlerRegistry msg = Map Text (CommandHandler msg)

-- The platform loop should need to initialize the registry as an ioref or smth

type Platform msg =
  Record
    '[ "commandHandlers" := CommandHandlerRegistry msg
     ]

-- TODO: This probably should be a concurrent var
runtimeState :: Var (Platform msg)
{-# NOINLINE runtimeState #-}
runtimeState = IO.dangerouslyRun do
  let commandHandlers = Map.empty
  let platform = ANON {commandHandlers = commandHandlers}
  (Var.new platform)
