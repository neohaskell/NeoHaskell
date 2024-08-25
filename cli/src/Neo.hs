module Neo (main) where

import Action qualified
import Array (Array)
import Array qualified
import Command qualified
import Core
import Path qualified
import Service qualified
import ToText (Show (..))


type State =
  Record
    '[ "foo" := Text,
       "bar" := Text
     ]


data Event
  = Transpile TranspilationStartedEvent
  | NoOp
  deriving (Show, Eq, Ord)


type TranspilationStartedEvent =
  Record
    '[ "inputPath" := Path,
       "outputPath" := Path
     ]


commandParser :: Command.OptionsParser Event
commandParser = do
  let transpile =
        ANON
          { name = "transpile",
            description = "Transpile a file or directory",
            version = Nothing,
            decoder = transpileParser
          }
  Command.commands
    (Array.fromLinkedList [transpile])


transpileParser :: Command.OptionsParser Event
transpileParser = do
  event <- transpilationParser
  pure (Transpile event)


transpilationParser :: Command.OptionsParser TranspilationStartedEvent
transpilationParser = do
  inputPath <-
    Command.path
      ANON
        { help = "Path to the input file or directory",
          long = "input",
          short = 'i',
          metavar = "PATH"
        }

  outputPath <-
    Command.path
      ANON
        { help = "Path to the output file or directory",
          long = "output",
          short = 'o',
          metavar = "PATH"
        }

  pure ANON {inputPath = inputPath, outputPath = outputPath}


init :: (State, Action Event)
init = do
  let emptyState = ANON {foo = "foo", bar = "bar"}
  let action =
        Command.parse
          ANON
            { name = "neo",
              description = "NeoHaskell's console helper",
              version = Just [version|0.0.0|],
              decoder = commandParser
            }
  (emptyState, action)


update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    Transpile transpilationStartedEvent -> do
      let newState = state {foo = "Transpilation started", bar = transpilationStartedEvent.inputPath |> Path.toText}
      let action = Action.none
      (newState, action)
    NoOp -> do
      let newState = state
      let action = Action.none
      (newState, action)


view :: State -> Text
view _ = "Hello, world!"


triggers :: Array (Trigger Event)
triggers = Array.empty


main :: IO ()
main = do
  let app :: Service.UserApp State Event
      app =
        ANON {init = init, view = view, triggers = triggers, update = update}
  Service.init app