module Neo.Repl (
  Command (..),
  Event (..),
  State (..),
  update,
  handleCommand,
) where

import Core


-- MESSAGES

data Message
  = EventMessage Event
  | CommandMessage Command
  | ViewMessage Ui


-- EVENTS

data Event
  = InputChanged String


-- STATE

data State = State
  { history :: Array String,
    currentCommand :: String
  }


update :: Event -> State -> State
update _ _ = todo


-- COMMANDS

data Command
  = ReadLine String


handleCommand :: services -> Command -> Promise Void
handleCommand _ _ = todo


-- Ui

data Ui
  = Repl


-- replUi :: Promise (View Cli Message)
replUi = do
  state <- getState

  view
    [ input
        [onChange (submit ReadLine)]
        [ text ""
        ]
    ]