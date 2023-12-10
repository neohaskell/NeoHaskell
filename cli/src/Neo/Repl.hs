{-# LANGUAGE OverloadedLabels #-}

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
  { history :: Array String
  , currentCommand :: String
  }


update :: Event -> State -> State
update _ _ = todo


-- COMMANDS

data Command
  = ReadLine String
  | SubmitInput


handleCommand :: services -> Command -> Promise Void
handleCommand _ _ = todo


-- Ui

data Ui
  = Repl


data View viewFormat message


data Cli


replUi :: Promise (View Cli Message)
replUi = do
  state <- getState

  resolveView
    [ input
        [ onChange (perform ReadLine)
        , onSubmit (perform SubmitInput)
        ]
        [text "> "]
    , text state.currentCommand
    ]
