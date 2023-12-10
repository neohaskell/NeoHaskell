{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Neo.Repl (
  Command (..),
  Event (..),
  State (..),
  update,
  handleCommand,
) where

import Array qualified
import Core


data Agent name command event state query


type ReplAgent = Agent "repl" Command Event State Query


-- MESSAGES

data Message
  = EventMessage Event
  | CommandMessage Command
  | ViewMessage Query


-- EVENTS

data Event
  = InputChanged String
  | InputSubmitted


-- STATE

data State = State
  { history :: Array HistoryResult
  , currentCommand :: String
  }


data HistoryResult = HistoryResult
  { command :: String
  , output :: String
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

data Query
  = Repl


data View viewFormat message


data Cli


replUi :: View Cli Event
replUi = do
  state <- getState "repl"

  let
    changeHandler =
      get "value" @String
        |> putInto InputChanged
        |> registerEvent

  let
    submitHandler = do
      registerEvent InputSubmitted

  let
    historyUi =
      state.history
        |> Array.applyToEach historyResultUi

  cliView do
    div [] do
      historyUi

    input [onChange changeHandler, onSubmit submitHandler] do
      text ("> " + state.currentCommand)


historyResultUi :: HistoryResult -> View Cli Event
historyResultUi historyResult =
  div [] do
    text ("> " + historyResult.command)
    text historyResult.output