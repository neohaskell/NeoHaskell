module Neo.Repl (
  Command (..),
  Event (..),
  State (..),
  update,
  handleCommand,
) where

import Core


-- EVENTS

data Event
  = CodeSent String
  | ReplCommandTriggered String
  | NaturalLanguageSent String


-- STATE

data State = State
  {
  }


update :: Event -> State -> State
update event state = todo


-- COMMANDS

data Command
  = ReadLine String


handleCommand :: services -> Command -> Promise Void
handleCommand services command = todo
