module Neo.Entities.Repl (
  entity,
) where

import Core
import Result qualified


data DefineEntity model event = DefineEntity
  { initialState :: model,
    updateFunction :: event -> model -> Result model String,
    actFunction :: event -> Promise Void
  }


entity :: DefineEntity Model Event
entity =
  DefineEntity
    { initialState = defaultValue,
      updateFunction = update,
      actFunction = act
    }


------ MODEL ---------------------------------------------------------------

data Model = Model
  { history :: Array HistoryResult,
    currentCommand :: String
  }


data HistoryResult = HistoryResult
  { command :: String,
    output :: String
  }


instance Default Model where
  defaultValue =
    Model
      { history = [],
        currentCommand = ""
      }


------ EVENTS --------------------------------------------------------------

data Event
  = InputChanged String
  | InputSubmitted


------ UPDATE --------------------------------------------------------------

update :: Event -> Model -> Result Model String
update event model = case event of
  InputChanged text -> do
    let currentCommand = text
    Result.Ok model {currentCommand}
  InputSubmitted -> do
    let newHistoryResult =
          HistoryResult
            { command = model.currentCommand,
              output = ""
            }
    let history = model.history + [newHistoryResult]
    let currentCommand = ""
    Result.Ok model {currentCommand, history}


------ ACT ---------------------------------------------------------------

act :: Event -> Promise Void
act event = case event of
  InputChanged text -> do
    print text
  InputSubmitted -> do
    print "Submitted"