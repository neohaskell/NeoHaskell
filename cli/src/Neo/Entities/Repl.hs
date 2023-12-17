module Neo.Entities.Repl (
  ) where

import Core


data DefineEntity model event = DefineEntity
  { initialState :: model,
    updateFunction :: event -> model -> model,
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

update :: Event -> Model -> Model
update event model = case event of
  InputChanged text -> do
    let
      currentCommand = text
    model{currentCommand}
  InputSubmitted -> do
    let
      history = model.history + [HistoryResult (model.currentCommand) ""]
    model{currentCommand = "", history}


------ ACT ---------------------------------------------------------------

act :: Event -> Promise Void
act event = case event of
  InputChanged text -> do
    print text
  InputSubmitted -> do
    print "Submitted"