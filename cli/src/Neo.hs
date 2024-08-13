module Neo (main) where

import Action qualified
import Core


type State =
  Record '[]


data Event
  = Transpile TranspileEvent
  | NoOp
  deriving (Show, Eq, Ord)


type TranspileEvent =
  Record
    '[ "inputPath" := Path,
       "outputPath" := Path
     ]


init :: Record '["next" := State, "action" := Action Event]
init = do
  let emptyState = ANON {}
  let action = Action.none
  ANON {next = emptyState, action = action}


update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    Transpile transpileEvent -> do
      let newState = state
      let action = Action.none
      (newState, action)
    NoOp -> do
      let newState = state
      let action = Action.none
      (newState, action)
