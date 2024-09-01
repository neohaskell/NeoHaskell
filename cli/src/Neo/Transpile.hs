module Neo.Transpile () where

import Action qualified
import Array qualified
import Core
import File qualified
import Result qualified
import Service qualified
import Time qualified
import ToText (Show)


type State =
  Record
    '[ "inputPath" := Path,
       "outputPath" := Path,
       "status" := Text
     ]


data Event
  = InputFileRead Text
  | InputFileAccessErrored File.Error
  | TranspilationCompleted Text
  | TranspilationFailed FailureReason
  | OutputFileWritten
  | OutputFileWriteErrored File.Error
  deriving (Show)


data FailureReason
  = TranspilationError Text
  deriving (Show)


init :: (State, Action Event)
init = do
  let emptyState =
        ANON
          { inputPath = Nothing,
            outputPath = Nothing,
            status = "Starting up"
          }
  let action =
        File.readText
          ANON
            { path = [path|inputPath.txt|],
              onSuccess = InputFileRead,
              onError = InputFileAccessErrored
            }
  (emptyState, action)


update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    InputFileRead fileContent -> do
      let newState = state {inputPath = Just fileContent, status = "Transpiling..."}
      let transpiled = transpile fileContent
      (newState, Action.continueWith (TranspilationCompleted transpiled))
    InputFileAccessErrored _ ->
      (state {status = "Input File Access Errored"}, Action.none)
    TranspilationCompleted transpiledContent -> do
      let newState = state {outputPath = Just transpiledContent, status = "Writing output..."}
      let action =
            File.writeText
              ANON
                { path = [path|outputPath.txt|],
                  content = transpiledContent,
                  onSuccess = \_ -> OutputFileWritten,
                  onError = OutputFileWriteErrored
                }
      (newState, action)
    TranspilationFailed reason ->
      (state {status = "Transpilation Failed: " ++ toText reason}, Action.none)
    OutputFileWritten ->
      (state {status = "Transpilation Completed"}, Action.none)
    OutputFileWriteErrored _ ->
      (state {status = "Output File Write Errored"}, Action.none)


view :: State -> Text
view state = state.status


transpile :: Text -> Text
transpile input =
  -- This is a placeholder for the actual transpilation logic
  -- In a real-world scenario, you'd implement your transpilation rules here
  "Transpiled: " ++ input


main :: IO ()
main =
  Service.init
    ( ANON
        { init = init,
          view = view,
          update = update,
          triggers = Array.empty
        }
    )