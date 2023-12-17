module Neo.Views.Repl () where

import Core


data View
  = Repl


data ViewComponent viewFormat message


data Cli


changeHandler :: ViewHandler
changeHandler = do
  text <- get #value @String
  registerEvent (InputChanged text)


submitHandler :: ViewHandler
submitHandler = do
  registerEvent InputSubmitted


replUi :: ViewComponent Cli Event
replUi = do
  state <- getState "repl"

  let historyUi =
        state . history
          |> Array.applyToEach historyResultUi

  cliView do
    div [] do
      historyUi

    input [onChange changeHandler, onSubmit submitHandler] do
      text ("> " + state . currentCommand)


historyUi :: ViewComponent Cli Event
historyUi = do
  state <- getState "repl"

  state . history
    |> Array.applyToEach historyResultUi


historyResultUi :: HistoryResult -> ViewComponent Cli Event
historyResultUi historyResult =
  div [] do
    text ("> " + historyResult . command)
    text historyResult . output