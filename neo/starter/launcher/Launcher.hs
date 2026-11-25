module Main where

import App (appEventModel)
import Core
import Service.EventModel.Launcher qualified as EventModel
import Task qualified


main :: IO ()
main = appEventModel |> EventModel.run |> Task.runOrPanic
