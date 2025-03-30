module Neo.Build.Templates.AppMain where

import Core
import Neo.Core
import Text qualified


template :: ProjectConfiguration -> Text
template
  ProjectConfiguration {name} = do
    let mainModuleName = Text.toPascalCase name
    [fmt|module Main where

import Core
import Task qualified
import {mainModuleName} qualified

main :: IO ()
main = Task.runOrPanic {mainModuleName}.run
|]
