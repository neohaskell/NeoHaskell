module Neo.Run (
  handle,
  Error (..),
) where

import Array qualified
import Directory qualified
import Neo.Core
import Subprocess qualified
import Task qualified


data Error
  = NixFileError
  | DirectoryError Directory.Error
  | CustomError Text
  deriving (Show)


handle :: ProjectConfiguration -> Task Error Unit
handle config =
  do
    let projectName = config.name
    let commandText = [fmt|./result/bin/#{projectName}|]
    rootFolder <-
      Directory.getCurrent
        |> Task.mapError DirectoryError
    Subprocess.openInherit commandText (Array.fromLinkedList []) rootFolder Subprocess.InheritBOTH
    |> discard
