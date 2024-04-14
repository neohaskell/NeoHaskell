module Process (getArgs) where

import Array (Array, applyToEach)
import HaskellCompatibility.Conversion qualified as Convert
import HaskellCompatibility.List qualified as Convert
import Pipe
import Promise (Promise)
import Promise qualified
import String (String)
import System.Environment qualified as GHC


getArgs :: Promise (Array String)
getArgs = Promise.do
  legacyArgs <- GHC.getArgs |> Promise.fromIO
  legacyArgs
    |> Convert.fromList
    |> Array.applyToEach Convert.fromLegacy
    |> Promise.yield