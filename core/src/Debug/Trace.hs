module Debug.Trace (report) where

import Debug.ToDo (todo)
import GHC.Base qualified as GHC
import HaskellCompatibility.Conversion qualified as Convert
import Pipe
import String (String)
import System.IO qualified as GHC
import System.IO.Unsafe qualified as GHC


-- | Print a message to the standard output.
-- Note that this function should be used for debugging purposes only,
-- as the compiler usually inlines functions that are pure, and it could
-- be that the message is not printed at all.
report :: String -> a
report message = GHC.unsafePerformIO (Convert.toLegacy message |> GHC.putStrLn) `GHC.seq` todo
{-# INLINE report #-}