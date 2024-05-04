module Debug.Spy where

import Debug.Trace qualified as GHCDebug
import HaskellCompatibility.Conversion qualified as Convert
import Text


spy :: Text -> a -> a
spy message = GHCDebug.trace (Convert.toLegacy message)