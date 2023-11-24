module Promise (
  Promise,
  wrap,
  runAsMain,
  fromIO,
) where

-- TODO: Move to compatibility
import Control.Applicative qualified as HsApplicative
import HaskellCompatibility.IO
import Void (Void)


-- | A 'Promise' is a computation that will be
-- evaluated in the future and will return a
-- `value`.
-- You await for them using '<-' in a 'do' block.
newtype Promise value = Promise (IO value)


-- | 'wrap' is a constructor for 'Promise's.
-- It will wrap the passed value into a 'Promise',
-- regardless of the value.
wrap :: value -> Promise value
wrap value =
  Promise (HsApplicative.pure value)


-- | 'unsafeConvertToHaskellIO' is a function that
-- will convert a 'Promise' into a 'IO'. It should
-- not be used unless you know what you are doing.
runAsMain :: Promise Void -> MainFunction
runAsMain (Promise io) =
  io


-- | 'fromIO' is a function that will convert a 'IO' into a 'Promise'. Shouldn't be used, unless for wrapping purposes.
fromIO :: IO value -> Promise value
fromIO io =
  Promise io