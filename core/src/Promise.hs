module Promise (
  Promise (..),
  wrap,
  runAsMain,
  fromIO,
  map,
) where

-- TODO: Move to compatibility
import Control.Applicative qualified as HsApplicative
import Data.Functor qualified as HsFunctor
import HaskellCompatibility.IO
import Void (Void)


-- | A 'Promise' is a computation that will be
-- evaluated in the future and will return a
-- `value`.
-- You await for them using '<-' in a 'do' block.
newtype Promise value = INTERNAL_CORE_PROMISE_CONSTRUCTOR (IO value)


-- | 'wrap' is a constructor for 'Promise's.
-- It will wrap the passed value into a 'Promise',
-- regardless of the value.
wrap :: value -> Promise value
wrap value =
  INTERNAL_CORE_PROMISE_CONSTRUCTOR (HsApplicative.pure value)


-- | `map` is a function that will apply a function
-- to the value of a 'Promise' once it is resolved.
map :: (value -> value2) -> Promise value -> Promise value2
map function (INTERNAL_CORE_PROMISE_CONSTRUCTOR io) =
  INTERNAL_CORE_PROMISE_CONSTRUCTOR (HsFunctor.fmap function io)


-- | 'unsafeConvertToHaskellIO' is a function that
-- will convert a 'Promise' into a 'IO'. It should
-- not be used unless you know what you are doing.
runAsMain :: Promise Void -> MainFunction
runAsMain (INTERNAL_CORE_PROMISE_CONSTRUCTOR io) =
  io


-- | 'fromIO' is a function that will convert a 'IO' into a 'Promise'. Shouldn't be used, unless for wrapping purposes.
fromIO :: IO value -> Promise value
fromIO io =
  INTERNAL_CORE_PROMISE_CONSTRUCTOR io