module Promise (
  Promise (..),
  wrap,
  runAsMain,
  fromIO,
  map,
  yield,
  andThen,
  (>>=),
  pure,
  return,
  (>>),
) where

-- TODO: Move to compatibility
import Control.Applicative qualified as HsApplicative
import Data.Functor qualified as HsFunctor
import HaskellCompatibility.IO
import HaskellCompatibility.Monad qualified as Monad
import Pipe
import Void (Void)


-- | A 'Promise' is a computation that will be
-- evaluated in the future and will return a
-- `value`.
-- You await for them using '<-' in a 'do' block.
newtype Promise value = INTERNAL_CORE_PROMISE_CONSTRUCTOR
  { value :: IO value
  }


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


andThen :: (value -> Promise value2) -> Promise value -> Promise value2
andThen function (INTERNAL_CORE_PROMISE_CONSTRUCTOR io) =
  io
    |> Monad._andThen (function .> value)
    |> INTERNAL_CORE_PROMISE_CONSTRUCTOR


yield :: value -> Promise value
yield value =
  Monad._yield value
    |> INTERNAL_CORE_PROMISE_CONSTRUCTOR


(>>=) :: Promise value -> (value -> Promise value2) -> Promise value2
(>>=) a b = andThen b a


(>>) :: Promise value -> Promise value2 -> Promise value2
(>>) a b = a >>= \_ -> b


pure :: value -> Promise value
pure = yield


return :: value -> Promise value
return = yield