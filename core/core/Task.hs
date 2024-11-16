module Task (
    Task (..),
    yield,
    throw,
    map,
    apply,
    andThen,
    run,
    runOrPanic,
    mapError,
    fromFailableIO,
    fromIO,
) where

import Applicable (Applicative (pure))
import Applicable qualified
import Basics
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad.IO.Class qualified as Monad
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Either qualified as Either
import IO (IO)
import IO qualified
import Mappable (Functor)
import Mappable qualified
import Result (Result)
import Result qualified
import Thenable (Monad)
import ToText (Show, toPrettyText)


newtype Task err value = Task
    {runTask :: (ExceptT err IO) value}
    deriving (Functor, Applicable.Applicative, Monad)


yield :: value -> Task _ value
yield value = Task (Applicable.pure value)


throw :: err -> Task err value
throw err = Task (throwE err)


map :: (input -> output) -> Task err input -> Task err output
map f self = Task (Mappable.map f (runTask self))


mapError :: (err1 -> err2) -> Task err1 value -> Task err2 value
mapError f self =
    runTask self
        |> withExceptT f
        |> Task


apply :: Task err (input -> output) -> Task err input -> Task err output
apply taskFunction self = Task (Applicable.apply (runTask taskFunction) (runTask self))


andThen :: (input -> Task err output) -> Task err input -> Task err output
andThen f self = Task do
    value <- runTask self
    runTask (f value)


-- TODO: Figure out the best API to ensure that the main function is just a Task that cannot fail and returns a unit

run :: (Result err value -> IO value) -> Task err value -> IO value
run reducer task =
    runTask task
        |> runExceptT
        |> IO.map Result.fromEither
        |> IO.andThen reducer


runOrPanic :: (Show err) => Task err value -> IO value
runOrPanic task = do
    let reducer (Result.Ok value) = IO.yield value
        reducer (Result.Err err) = panic (toPrettyText err)
    task |> run reducer


-- fromFailableIO is the reverse of run
-- it receives an io, an exception catcher, and returns a task
-- the task will run the io and catch exceptions
-- if an exception is caught, it will be passed to the exception catcher
-- if no exception is caught, the result will be passed to the success handler
fromFailableIO ::
    forall exception result.
    (Exception exception) =>
    IO result ->
    Task exception result
fromFailableIO io = do
    result <- io |> Exception.try @exception |> Monad.liftIO |> Task
    case result of
        Either.Left exception -> throw exception
        Either.Right value -> pure value


fromIO :: IO value -> Task _ value
fromIO io =
    io
        |> Monad.liftIO
        |> Task
