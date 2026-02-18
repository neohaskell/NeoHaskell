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
  runMain,
  forEach,
  runNoErrors,
  mapArray,
  runResult,
  unless,
  when,
  recover,
  asResult,
  asResultSafe,
  fromIOResult,
  forever,
  errorAsResult,
  fromIOEither,
  while,
  ignoreError,
  finally,
) where

import Applicable (Applicative (pure))
import Applicable qualified
import Array (Array)
import Array qualified
import Basics
import Control.Exception (Exception, SomeException)
import Control.Exception qualified as Exception
import Control.Monad qualified
import Control.Monad.IO.Class qualified as Monad
import Control.Monad.Loops qualified as Loops
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Either qualified as Either
import Data.Foldable qualified
import Data.Text.IO qualified as GHCText
import IO (IO)
import IO qualified
import Main.Utf8 (withUtf8)
import Mappable (Functor)
import Mappable qualified
import Maybe (Maybe (..))
import Result (Result)
import Result qualified
import Text (Text)
import Text qualified
import Thenable (Monad)
import ToText (toPrettyText)
import Prelude qualified


newtype Task err value = Task
  {runTask :: (ExceptT err IO) value}
  deriving (Functor, Applicable.Applicative, Monad)


yield :: value -> Task _ value
yield value = Task (Applicable.pure value)
{-# INLINE yield #-}


throw :: err -> Task err _
throw err = Task (Except.throwE err)
{-# INLINE throw #-}


map :: (input -> output) -> Task err input -> Task err output
map f self = Task (Mappable.map f (self.runTask))
{-# INLINE map #-}


mapError :: (err1 -> err2) -> Task err1 value -> Task err2 value
mapError f self =
  self.runTask
    |> Except.withExceptT f
    |> Task
{-# INLINE mapError #-}


apply :: Task err (input -> output) -> Task err input -> Task err output
apply taskFunction self = Task (Applicable.apply (taskFunction.runTask) (self.runTask))
{-# INLINE apply #-}


recover :: (err -> Task err2 value) -> Task err value -> Task err2 value
recover f self = Task (Except.catchE (self.runTask) (\it -> (f it).runTask))
{-# INLINE recover #-}


asResult :: Task err value -> Task err2 (Result err value)
asResult task =
  runResult task
    |> fromIO
{-# INLINE asResult #-}


-- | Like 'asResult', but also catches IO exceptions (not just ExceptT errors).
--
-- 'asResult' only catches errors thrown via 'Task.throw' (ExceptT layer).
-- IO exceptions from the underlying IO action propagate uncaught, which can
-- silently kill worker threads.
--
-- 'asResultSafe' catches BOTH:
-- 1. ExceptT errors (from Task.throw) — converted via 'show'
-- 2. IO exceptions (from underlying IO) — converted via 'show'
--
-- Essential for worker loops where uncaught IO exceptions would kill the thread.
asResultSafe ::
  forall err value err2.
  (Show err) =>
  Task err value ->
  Task err2 (Result Text value)
asResultSafe task = do
  result <-
    task.runTask
      |> Except.runExceptT
      |> Exception.try @SomeException
      |> fromIO
  case result of
    Either.Left someException ->
      yield (Result.Err (show someException |> Text.fromLinkedList))
    Either.Right (Either.Left err) ->
      yield (Result.Err (show err |> Text.fromLinkedList))
    Either.Right (Either.Right value) ->
      yield (Result.Ok value)
{-# INLINE asResultSafe #-}


andThen :: (input -> Task err output) -> Task err input -> Task err output
andThen f self = Task do
  value <- self.runTask
  (f value).runTask
{-# INLINE andThen #-}


-- TODO: Figure out the best API to ensure that the main function is just a Task that cannot fail and returns a unit

runResult :: Task err value -> IO (Result err value)
runResult task = do
  task.runTask
    |> Except.runExceptT
    |> IO.map Result.fromEither
{-# INLINE runResult #-}


run :: (Result err value -> IO value) -> Task err value -> IO value
run reducer task = do
  task.runTask
    |> Except.runExceptT
    |> IO.map Result.fromEither
    |> IO.andThen reducer
    |> withUtf8
{-# INLINE run #-}


runNoErrors :: Task Never value -> IO value
runNoErrors task = do
  let reducer (Result.Ok value) = IO.yield value
      reducer (Result.Err _) = panic "Task.runNoErrors: Impossible error in Task Never - this indicates a type system violation"
  task.runTask
    |> Except.runExceptT
    |> IO.map Result.fromEither
    |> IO.andThen reducer
    |> withUtf8
{-# INLINE runNoErrors #-}


runOrPanic :: (HasCallStack, Show err) => Task err value -> IO value
runOrPanic task = do
  let reducer result = case result of
        Result.Ok value -> IO.yield value
        Result.Err err -> panic (toPrettyText err)
      {-# INLINE reducer #-}
  task
    |> run reducer
    |> withUtf8
{-# INLINE runOrPanic #-}


runMain :: Task Text Unit -> IO Unit
runMain task = do
  let reducer (Result.Ok _) = IO.yield ()
      reducer (Result.Err err) = GHCText.putStrLn err
  task
    |> run reducer
    |> withUtf8
{-# INLINE runMain #-}


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
    Either.Right value -> yield value
{-# INLINE fromFailableIO #-}


fromIO :: IO value -> Task _ value
fromIO io =
  io
    |> Monad.liftIO
    |> Task
{-# INLINE fromIO #-}


fromIOResult :: (Show err) => IO (Result err value) -> Task err value
fromIOResult io =
  io
    |> Prelude.fmap Result.toEither
    |> Except.ExceptT
    |> Task
{-# INLINE fromIOResult #-}


fromIOEither :: (Show err) => IO (Prelude.Either err value) -> Task err value
fromIOEither io =
  io
    |> Except.ExceptT
    |> Task
{-# INLINE fromIOEither #-}


forEach ::
  forall (element :: Type) (err :: Type).
  (element -> Task err Unit) ->
  Array element ->
  Task err Unit
forEach callback array =
  Data.Foldable.traverse_ callback (Array.unwrap array)
{-# INLINE forEach #-}


mapArray :: (element -> Task err output) -> Array element -> Task err (Array output)
mapArray f array =
  Array.unwrap array
    |> Control.Monad.mapM f
    |> Task.map Array.fromLegacy
{-# INLINE mapArray #-}


-- | Run a task if a condition is false
unless :: Bool -> Task err Unit -> Task err Unit
unless condition task =
  if condition
    then Applicable.pure unit
    else task
{-# INLINE unless #-}


-- | Run a task if a condition is true
when :: Bool -> Task err Unit -> Task err Unit
when condition task =
  if condition
    then task
    else Applicable.pure unit
{-# INLINE when #-}


-- | Run a task indefinitely until it throws an exception
-- The exception will be caught and returned as an error
forever :: Task err Unit -> Task err Unit
forever task = Task do
  let loop = do
        result <- Monad.liftIO (Except.runExceptT (task.runTask))
        case result of
          Either.Left err -> Except.throwE err -- Propagate the error and exit the loop
          Either.Right _ -> loop -- Continue the loop
      {-# INLINE loop #-}
  loop
{-# INLINE forever #-}


-- | Returns the error as the result
errorAsResult :: Task err Unit -> Task Never (Maybe err)
errorAsResult task =
  task.runTask
    |> Except.mapExceptT
      ( \x -> do
          result <- x
          case result of
            Either.Right _ -> pure (Either.Right Nothing)
            Either.Left err -> pure (Either.Right (Just err))
      )
    |> Task
{-# INLINE errorAsResult #-}


-- | Repeatedly run a task while a condition is true
-- The condition is checked before each iteration
while :: Task err Bool -> Task err Unit -> Task err Unit
while condition action = Task do
  Loops.whileM_ (condition.runTask) (action.runTask)
{-# INLINE while #-}


-- | Ignores error on unit result
ignoreError :: Task err Unit -> Task _ Unit
ignoreError task = do
  result <- task |> asResult
  case result of
    Result.Err _ -> yield unit
    Result.Ok res -> yield res
{-# INLINE ignoreError #-}


-- | Run a cleanup action after a task completes, regardless of success or failure.
-- The cleanup action runs even if the main task throws an IO exception.
-- This is essential for releasing locks, closing resources, etc.
--
-- NOTE: If the cleanup action fails, its error is silently discarded and the
-- original task's result (success or failure) is preserved. This matches
-- standard finally semantics in most languages. If you need to handle cleanup
-- failures, use 'Task.asResult' on the cleanup action.
--
-- Example:
-- @
-- Task.finally cleanup mainTask
-- -- cleanup runs whether mainTask succeeds, fails with typed error, or throws IO exception
-- @
finally :: Task err2 Unit -> Task err value -> Task err value
finally cleanup task = Task do
  let mainIO = Except.runExceptT (task.runTask)
  let cleanupIO = Except.runExceptT (cleanup.runTask)
  -- Use bracket pattern: run main, then cleanup regardless of outcome
  result <- Monad.liftIO (Exception.finally mainIO (IO.map (Prelude.const ()) cleanupIO))
  case result of
    Either.Left err -> Except.throwE err
    Either.Right val -> Prelude.pure val
{-# INLINE finally #-}