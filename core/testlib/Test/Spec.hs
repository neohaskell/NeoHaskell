{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE FlexibleInstances #-}

module Test.Spec (
  Spec,
  describe,
  it,
  only_it,
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldContain,
  shouldNotContain,
  shouldStartWith,
  shouldEndWith,
  shouldMatchList,
  shouldReturn,
  shouldHaveIncreasingOrder,
  pending,
  xdescribe,
  parallel,
  sequential,
  runTask,
  fail,
  beforeAll,
  before,
  after,
  afterAll,
  whenEnvVar,
  shouldHaveDecreasingOrder,
  shouldBeLessThanOrEqual,
  shouldBeGreaterThanOrEqual,
  shouldBeLessThan,
  shouldBeGreaterThan,
  varContents,
) where

import Array qualified
import Control.Monad qualified
import Core
import Environment qualified
import Service.EventStore (EventStore (..))
import Task qualified
import Test.Hspec qualified as Hspec
import Test.Service.CommandHandler.Execute.Context qualified as CommandHandlerExecuteContext
import Test.Service.EntityFetcher.Fetch.Context qualified as EntityFetcherFetchContext
import Test.Service.EntityFetcher.FetchWithCache.Context qualified as EntityFetcherFetchWithCacheContext
import Test.Service.EventStore.BatchValidation.Context qualified as EventStoreBatchValidationContext
import Test.Service.EventStore.GlobalStreamOrdering.Context qualified as EventStoreGlobalStreamOrderingContext
import Test.Service.EventStore.IndividualStreamOrdering.Context qualified as EventStoreIndividualStreamOrderingContext
import Test.Service.EventStore.OptimisticConcurrency.Context qualified as EventStoreOptimisticConcurrencyContext
import Test.Service.EventStore.ReadAllBackwardsFromEnd.Context qualified as EventStoreReadAllBackwardsFromEndContext
import Test.Service.EventStore.ReadAllForwardsFromStart.Context qualified as EventStoreReadAllForwardsFromStartContext
import Test.Service.EventStore.StreamTruncation.Context qualified as EventStoreStreamTruncationContext
import Test.Service.EventStore.Subscriptions.Context qualified as EventStoreSubscriptionsContext
import Text qualified
import Var qualified


type Spec a = Hspec.SpecWith a


class AutoCleanup context where
  autoCleanup :: context -> Task Text Unit


instance {-# OVERLAPPABLE #-} AutoCleanup context where
  autoCleanup _ = Task.yield unit


instance AutoCleanup (EventStore eventType) where
  autoCleanup store = store.close


instance AutoCleanup EventStoreReadAllForwardsFromStartContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreReadAllBackwardsFromEndContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreIndividualStreamOrderingContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreGlobalStreamOrderingContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreOptimisticConcurrencyContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreStreamTruncationContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreSubscriptionsContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EventStoreBatchValidationContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EntityFetcherFetchContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup EntityFetcherFetchWithCacheContext.Context where
  autoCleanup context = context.store.close


instance AutoCleanup CommandHandlerExecuteContext.Context where
  autoCleanup context = context.cartStore.close

-- | Describe a group of tests
describe :: Text -> Spec Unit -> Spec Unit
describe name = Hspec.describe (Text.toLinkedList name)
{-# INLINE describe #-}


-- | Define a test case
it :: (HasCallStack) => Text -> (context -> Task Text Unit) -> Spec context
it name block =
  Hspec.it (Text.toLinkedList name) \context -> do
    block context |> Task.runOrPanic
{-# INLINE it #-}


-- | Define a focused test case (runs only this test)
only_it :: (HasCallStack) => Text -> (context -> Task Text Unit) -> Spec context
only_it name block =
  Hspec.fit (Text.toLinkedList name) \context -> do
    block context |> Task.runOrPanic
{-# INLINE only_it #-}


-- | Marks a test as pending
pending :: Text -> Task Text Unit
pending name =
  Hspec.pendingWith (Text.toLinkedList name)
    |> Task.fromIO
{-# INLINE pending #-}


-- | Assert that two values are equal
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Task Text Unit
shouldBe expected actual = do
  Task.fromIO (Hspec.shouldBe actual expected)
{-# INLINE shouldBe #-}


-- | Assert that two values are not equal
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> Task Text Unit
shouldNotBe expected actual = do
  Task.fromIO (Hspec.shouldNotBe actual expected)
{-# INLINE shouldNotBe #-}


-- | Assert that a value satisfies a predicate
shouldSatisfy :: (HasCallStack, Show a) => (a -> Bool) -> a -> Task Text Unit
shouldSatisfy predicate value = do
  Task.fromIO (Hspec.shouldSatisfy value predicate)
{-# INLINE shouldSatisfy #-}


-- | Assert that an array contains another array
shouldContain :: (HasCallStack, Show a, Eq a) => Array a -> Array a -> Task Text Unit
shouldContain expected actual = do
  Task.fromIO (Hspec.shouldContain (Array.toLinkedList expected) (Array.toLinkedList actual))
{-# INLINE shouldContain #-}


-- | Assert that an array does not contain another array
shouldNotContain :: (HasCallStack, Show a, Eq a) => Array a -> Array a -> Task Text Unit
shouldNotContain expected actual = do
  Task.fromIO (Hspec.shouldNotContain (Array.toLinkedList expected) (Array.toLinkedList actual))
{-# INLINE shouldNotContain #-}


-- | Assert that a text starts with a prefix
shouldStartWith :: (HasCallStack) => Text -> Text -> Task Text Unit
shouldStartWith prefix text = do
  Task.fromIO (Hspec.shouldStartWith (Text.toLinkedList text) (Text.toLinkedList prefix))
{-# INLINE shouldStartWith #-}


-- | Assert that a string ends with a suffix
shouldEndWith :: (HasCallStack) => Text -> Text -> Task Text Unit
shouldEndWith suffix text = do
  Task.fromIO (Hspec.shouldEndWith (Text.toLinkedList text) (Text.toLinkedList suffix))
{-# INLINE shouldEndWith #-}


-- | Assert that two lists contain the same elements, regardless of order
shouldMatchList :: (HasCallStack, Show a, Eq a) => Array a -> Array a -> Task Text Unit
shouldMatchList expected actual = do
  Task.fromIO (Hspec.shouldMatchList (Array.toLinkedList actual) (Array.toLinkedList expected))
{-# INLINE shouldMatchList #-}


-- | Assert that a task returns a specific value
shouldReturn :: (HasCallStack, Show a, Eq a, Show err) => a -> Task err a -> Task err2 Unit
shouldReturn expected actual = do
  Task.fromIO (Hspec.shouldReturn (Task.runOrPanic actual) expected)
{-# INLINE shouldReturn #-}


-- | Fail the test with a message
fail :: (HasCallStack) => Text -> Task Text Unit
fail message = do
  Task.fromIO (Hspec.expectationFailure (Text.toLinkedList message))
{-# INLINE fail #-}


-- | Mark a group of tests as pending
xdescribe :: Text -> Spec Unit -> Spec Unit
xdescribe name = Hspec.xdescribe (Text.toLinkedList name)
{-# INLINE xdescribe #-}


-- | Run a Task before the tests
beforeAll :: (HasCallStack, Show err, AutoCleanup context) => Task err context -> (Spec context) -> Spec Unit
beforeAll beforeTask block =
  Hspec.beforeAll
    (Task.runOrPanic beforeTask)
    ( Hspec.afterAll
        (\context -> Task.runOrPanic (autoCleanup context))
        block
    )
{-# INLINE beforeAll #-}


-- | Run a Task before each test
before :: (HasCallStack, Show err, AutoCleanup context) => Task err context -> (Spec context) -> Spec Unit
before beforeTask block =
  Hspec.before
    (Task.runOrPanic beforeTask)
    ( Hspec.after
        (\context -> Task.runOrPanic (autoCleanup context))
        block
    )
{-# INLINE before #-}


-- -- | Run an IO action before each test with a specific value
-- beforeWith :: a -> (a -> Task _ b) -> (b -> Hspec.Spec) -> Hspec.Spec
-- beforeWith = Hspec.beforeWith

-- | Run a Task after each test
after :: (HasCallStack, Show err) => Task err Unit -> Spec Unit -> Spec Unit
after afterTask block =
  Hspec.after_
    (Task.runOrPanic afterTask)
    block
{-# INLINE after #-}


-- | Run a Task once after all tests in a group
afterAll :: (HasCallStack, Show err) => Task err Unit -> Spec Unit -> Spec Unit
afterAll afterTask block =
  Hspec.afterAll_
    (Task.runOrPanic afterTask)
    block
{-# INLINE afterAll #-}

-- | Run tests in parallel
parallel :: Hspec.Spec -> Hspec.Spec
parallel = Hspec.parallel
{-# INLINE parallel #-}


-- | Run tests sequentially
sequential :: Hspec.Spec -> Hspec.Spec
sequential = Hspec.sequential
{-# INLINE sequential #-}


-- | Run a Task in a test
runTask :: (HasCallStack) => (Show err) => Task err a -> _
runTask task =
  task |> Task.runOrPanic
{-# INLINE runTask #-}


-- | Runs a block of tests if an environment variable is set
whenEnvVar :: (HasCallStack) => Text -> Spec Unit -> Spec Unit
whenEnvVar envVarName block = do
  x <-
    Environment.getVariable envVarName
      |> Task.recover (\_ -> pure "")
      |> Task.runOrPanic @Text @Text
      |> Hspec.runIO

  Control.Monad.when (x != "") do
    block
{-# INLINE whenEnvVar #-}


-- | Assert that an array is in increasing order
shouldHaveIncreasingOrder :: (HasCallStack, Show a, Ord a) => Array a -> Task Text Unit
shouldHaveIncreasingOrder array = do
  array
    |> Array.indexed
    |> Array.take (Array.length array - 1)
    |> Task.forEach
      ( \(index, value) -> do
          case Array.get (index + 1) array of
            Just nextValue ->
              value |> shouldBeLessThan nextValue
            Nothing ->
              fail
                [fmt|Should never happen: shouldHaveIncreasingOrder: index is out of bounds.

Please report this as a bug at the NeoHaskell GitHub issue tracker:
https://github.com/NeoHaskell/NeoHaskell/issues|]
      )
{-# INLINE shouldHaveIncreasingOrder #-}


-- | Assert that an array is in decreasing order
shouldHaveDecreasingOrder :: (HasCallStack, Show a, Ord a) => Array a -> Task Text Unit
shouldHaveDecreasingOrder array =
  array
    |> Array.indexed
    |> Array.take (Array.length array - 1)
    |> Task.forEach
      ( \(index, value) -> do
          case array |> Array.get (index + 1) of
            Just nextValue ->
              value |> shouldBeGreaterThan nextValue
            Nothing ->
              fail
                [fmt|Should never happen: shouldHaveDecreasingOrder: index is out of bounds.

Please report this as a bug at the NeoHaskell GitHub issue tracker:
https://github.com/NeoHaskell/NeoHaskell/issues|]
      )
{-# INLINE shouldHaveDecreasingOrder #-}


-- | Assert that a value is less than or equal to a maximum value
shouldBeLessThanOrEqual :: (HasCallStack, Show a, Ord a) => a -> a -> Task Text Unit
shouldBeLessThanOrEqual maximum value = do
  let msg = [fmt|#{toText value} is not less than or equal to #{toText maximum}|]
  if value <= maximum
    then Task.yield unit
    else msg |> fail
{-# INLINE shouldBeLessThanOrEqual #-}


-- | Assert that a value is greater than or equal to a minimum value
shouldBeGreaterThanOrEqual :: (HasCallStack, Show a, Ord a) => a -> a -> Task Text Unit
shouldBeGreaterThanOrEqual minimum value = do
  let msg = [fmt|#{toText value} is not greater than or equal to #{toText minimum}|]
  if value >= minimum
    then Task.yield unit
    else msg |> fail
{-# INLINE shouldBeGreaterThanOrEqual #-}


-- | Assert that a value is less than a maximum value
shouldBeLessThan :: (HasCallStack, Show a, Ord a) => a -> a -> Task Text Unit
shouldBeLessThan maximum value = do
  let msg = [fmt|#{toText value} is not less than #{toText maximum}|]
  if value < maximum
    then Task.yield unit
    else msg |> fail
{-# INLINE shouldBeLessThan #-}


-- | Assert that a value is greater than a minimum value
shouldBeGreaterThan :: (HasCallStack, Show a, Ord a) => a -> a -> Task Text Unit
shouldBeGreaterThan minimum value = do
  let msg = [fmt|#{toText value} is not greater than #{toText minimum}|]
  if value > minimum
    then Task.yield unit
    else msg |> fail
{-# INLINE shouldBeGreaterThan #-}


-- | Modifier that allows asserting the contents of a Var
varContents ::
  (HasCallStack, Show a, Ord a) =>
  (a -> a -> Task Text Unit) ->
  a ->
  Var a ->
  Task Text Unit
varContents assert expected var = do
  actual <- Var.get var
  assert expected actual
{-# INLINE varContents #-}
