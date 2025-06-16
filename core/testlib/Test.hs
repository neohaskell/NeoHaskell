module Test (
  -- * Types
  Spec,

  -- * Core Test Functions
  describe,
  it,
  shouldBe,
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
  whenEnvVar,
  shouldHaveDecreasingOrder,
) where

import Array qualified
import Control.Monad qualified
import Core
import Environment qualified
import Task qualified
import Test.Hspec qualified as Hspec
import Text qualified


type Spec a = Hspec.SpecWith a


-- | Describe a group of tests
describe :: Text -> Spec Unit -> Spec Unit
describe name = Hspec.describe (Text.toLinkedList name)
{-# INLINE describe #-}


-- | Define a test case
it :: (Show err) => Text -> (context -> Task err Unit) -> Spec context
it name block =
  Hspec.it (Text.toLinkedList name) \ctx -> do
    block ctx |> Task.runOrPanic
{-# INLINE it #-}


-- | Marks a test as pending
pending :: Text -> Task err Unit
pending name =
  Hspec.pendingWith (Text.toLinkedList name)
    |> Task.fromIO
{-# INLINE pending #-}


-- | Assert that two values are equal
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Task Text Unit
shouldBe expected actual = do
  Task.fromIO (Hspec.shouldBe actual expected)
{-# INLINE shouldBe #-}


-- | Assert that a value satisfies a predicate
shouldSatisfy :: (HasCallStack, Show a) => (a -> Bool) -> a -> Task err Unit
shouldSatisfy predicate value = do
  Task.fromIO (Hspec.shouldSatisfy value predicate)
{-# INLINE shouldSatisfy #-}


-- | Assert that an array contains another array
shouldContain :: (HasCallStack, Show a, Eq a) => Array a -> Array a -> Task err Unit
shouldContain expected actual = do
  Task.fromIO (Hspec.shouldContain (Array.toLinkedList expected) (Array.toLinkedList actual))
{-# INLINE shouldContain #-}


-- | Assert that an array does not contain another array
shouldNotContain :: (HasCallStack, Show a, Eq a) => Array a -> Array a -> Task err Unit
shouldNotContain expected actual = do
  Task.fromIO (Hspec.shouldNotContain (Array.toLinkedList expected) (Array.toLinkedList actual))
{-# INLINE shouldNotContain #-}


-- | Assert that a text starts with a prefix
shouldStartWith :: (HasCallStack) => Text -> Text -> Task err Unit
shouldStartWith prefix text = do
  Task.fromIO (Hspec.shouldStartWith (Text.toLinkedList text) (Text.toLinkedList prefix))
{-# INLINE shouldStartWith #-}


-- | Assert that a string ends with a suffix
shouldEndWith :: (HasCallStack) => Text -> Text -> Task err Unit
shouldEndWith suffix text = do
  Task.fromIO (Hspec.shouldEndWith (Text.toLinkedList text) (Text.toLinkedList suffix))
{-# INLINE shouldEndWith #-}


-- | Assert that two lists contain the same elements, regardless of order
shouldMatchList :: (HasCallStack, Show a, Eq a) => Array a -> Array a -> Task err Unit
shouldMatchList expected actual = do
  Task.fromIO (Hspec.shouldMatchList (Array.toLinkedList actual) (Array.toLinkedList expected))
{-# INLINE shouldMatchList #-}


-- | Assert that a task returns a specific value
shouldReturn :: (HasCallStack, Show a, Eq a, Show err) => a -> Task err a -> Task err2 Unit
shouldReturn expected actual = do
  Task.fromIO (Hspec.shouldReturn (Task.runOrPanic actual) expected)
{-# INLINE shouldReturn #-}


-- | Fail the test with a message
fail :: Text -> Task err Unit
fail message = do
  Task.fromIO (Hspec.expectationFailure (Text.toLinkedList message))
{-# INLINE fail #-}


-- | Mark a group of tests as pending
xdescribe :: Text -> Spec Unit -> Spec Unit
xdescribe name = Hspec.xdescribe (Text.toLinkedList name)
{-# INLINE xdescribe #-}


-- | Run a Task before the tests
beforeAll :: (HasCallStack, Show err) => Task err a -> (Spec a) -> Spec Unit
beforeAll beforeTask block =
  Hspec.beforeAll
    (Task.runOrPanic beforeTask)
    block
{-# INLINE beforeAll #-}


-- -- | Run an IO action before each test with a specific value
-- beforeWith :: a -> (a -> Task _ b) -> (b -> Hspec.Spec) -> Hspec.Spec
-- beforeWith = Hspec.beforeWith

-- -- | Run an IO action after each test
-- after :: Task _ a -> Hspec.Spec -> Hspec.Spec
-- after = Hspec.after

-- -- | Run an IO action around each test
-- around :: (Task _ a -> Task _ b) -> (a -> Hspec.Spec) -> Hspec.Spec
-- around = Hspec.around

-- -- | Run an IO action around each test with a specific value
-- aroundWith :: a -> (a -> Task _ b -> Task _ c) -> (b -> Hspec.Spec) -> Hspec.Spec
-- aroundWith = Hspec.aroundWith

-- | Run tests in parallel
parallel :: Hspec.Spec -> Hspec.Spec
parallel = Hspec.parallel
{-# INLINE parallel #-}


-- | Run tests sequentially
sequential :: Hspec.Spec -> Hspec.Spec
sequential = Hspec.sequential
{-# INLINE sequential #-}


-- | Run a Task in a test
runTask :: (Show err) => Task err a -> _
runTask task =
  task |> Task.runOrPanic
{-# INLINE runTask #-}


-- | Runs a block of tests if an environment variable is set
whenEnvVar :: Text -> Spec Unit -> Spec Unit
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
shouldHaveIncreasingOrder :: (HasCallStack, Show a, Ord a, IsString err) => Array a -> Task err Unit
shouldHaveIncreasingOrder array = do
  array
    |> Array.indexed
    |> Array.take (Array.length array - 1)
    |> Task.forEach
      ( \(index, value) -> do
          case Array.get (index + 1) array of
            Just nextValue ->
              value |> shouldSatisfy (\v -> v < nextValue)
            Nothing ->
              fail
                [fmt|Should never happen: shouldHaveIncreasingOrder: index is out of bounds.
            
Please report this as a bug at the NeoHaskell GitHub issue tracker:
https://github.com/NeoHaskell/NeoHaskell/issues|]
      )
{-# INLINE shouldHaveIncreasingOrder #-}


-- | Assert that an array is in decreasing order
shouldHaveDecreasingOrder :: (HasCallStack, Show a, Ord a, IsString err) => Array a -> Task err Unit
shouldHaveDecreasingOrder array =
  array
    |> Array.indexed
    |> Array.take (Array.length array - 1)
    |> Task.forEach
      ( \(index, value) -> do
          case array |> Array.get (index + 1) of
            Just nextValue ->
              value |> shouldSatisfy (\v -> v > nextValue)
            Nothing ->
              fail
                [fmt|Should never happen: shouldHaveDecreasingOrder: index is out of bounds.
            
Please report this as a bug at the NeoHaskell GitHub issue tracker:
https://github.com/NeoHaskell/NeoHaskell/issues|]
      )
{-# INLINE shouldHaveDecreasingOrder #-}
