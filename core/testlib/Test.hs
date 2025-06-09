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
  pending,
  xdescribe,
  parallel,
  sequential,
  runTask,
  fail,
  beforeAll,
  whenEnvVar,
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


-- | Define a test case
it :: (Show err) => Text -> (context -> Task err Unit) -> Spec context
it name block =
  Hspec.it (Text.toLinkedList name) \ctx -> do
    block ctx |> Task.runOrPanic


-- | Marks a test as pending
pending :: Text -> Task err Unit
pending name =
  Hspec.pendingWith (Text.toLinkedList name)
    |> Task.fromIO


-- | Assert that two values are equal
shouldBe :: (Show a, Eq a) => a -> a -> Task Text Unit
shouldBe expected actual = do
  Task.fromIO (Hspec.shouldBe actual expected)


-- | Assert that a value satisfies a predicate
shouldSatisfy :: (Show a) => (a -> Bool) -> a -> Task err Unit
shouldSatisfy predicate value = do
  Task.fromIO (Hspec.shouldSatisfy value predicate)


-- | Assert that an array contains another array
shouldContain :: (Show a, Eq a) => Array a -> Array a -> Task err Unit
shouldContain expected actual = do
  Task.fromIO (Hspec.shouldContain (Array.toLinkedList expected) (Array.toLinkedList actual))


-- | Assert that an array does not contain another array
shouldNotContain :: (Show a, Eq a) => Array a -> Array a -> Task err Unit
shouldNotContain expected actual = do
  Task.fromIO (Hspec.shouldNotContain (Array.toLinkedList expected) (Array.toLinkedList actual))


-- | Assert that a text starts with a prefix
shouldStartWith :: Text -> Text -> Task err Unit
shouldStartWith prefix text = do
  Task.fromIO (Hspec.shouldStartWith (Text.toLinkedList text) (Text.toLinkedList prefix))


-- | Assert that a string ends with a suffix
shouldEndWith :: Text -> Text -> Task err Unit
shouldEndWith suffix text = do
  Task.fromIO (Hspec.shouldEndWith (Text.toLinkedList text) (Text.toLinkedList suffix))


-- | Assert that two lists contain the same elements, regardless of order
shouldMatchList :: (Show a, Eq a) => Array a -> Array a -> Task err Unit
shouldMatchList expected actual = do
  Task.fromIO (Hspec.shouldMatchList (Array.toLinkedList actual) (Array.toLinkedList expected))


-- | Assert that a task returns a specific value
shouldReturn :: (Show a, Eq a, Show err) => a -> Task err a -> Task err2 Unit
shouldReturn expected actual = do
  Task.fromIO (Hspec.shouldReturn (Task.runOrPanic actual) expected)


-- | Fail the test with a message
fail :: Text -> Task err Unit
fail message = do
  Task.fromIO (Hspec.expectationFailure (Text.toLinkedList message))


-- | Mark a group of tests as pending
xdescribe :: Text -> Spec Unit -> Spec Unit
xdescribe name = Hspec.xdescribe (Text.toLinkedList name)


-- | Run a Task before the tests
beforeAll :: (Show err) => Task err a -> (Spec a) -> Spec Unit
beforeAll beforeTask block =
  Hspec.beforeAll
    (Task.runOrPanic beforeTask)
    block


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


-- | Run tests sequentially
sequential :: Hspec.Spec -> Hspec.Spec
sequential = Hspec.sequential


-- | Run a Task in a test
runTask :: (Show err) => Task err a -> _
runTask task =
  task |> Task.runOrPanic


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
