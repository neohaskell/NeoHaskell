module Test (
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
  shouldThrow,
  pending,
  pendingWith,
  xit,
  xdescribe,
  before,
  beforeWith,
  after,
  around,
  aroundWith,
  parallel,
  sequential,
  runIO,
  runTask,
) where

import Control.Exception qualified as Exception
import Core
import IO (IO)
import IO qualified
import Result (Result)
import Result qualified
import Task (Task)
import Task qualified
import Test.Hspec qualified as Hspec
import Text (Text)
import Text qualified
import ToText (Show, toPrettyText)


-- | Describe a group of tests
describe :: Text -> Hspec.Spec -> Hspec.Spec
describe name = Hspec.describe (Text.toLinkedList name)


-- | Define a test case
it :: Text -> Hspec.Spec -> Hspec.Spec
it name = Hspec.it (Text.toLinkedList name)


-- | Assert that two values are equal
shouldBe :: (Show a, Eq a) => a -> a -> Hspec.Expectation
shouldBe = Hspec.shouldBe


-- | Assert that a value satisfies a predicate
shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Hspec.Expectation
shouldSatisfy = Hspec.shouldSatisfy


-- | Assert that a list contains a value
shouldContain :: (Show a, Eq a) => [a] -> a -> Hspec.Expectation
shouldContain = Hspec.shouldContain


-- | Assert that a list does not contain a value
shouldNotContain :: (Show a, Eq a) => [a] -> a -> Hspec.Expectation
shouldNotContain = Hspec.shouldNotContain


-- | Assert that a string starts with a prefix
shouldStartWith :: Text -> Text -> Hspec.Expectation
shouldStartWith prefix text = Hspec.shouldStartWith (Text.toLinkedList prefix) (Text.toLinkedList text)


-- | Assert that a string ends with a suffix
shouldEndWith :: Text -> Text -> Hspec.Expectation
shouldEndWith suffix text = Hspec.shouldEndWith (Text.toLinkedList suffix) (Text.toLinkedList text)


-- | Assert that two lists contain the same elements, regardless of order
shouldMatchList :: (Show a, Eq a) => [a] -> [a] -> Hspec.Expectation
shouldMatchList = Hspec.shouldMatchList


-- | Assert that an IO action returns a specific value
shouldReturn :: (Show a, Eq a) => IO a -> a -> Hspec.Expectation
shouldReturn = Hspec.shouldReturn


-- | Mark a test as pending
pending :: Hspec.Spec
pending = Hspec.pending


-- | Mark a test as pending with a reason
pendingWith :: Text -> Hspec.Spec
pendingWith reason = Hspec.pendingWith (Text.toLinkedList reason)


-- | Mark a test as pending (alternative syntax)
xit :: Text -> Hspec.Spec -> Hspec.Spec
xit name = Hspec.xit (Text.toLinkedList name)


-- | Mark a group of tests as pending
xdescribe :: Text -> Hspec.Spec -> Hspec.Spec
xdescribe name = Hspec.xdescribe (Text.toLinkedList name)


-- | Run an IO action before each test
before :: IO a -> (a -> Hspec.Spec) -> Hspec.Spec
before = Hspec.before


-- | Run an IO action before each test with a specific value
beforeWith :: a -> (a -> IO b) -> (b -> Hspec.Spec) -> Hspec.Spec
beforeWith = Hspec.beforeWith


-- | Run an IO action after each test
after :: IO a -> Hspec.Spec -> Hspec.Spec
after = Hspec.after


-- | Run an IO action around each test
around :: (IO a -> IO b) -> (a -> Hspec.Spec) -> Hspec.Spec
around = Hspec.around


-- | Run an IO action around each test with a specific value
aroundWith :: a -> (a -> IO b -> IO c) -> (b -> Hspec.Spec) -> Hspec.Spec
aroundWith = Hspec.aroundWith


-- | Run tests in parallel
parallel :: Hspec.Spec -> Hspec.Spec
parallel = Hspec.parallel


-- | Run tests sequentially
sequential :: Hspec.Spec -> Hspec.Spec
sequential = Hspec.sequential


-- | Run an IO action in a test
runIO :: IO a -> Hspec.Expectation
runIO = Hspec.runIO


-- | Run a Task in a test
runTask :: (Show err) => Task err a -> Hspec.Expectation
runTask task = do
  result <- Task.run Result.fromEither task
  case result of
    Result.Ok value -> Hspec.runIO (IO.yield value)
    Result.Err err -> Hspec.expectationFailure (Text.toLinkedList (toPrettyText err))
