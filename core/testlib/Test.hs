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
  pendingWith,
  xit,
  xdescribe,
  parallel,
  sequential,
  runTask,
) where

import Array qualified
import Core
import Task qualified
import Test.Hspec qualified as Hspec
import Text qualified


type Spec = Hspec.Spec


-- | Describe a group of tests
describe :: Text -> Hspec.Spec -> Hspec.Spec
describe name = Hspec.describe (Text.toLinkedList name)


-- | Define a test case
it :: (Hspec.Example a) => Text -> a -> Hspec.SpecWith (Hspec.Arg a)
it name = Hspec.it (Text.toLinkedList name)


-- | Assert that two values are equal
shouldBe :: (Show a, Eq a) => a -> a -> Hspec.Expectation
shouldBe = Hspec.shouldBe


-- | Assert that a value satisfies a predicate
shouldSatisfy :: (Show a) => (a -> Bool) -> a -> Hspec.Expectation
shouldSatisfy predicate value = Hspec.shouldSatisfy value predicate


-- | Assert that an array contains another array
shouldContain :: (Show a, Eq a) => Array a -> Array a -> Hspec.Expectation
shouldContain expected actual = Hspec.shouldContain (Array.toLinkedList expected) (Array.toLinkedList actual)


-- | Assert that an array does not contain another array
shouldNotContain :: (Show a, Eq a) => Array a -> Array a -> Hspec.Expectation
shouldNotContain expected actual = Hspec.shouldNotContain (Array.toLinkedList expected) (Array.toLinkedList actual)


-- | Assert that a text starts with a prefix
shouldStartWith :: Text -> Text -> Hspec.Expectation
shouldStartWith prefix text = Hspec.shouldStartWith (Text.toLinkedList text) (Text.toLinkedList prefix)


-- | Assert that a string ends with a suffix
shouldEndWith :: Text -> Text -> Hspec.Expectation
shouldEndWith suffix text = Hspec.shouldEndWith (Text.toLinkedList text) (Text.toLinkedList suffix)


-- | Assert that two lists contain the same elements, regardless of order
shouldMatchList :: (Show a, Eq a) => Array a -> Array a -> Hspec.Expectation
shouldMatchList expected actual = Hspec.shouldMatchList (Array.toLinkedList actual) (Array.toLinkedList expected)


-- | Assert that a task returns a specific value
shouldReturn :: (Show a, Eq a, Show err) => a -> Task err a -> Hspec.Expectation
shouldReturn expected actual =
  Hspec.shouldReturn (Task.runOrPanic actual) expected


-- | Mark a test as pending
pending :: Hspec.Expectation
pending = Hspec.pending


-- | Mark a test as pending with a reason
pendingWith :: Text -> Hspec.Expectation
pendingWith reason = Hspec.pendingWith (Text.toLinkedList reason)


-- | Mark a test as pending (alternative syntax)
xit :: (Hspec.Example a) => Text -> a -> Hspec.SpecWith (Hspec.Arg a)
xit name = Hspec.xit (Text.toLinkedList name)


-- | Mark a group of tests as pending
xdescribe :: Text -> Hspec.Spec -> Hspec.Spec
xdescribe name = Hspec.xdescribe (Text.toLinkedList name)


-- | Run an IO action before each test
-- before :: Task _ a -> (a -> Hspec.Spec) -> Hspec.Spec
-- before = Hspec.before

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
