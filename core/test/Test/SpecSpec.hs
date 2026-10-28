{-| Check the pipe-friendly containment matchers exposed by 'Test'.
Use 'spec' to verify that the piped array is the value being inspected.
-}
module Test.SpecSpec (spec) where

import Core
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Test.Spec containment" do
    it "finds a shorter subarray in the actual array" \_ -> do
      (["alpha", "beta", "gamma"] :: Array Text)
        |> shouldContain ["beta"]

    it "accepts a missing subarray even when its prefix contains the actual array" \_ -> do
      (["alpha", "beta"] :: Array Text)
        |> shouldNotContain ["alpha", "beta", "gamma"]

    it "rejects a containment check when only the expected array contains the actual array" \_ -> do
      result <-
        (["beta"] :: Array Text)
          |> shouldContain ["alpha", "beta", "gamma"]
          |> Task.asResultSafe @Text
      case result of
        Err _ -> Task.yield unit
        Ok _ -> Test.fail "the actual array did not contain the expected array"

    it "rejects a forbidden subarray in the actual array" \_ -> do
      result <-
        (["alpha", "beta"] :: Array Text)
          |> shouldNotContain ["beta"]
          |> Task.asResultSafe @Text
      case result of
        Err _ -> Task.yield unit
        Ok _ -> Test.fail "the actual array contained the forbidden array"
