{-| Demonstrate the public containment matchers in a runnable application test.
Run this module through the @nhtestbed-spec-demo@ test suite.
-}
module Main (main) where

import Core
import Test
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec spec


spec :: Spec Unit
spec = do
  describe "public containment matchers" do
    it "finds a value in the piped array" \_ -> do
      (["draft", "review", "published"] :: Array Text)
        |> shouldContain ["review"]

    it "accepts a longer missing sequence" \_ -> do
      (["draft", "review"] :: Array Text)
        |> shouldNotContain ["draft", "review", "published"]
