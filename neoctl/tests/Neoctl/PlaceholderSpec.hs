{-| Verify anonymous access to neoctl's placeholder query.
Run these checks with @neo test --ci@ from the neoctl project.
-}
module Neoctl.PlaceholderSpec (spec) where

import Core
import Neoctl.Placeholder (Placeholder (..))
import Neoctl.Placeholder qualified as Placeholder
import Test


spec :: Spec Unit
spec = describe "Placeholder query access" do
  it "allows anonymous callers to access the query" \_ -> do
    Placeholder.canAccess Nothing |> shouldBe Nothing

  it "allows anonymous callers to view a result" \_ -> do
    let placeholder = Placeholder {purpose = "Control service startup"}
    Placeholder.canView Nothing placeholder |> shouldBe Nothing
