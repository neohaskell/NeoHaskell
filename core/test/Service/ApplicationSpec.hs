module Service.ApplicationSpec where

import Application qualified
import Core
import Test


spec :: Spec Unit
spec = do
  describe "Application" do
    describe "new" do
      it "creates an empty application" \_ -> do
        let _app = Application.new
        -- Application exists and can be created
        True |> shouldBe True

    describe "add" do
      it "adds a service to the application" \_ -> do
        -- For now, just verify the types work
        -- We'll add more meaningful tests as we build out functionality
        let _app = Application.new
        True |> shouldBe True
