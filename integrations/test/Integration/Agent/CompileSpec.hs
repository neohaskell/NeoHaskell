{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Integration.Agent.CompileSpec (spec) where

import Basics
import Test.CompileTime (NFData (..))
import Integration.Agent.Types (Config, defaultConfig)
import Integration.OpenRouter.Response (ToolCallFunction (..))
import Redacted qualified
import Task qualified
import Test.CompileTime qualified as CompileTime
import Test.Hspec
import Text (Text)


-- | Stub NFData instance required by shouldNotTypecheck.
-- The actual Config type intentionally omits NFData; this exists only for testing.
-- Orphan instance: acceptable in test-only module.
instance NFData Config where
  rnf _ = ()


-- | Stub NFData instance required by shouldNotTypecheck.
-- ToolCallFunction intentionally omits NFData to avoid deriving constraints.
-- Orphan instance: acceptable in test-only module.
instance NFData ToolCallFunction where
  rnf _ = ()





spec :: Spec
spec = do
  describe "Integration.Agent.Types - Config no-Show contract" do
    it "show is rejected at compile-time for Config" do
      CompileTime.shouldNotTypecheck (show (defaultConfig :: Config))
        |> Task.runOrPanic


    it "Eq still works for Config" do
      (defaultConfig == defaultConfig) `shouldBe` True

  describe "Integration.OpenRouter.Response - ToolCallFunction no-Show contract" do
    it "show is rejected at compile-time for ToolCallFunction" do
      let tcf = ToolCallFunction
            { name = "AddItem"
            , arguments = Redacted.wrap ("{" :: Text)
            }
      CompileTime.shouldNotTypecheck (show tcf)
        |> Task.runOrPanic
