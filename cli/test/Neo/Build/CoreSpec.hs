module Neo.Build.CoreSpec (spec) where

import Array qualified
import Core
import Neo.Build.Core
import Test

spec :: Spec Unit
spec = do
  describe "Neo.Build.Core" do
    describe "initialState" do
      it "has empty status" \_ -> do
        initialState.status |> shouldBe ""

      it "has empty projectPath" \_ -> do
        initialState.projectPath |> shouldBe ""

      it "has empty modulesTranspiled array" \_ -> do
        initialState.modulesTranspiled |> Array.length |> shouldBe 0

      it "has empty errors array" \_ -> do
        initialState.errors |> Array.length |> shouldBe 0

      it "has empty warnings array" \_ -> do
        initialState.warnings |> Array.length |> shouldBe 0

    describe "update" do
      describe "BuildStarted" do
        it "sets status to building" \_ -> do
          let event = BuildStarted
                (BuildStartedEvent
                  { entityId = def,
                    projectPath = "/home/user/my-project"
                  })
          let updated = update event initialState
          updated.status |> shouldBe "building"

        it "sets projectPath from event" \_ -> do
          let event = BuildStarted
                (BuildStartedEvent
                  { entityId = def,
                    projectPath = "/tmp/test-project"
                  })
          let updated = update event initialState
          updated.projectPath |> shouldBe "/tmp/test-project"

      describe "ModuleTranspiled" do
        it "appends module name to modulesTranspiled" \_ -> do
          let state = initialState { status = "building" }
          let event = ModuleTranspiled
                (ModuleTranspiledEvent
                  { entityId = def,
                    moduleName = "Main"
                  })
          let updated = update event state
          updated.modulesTranspiled |> Array.length |> shouldBe 1

        it "preserves existing modules when adding new one" \_ -> do
          let state = initialState { modulesTranspiled = ["Module1", "Module2"] }
          let event = ModuleTranspiled
                (ModuleTranspiledEvent
                  { entityId = def,
                    moduleName = "Module3"
                  })
          let updated = update event state
          updated.modulesTranspiled |> Array.length |> shouldBe 3

      describe "BuildSucceeded" do
        it "sets status to succeeded" \_ -> do
          let state = initialState { status = "building" }
          let event = BuildSucceeded
                (BuildSucceededEvent
                  { entityId = def,
                    outputPath = "/tmp/output",
                    warnings = Array.empty
                  })
          let updated = update event state
          updated.status |> shouldBe "succeeded"

        it "sets warnings from event" \_ -> do
          let state = initialState { status = "building" }
          let event = BuildSucceeded
                (BuildSucceededEvent
                  { entityId = def,
                    outputPath = "/tmp/output",
                    warnings = ["Unused import", "Type warning"]
                  })
          let updated = update event state
          updated.warnings |> Array.length |> shouldBe 2

      describe "BuildFailed" do
        it "sets status to failed" \_ -> do
          let state = initialState { status = "building" }
          let event = BuildFailed
                (BuildFailedEvent
                  { entityId = def,
                    errors = Array.empty
                  })
          let updated = update event state
          updated.status |> shouldBe "failed"

        it "sets errors from event" \_ -> do
          let state = initialState { status = "building" }
          let event = BuildFailed
                (BuildFailedEvent
                  { entityId = def,
                    errors = ["Syntax error", "Type mismatch"]
                  })
          let updated = update event state
          updated.errors |> Array.length |> shouldBe 2
