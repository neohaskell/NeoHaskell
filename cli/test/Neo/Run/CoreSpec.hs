module Neo.Run.CoreSpec (spec) where

import Core
import Neo.Run.Core
import Test

spec :: Spec Unit
spec = do
  describe "Neo.Run.Core" do
    describe "initialState" do
      it "has empty status" \_ -> do
        initialState.status |> shouldBe ""

      it "has Nothing for pid" \_ -> do
        initialState.pid |> shouldBe Nothing

      it "has empty projectPath" \_ -> do
        initialState.projectPath |> shouldBe ""

    describe "update" do
      describe "RunRequested" do
        it "sets status to requested" \_ -> do
          let event = RunRequested
                (RunRequestedEvent
                  { entityId = def,
                    projectPath = "/home/user/my-project"
                  })
          let updated = update event initialState
          updated.status |> shouldBe "requested"

        it "sets projectPath from event" \_ -> do
          let event = RunRequested
                (RunRequestedEvent
                  { entityId = def,
                    projectPath = "/tmp/test-project"
                  })
          let updated = update event initialState
          updated.projectPath |> shouldBe "/tmp/test-project"

      describe "AppStarted" do
        it "sets status to running" \_ -> do
          let event = AppStarted
                (AppStartedEvent
                  { entityId = def,
                    pid = 42
                  })
          let updated = update event initialState
          updated.status |> shouldBe "running"

        it "sets pid from event" \_ -> do
          let event = AppStarted
                (AppStartedEvent
                  { entityId = def,
                    pid = 12345
                  })
          let updated = update event initialState
          updated.pid |> shouldBe (Just 12345)

      describe "AppStopped" do
        it "sets status to stopped" \_ -> do
          let state = initialState { status = "running", pid = Just 42 }
          let event = AppStopped (AppStoppedEvent { entityId = def })
          let updated = update event state
          updated.status |> shouldBe "stopped"

        it "clears pid when stopped" \_ -> do
          let state = initialState { status = "running", pid = Just 42 }
          let event = AppStopped (AppStoppedEvent { entityId = def })
          let updated = update event state
          updated.pid |> shouldBe Nothing
