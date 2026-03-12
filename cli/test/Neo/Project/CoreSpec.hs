module Neo.Project.CoreSpec (spec) where

import Array qualified
import Core
import Neo.Project.Core
import Test

spec :: Spec Unit
spec = do
  describe "Neo.Project.Core" do
    describe "initialState" do
      it "has empty status" \_ -> do
        initialState.status |> shouldBe ""

      it "has empty name" \_ -> do
        initialState.name |> shouldBe ""

      it "has empty path" \_ -> do
        initialState.path |> shouldBe ""

      it "has empty filesCreated array" \_ -> do
        initialState.filesCreated |> Array.length |> shouldBe 0

    describe "update" do
      describe "ProjectInitRequested" do
        it "sets status to initializing" \_ -> do
          let event = ProjectInitRequested
                (ProjectInitRequestedEvent
                  { entityId = def,
                    name = "my-project",
                    path = "/home/user/my-project"
                  })
          let updated = update event initialState
          updated.status |> shouldBe "initializing"

        it "sets name from event" \_ -> do
          let event = ProjectInitRequested
                (ProjectInitRequestedEvent
                  { entityId = def,
                    name = "test-project",
                    path = "/tmp/test"
                  })
          let updated = update event initialState
          updated.name |> shouldBe "test-project"

        it "sets path from event" \_ -> do
          let event = ProjectInitRequested
                (ProjectInitRequestedEvent
                  { entityId = def,
                    name = "test-project",
                    path = "/tmp/test"
                  })
          let updated = update event initialState
          updated.path |> shouldBe "/tmp/test"

      describe "ProjectFileCreated" do
        it "appends file path to filesCreated" \_ -> do
          let state = initialState { status = "initializing" }
          let event = ProjectFileCreated
                (ProjectFileCreatedEvent
                  { entityId = def,
                    filePath = "package.json"
                  })
          let updated = update event state
          updated.filesCreated |> Array.length |> shouldBe 1

        it "preserves existing files when adding new one" \_ -> do
          let state = initialState { filesCreated = Array.empty |> Array.push "file1.txt" }
          let event = ProjectFileCreated
                (ProjectFileCreatedEvent
                  { entityId = def,
                    filePath = "file2.txt"
                  })
          let updated = update event state
          updated.filesCreated |> Array.length |> shouldBe 2

      describe "GitInitialized" do
        it "sets status to git-initialized" \_ -> do
          let state = initialState { status = "initializing" }
          let event = GitInitialized (GitInitializedEvent { entityId = def })
          let updated = update event state
          updated.status |> shouldBe "git-initialized"

      describe "ProjectInitCompleted" do
        it "sets status to completed" \_ -> do
          let state = initialState { status = "git-initialized" }
          let event = ProjectInitCompleted (ProjectInitCompletedEvent { entityId = def })
          let updated = update event state
          updated.status |> shouldBe "completed"

      describe "ProjectInitFailed" do
        it "sets status to failed" \_ -> do
          let state = initialState { status = "initializing" }
          let event = ProjectInitFailed
                (ProjectInitFailedEvent
                  { entityId = def,
                    reason = "Permission denied"
                  })
          let updated = update event state
          updated.status |> shouldBe "failed"
