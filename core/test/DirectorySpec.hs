-- | Checks private temporary folders and cleanup across task outcomes.
-- Run in the core suite when changing Directory.withTemporaryFolder.
module DirectorySpec (spec) where

import AsyncTask qualified
import Array qualified
import Basics
import Channel (Channel)
import Channel qualified
import Data.Bits qualified as Bits
import Directory qualified
import Maybe (Maybe (..))
import "nhcore" Path qualified
import Prelude qualified as GhcPrelude
import Result (Result (..))
import System.Directory qualified as GhcDirectory
import System.FilePath qualified as GhcFilePath
import System.Posix.Files qualified as PosixFiles
import Task (Task)
import Task qualified
import Test
import Text (Text)
import Text qualified
import ToText (toText)
import Var qualified


spec :: Spec Unit
spec = do
  describe "withTemporaryFolder" do
    it "creates a private folder and removes it after success" \_ -> do
      folder <- Directory.withTemporaryFolder toText \folder -> do
        GhcFilePath.isAbsolute (Path.toLinkedList folder) |> shouldBe True
        exists <- folderExists folder
        exists |> shouldBe True
        mode <-
          PosixFiles.getFileStatus (Path.toLinkedList folder)
            |> GhcPrelude.fmap PosixFiles.fileMode
            |> Task.fromIO
        (mode Bits..&. 0o077) |> shouldBe 0
        Task.yield folder
      assertRemoved folder

    it "removes the folder after a typed failure" \_ -> do
      captured <- Var.new Nothing
      outcome <-
        Directory.withTemporaryFolder toText (\folder -> do
          Var.set (Just folder) captured
          Task.throw ("expected failure" :: Text)
        )
          |> Task.asResult
      case outcome of
        Err message -> message |> shouldBe "expected failure"
        Ok _ -> Test.fail "expected a typed failure"
      folder <- capturedFolder captured
      assertRemoved folder

    it "removes the folder after a synchronous exception" \_ -> do
      captured <- Var.new Nothing
      outcome <-
        Directory.withTemporaryFolder toText (\folder -> do
          Var.set (Just folder) captured
          Task.fromIO (GhcPrelude.ioError (GhcPrelude.userError "runtime failure")) :: Task Text Unit
        )
          |> Task.asResultSafe @Text
      case outcome of
        Err message -> message |> shouldSatisfy (Text.contains "runtime failure")
        Ok _ -> Test.fail "expected the runtime exception"
      folder <- capturedFolder captured
      assertRemoved folder

    it "removes the folder when the callback throws while building its task" \_ -> do
      outcome <-
        Directory.withTemporaryFolder toText (\folder -> GhcPrelude.error (Path.toLinkedList folder))
          |> Task.asResultSafe @Text
      case outcome of
        Err message ->
          case Array.first (Text.lines message) of
            Just pathText ->
              case Path.fromText pathText of
                Just folder -> assertRemoved folder
                Nothing -> Test.fail "the callback error did not contain a folder path"
            Nothing -> Test.fail "the callback error was empty"
        Ok _ -> Test.fail "expected the runtime exception"

    it "removes the folder after cancellation" \_ -> do
      ready <- Channel.new
      never <- Channel.newBounded 1 :: Task Text (Channel Unit)
      let work folder = do
            Channel.write folder ready
            Channel.read never
      handle <- AsyncTask.run (Directory.withTemporaryFolder toText work)
      folder <- Channel.read ready
      AsyncTask.cancel handle
      outcome <- AsyncTask.waitOutcome handle
      case outcome of
        AsyncTask.Interrupted -> Task.yield unit
        _ -> Test.fail "expected cancellation"
      assertRemoved folder

    it "reserves a different path for each live folder" \_ -> do
      Directory.withTemporaryFolder toText \first -> do
        Directory.withTemporaryFolder toText \second -> do
          first |> shouldSatisfy (\path -> path GhcPrelude./= second)
          firstExists <- folderExists first
          secondExists <- folderExists second
          firstExists |> shouldBe True
          secondExists |> shouldBe True

folderExists :: Path.Path -> Task err Bool
folderExists folder =
  GhcDirectory.doesDirectoryExist (Path.toLinkedList folder)
    |> Task.fromIO


assertRemoved :: Path.Path -> Task Text Unit
assertRemoved folder = do
  exists <-
    GhcDirectory.doesPathExist (Path.toLinkedList folder)
      |> Task.fromIO
  exists |> shouldBe False


capturedFolder :: Var.Var (Maybe Path.Path) -> Task Text Path.Path
capturedFolder captured = do
  maybeFolder <- Var.get captured
  case maybeFolder of
    Just folder -> Task.yield folder
    Nothing -> Task.throw "the scoped callback did not receive a folder"
