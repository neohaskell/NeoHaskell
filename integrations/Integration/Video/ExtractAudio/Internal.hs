{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for video audio extraction.
--
-- This module contains Nick's code - the 'ToAction' instance, ffmpeg
-- execution, and temp file handling.
--
-- The process boundary is an explicit 'Runner' record. The 'ToAction'
-- instance uses 'realRunner'; tests pass a fake, so they never need ffmpeg.
--
-- __This module is not exported to Jess.__
module Integration.Video.ExtractAudio.Internal
  ( -- * Process boundary
    Runner (..)
  , realRunner

    -- * For Testing Only
  , executeExtraction
  , extractAudio
  , resolveFfmpeg
  , buildFfmpegArgs
  ) where

import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import File qualified
import Integration qualified
import Integration.Video.ExtractAudio (Config (..), ExtractionResult (..), Request (..))
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Path (Path)
import Path qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.FileUpload.Core (FileAccessError (..), FileRef)
import Subprocess qualified
import System.Directory qualified as GhcDir
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Uuid qualified


-- | The process boundary: everything the extraction needs from the OS.
--
-- Production code uses 'realRunner'. Tests substitute a fake that records
-- its arguments and writes a fake WAV file.
data Runner = Runner
  { which :: Text -> Task Subprocess.Error (Maybe Text)
  -- ^ Find an executable; Nothing when it is not installed
  , run :: Int -> Text -> Array Text -> Path -> Task Subprocess.Error Subprocess.Completion
  -- ^ Run an executable with a timeout (seconds), arguments and working directory
  }


-- | The runner that talks to the real operating system.
realRunner :: Runner
realRunner = Runner
  { which = Subprocess.which
  , run = Subprocess.runWithTimeout
  }


-- | The temp files of one extraction, all in one directory.
data Workspace = Workspace
  { workDir :: Path
  , inputPath :: Path
  , outputPath :: Path
  }


-- | ToAction instance that executes the audio extraction.
--
-- This is the main entry point - when Jess writes:
--
-- @
-- Integration.outbound VideoAudio.Request { ... }
-- @
--
-- This instance converts the config into an executable action.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction request = Integration.action \ctx -> do
    executeExtraction realRunner ctx request


-- | Execute an audio extraction request.
--
-- Setup problems (uploads disabled, ffmpeg not installed) fail the action.
-- Problems with the video itself reach Jess through 'onError'.
executeExtraction ::
  forall command.
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Runner ->
  Integration.ActionContext ->
  Request command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeExtraction runner ctx request = do
  fileAccess <- requireFileAccess ctx
  ffmpeg <- resolveFfmpeg runner request.config
  outcome <- extractAudio runner ffmpeg fileAccess request.fileRef request.config
    |> Task.asResult
  case outcome of
    Ok result ->
      Integration.emitCommand (request.onSuccess result)
    Err integrationError -> do
      let errorText = integrationErrorToText integrationError
      Integration.emitCommand (request.onError errorText)


-- | File uploads must be enabled, or there is nothing to read the video from.
requireFileAccess ::
  Integration.ActionContext ->
  Task Integration.IntegrationError Integration.FileAccessContext
requireFileAccess ctx = case ctx.fileAccess of
  Nothing ->
    Task.throw (Integration.ValidationError "File uploads not enabled. Cannot access video file.")
  Just fileAccess ->
    Task.yield fileAccess


-- | Find the ffmpeg executable, or fail with install instructions.
resolveFfmpeg :: Runner -> Config -> Task Integration.IntegrationError Text
resolveFfmpeg runner config = do
  let requested = config.ffmpegPath |> Maybe.withDefault "ffmpeg"
  found <- runner.which requested
    |> Task.mapError subprocessToIntegrationError
  case found of
    Nothing ->
      Task.throw (Integration.ValidationError [fmt|ffmpeg not found (looked for #{requested}). Please install ffmpeg (apt-get install ffmpeg, brew install ffmpeg) or set ffmpegPath.|])
    Just resolved ->
      Task.yield resolved


-- | Fetch the video, run ffmpeg over it, and return the WAV audio.
--
-- Both temp files are removed whether the run succeeds or fails.
extractAudio ::
  Runner ->
  Text ->
  Integration.FileAccessContext ->
  FileRef ->
  Config ->
  Task Integration.IntegrationError ExtractionResult
extractAudio runner ffmpeg fileAccess fileRef config = do
  videoBytes <- fileAccess.retrieveFile fileRef
    |> Task.mapError fileAccessToIntegrationError
  workspace <- newWorkspace
  let cleanup = do
        File.deleteIfExists workspace.inputPath |> Task.ignoreError
        File.deleteIfExists workspace.outputPath |> Task.ignoreError
  convertToWav runner ffmpeg config workspace videoBytes
    |> Task.finally cleanup


-- | Write the video, run ffmpeg over it, and read the WAV it produced.
convertToWav ::
  Runner ->
  Text ->
  Config ->
  Workspace ->
  Bytes ->
  Task Integration.IntegrationError ExtractionResult
convertToWav runner ffmpeg config workspace videoBytes = do
  File.writeBytes workspace.inputPath videoBytes
    |> Task.mapError (\_ -> Integration.UnexpectedError "Failed to write temp video file")
  let args = buildFfmpegArgs config workspace.inputPath workspace.outputPath
  completion <- runner.run config.timeoutSeconds ffmpeg args workspace.workDir
    |> Task.mapError subprocessToIntegrationError
  Task.unless (completion.exitCode == 0) do
    let stderrText = Text.trim completion.stderr
    Task.throw (Integration.PermanentFailure [fmt|ffmpeg failed: #{stderrText}|])
  audio <- File.readBytes workspace.outputPath
    |> Task.mapError (\_ -> Integration.PermanentFailure "ffmpeg produced no audio output")
  Task.yield ExtractionResult {audio, mimeType = "audio/wav"}


-- | Build the ffmpeg command arguments.
--
-- Drops the video stream (@-vn@), converts to the configured channel count
-- and sample rate, and stops writing at the configured size (@-fs@).
--
-- >>> import Array qualified
-- >>> import Integration.Video.ExtractAudio (defaultConfig)
-- >>> Array.toLinkedList (buildFfmpegArgs defaultConfig "in.mp4" "out.wav")
-- ["-nostdin","-v","error","-i","in.mp4","-vn","-ac","1","-ar","16000","-fs","25000000","out.wav"]
buildFfmpegArgs :: Config -> Path -> Path -> Array Text
buildFfmpegArgs config inputPath outputPath = do
  let channelsText = Text.fromInt config.channels
  let sampleRateText = Text.fromInt config.sampleRateHz
  let maxBytesText = Text.fromInt config.maxOutputBytes
  Array.fromLinkedList
    [ "-nostdin"
    , "-v", "error"
    , "-i", Path.toText inputPath
    , "-vn"
    , "-ac", channelsText
    , "-ar", sampleRateText
    , "-fs", maxBytesText
    , Path.toText outputPath
    ]


-- | Pick unique input and output temp paths for one extraction.
newWorkspace :: Task Integration.IntegrationError Workspace
newWorkspace = do
  tempDir <- GhcDir.getTemporaryDirectory
    |> Task.fromIO
  workDir <- case Path.fromLinkedList tempDir of
    Nothing -> Task.throw (Integration.UnexpectedError "Invalid temp directory path")
    Just dir -> Task.yield dir
  tempId <- Uuid.generate
  let tempIdText = Uuid.toText tempId
  inputPath <- tempFilePath workDir [fmt|neohaskell-video-#{tempIdText}.input|]
  outputPath <- tempFilePath workDir [fmt|neohaskell-video-#{tempIdText}.wav|]
  Task.yield Workspace {workDir, inputPath, outputPath}


-- | A file name inside the temp directory.
tempFilePath :: Path -> Text -> Task Integration.IntegrationError Path
tempFilePath workDir fileName = case Path.fromText fileName of
  Nothing -> Task.throw (Integration.UnexpectedError "Invalid temp file name")
  Just name -> Task.yield (Path.append name workDir)


-- | Convert FileAccessError to IntegrationError.
fileAccessToIntegrationError :: FileAccessError -> Integration.IntegrationError
fileAccessToIntegrationError err = case err of
  FileNotFound _ ->
    Integration.ValidationError "File not found"
  StateLookupFailed _ _ ->
    Integration.UnexpectedError "Audio extraction failed: state lookup error"
  NotOwner _ ->
    Integration.AuthenticationError "Not authorized to access this file"
  FileExpired _ ->
    Integration.ValidationError "File has expired"
  FileIsDeleted _ ->
    Integration.ValidationError "File has been deleted"
  BlobMissing _ ->
    Integration.UnexpectedError "File blob is missing from storage"
  StorageError _ ->
    Integration.UnexpectedError "Audio extraction failed: storage error"


-- | Convert Subprocess.Error to IntegrationError.
subprocessToIntegrationError :: Subprocess.Error -> Integration.IntegrationError
subprocessToIntegrationError err = case err of
  Subprocess.ProcessError msg ->
    Integration.UnexpectedError [fmt|Process error: #{msg}|]
  Subprocess.TimeoutError msg ->
    Integration.PermanentFailure [fmt|Audio extraction timed out (#{msg}). The video may be too long.|]
  Subprocess.ToolNotFound msg ->
    Integration.ValidationError [fmt|Tool not found: #{msg}|]


-- | Convert IntegrationError to Text for the onError callback.
integrationErrorToText :: Integration.IntegrationError -> Text
integrationErrorToText err = case err of
  Integration.ValidationError msg -> msg
  Integration.AuthenticationError msg -> msg
  Integration.NetworkError msg -> msg
  Integration.RateLimited _ -> "Rate limited"
  Integration.PermanentFailure msg -> msg
  Integration.UnexpectedError msg -> msg
