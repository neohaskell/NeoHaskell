{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for video audio extraction.
--
-- This module contains Nick's code - the 'ToAction' instance, ffmpeg
-- execution, and temp file handling.
--
-- The process boundary is an explicit 'Runner' record. The 'ToAction'
-- instance uses 'realRunner'; unit tests pass a fake and boundary tests run ffmpeg.
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
import Bytes qualified
import Char (Char)
import File qualified
import Integration qualified
import Integration.Video.ExtractAudio (Config (..), ExtractionResult (..), Request (..))
import Json qualified
import LinkedList qualified
import Maybe (Maybe (..))
import Maybe qualified
import Path (Path)
import Path qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.FileUpload.Core (FileAccessError (..), FileRef)
import Subprocess qualified
import System.Directory qualified as GhcDir
import System.IO.Temp qualified as GhcTemp
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


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
      Task.throw (Integration.ValidationError "ffmpeg not found. Please install ffmpeg (apt-get install ffmpeg, brew install ffmpeg) or set ffmpegPath.")
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
  withWorkspace \workspace ->
    convertToWav runner ffmpeg config workspace videoBytes


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
    Task.throw (Integration.PermanentFailure (conversionFailure completion.stderr))
  audio <- File.readBytes workspace.outputPath
    |> Task.mapError (\_ -> Integration.PermanentFailure "ffmpeg produced no audio output")
  boundedAudio <- validateAndBoundWav config audio
  Task.yield ExtractionResult {audio = boundedAudio, mimeType = "audio/wav"}


-- | Validate the complete container before selecting whole PCM frames.
validateAndBoundWav :: Config -> Bytes -> Task Integration.IntegrationError Bytes
validateAndBoundWav config audio = do
  requireWav (Bytes.length audio >= 12)
  requireWav (slice 0 4 audio == Text.toBytes "RIFF" && slice 8 4 audio == Text.toBytes "WAVE")
  requireWav (readLittleEndian (slice 4 4 audio) == Bytes.length audio - 8)
  (format, samples) <- readWavChunks audio 12 Nothing Nothing
  frameBytes <- validatePcmFormat config format
  let sampleBytes = Bytes.length samples
  requireWav (sampleBytes > 0 && modBy frameBytes sampleBytes == 0)
  let headerBytes = 28 + Bytes.length format
  requireWav (config.maxOutputBytes >= headerBytes)
  let available = config.maxOutputBytes - headerBytes
  requireWav (available >= frameBytes)
  if Bytes.length audio <= config.maxOutputBytes
    then Task.yield audio
    else do
      let retained = min sampleBytes ((available // frameBytes) * frameBytes)
      Task.yield (assembleWav format (Bytes.take retained samples))


-- ffmpeg emits PCM16, including WAVE_FORMAT_EXTENSIBLE for multichannel audio.
-- | Check frame representation against the requested channel count and rate.
validatePcmFormat :: Config -> Bytes -> Task Integration.IntegrationError Int
validatePcmFormat config format = do
  requireWav (Bytes.length format >= 16 && modBy 2 (Bytes.length format) == 0)
  Task.when (Bytes.length format > 16) do
    requireWav (readLittleEndian (slice 16 2 format) == Bytes.length format - 18)
  let channels = readLittleEndian (slice 2 2 format)
  let rate = readLittleEndian (slice 4 4 format)
  let frameBytes = readLittleEndian (slice 12 2 format)
  requireWav (channels > 0 && channels == config.channels && rate > 0 && rate == config.sampleRateHz)
  requireWav (readLittleEndian (slice 14 2 format) == 16 && frameBytes == 2 * channels)
  requireWav (readLittleEndian (slice 8 4 format) == rate * frameBytes)
  let formatTag = readLittleEndian (slice 0 2 format)
  requireWav (formatTag == 1 || isExtendedPcm16 formatTag format)
  Task.yield frameBytes


-- | Recognize PCM16 extensible format without accepting other sample encodings.
isExtendedPcm16 :: Int -> Bytes -> Bool
isExtendedPcm16 formatTag format =
  formatTag == 65534 && Bytes.length format >= 40
    && readLittleEndian (slice 16 2 format) >= 22
    && readLittleEndian (slice 18 2 format) == 16
    && slice 24 16 format == Bytes.pack [1,0,0,0,0,0,16,0,128,0,0,170,0,56,155,113]


-- | Accept optional chunks only when their padded lengths fit the container.
readWavChunks ::
  Bytes -> Int -> Maybe Bytes -> Maybe Bytes ->
  Task Integration.IntegrationError (Bytes, Bytes)
readWavChunks audio offset format samples = do
  if offset == Bytes.length audio
    then requireWavChunks format samples
    else do
      requireWav (Bytes.length audio - offset >= 8)
      let name = slice offset 4 audio
      let size = readLittleEndian (slice (offset + 4) 4 audio)
      let paddedSize = size + modBy 2 size
      requireWav (paddedSize <= Bytes.length audio - offset - 8)
      let payload = slice (offset + 8) size audio
      nextFormat <- selectWavChunk "fmt " name format payload
      nextSamples <- selectWavChunk "data" name samples payload
      readWavChunks audio (offset + 8 + paddedSize) nextFormat nextSamples


-- | Reject duplicate required chunks instead of choosing an ambiguous payload.
selectWavChunk :: Text -> Bytes -> Maybe Bytes -> Bytes -> Task Integration.IntegrationError (Maybe Bytes)
selectWavChunk expected name previous payload = do
  if name == Text.toBytes expected
    then do
      case previous of
        Nothing -> Task.yield (Just payload)
        Just _ -> Task.throw (Integration.PermanentFailure "ffmpeg produced invalid or empty audio")
    else Task.yield previous


-- | Require both the format description and the complete sample payload.
requireWavChunks :: Maybe Bytes -> Maybe Bytes -> Task Integration.IntegrationError (Bytes, Bytes)
requireWavChunks format samples = case (format, samples) of
  (Just actualFormat, Just actualSamples) -> Task.yield (actualFormat, actualSamples)
  _ -> Task.throw (Integration.PermanentFailure "ffmpeg produced invalid or empty audio")


-- | Map unusable output to one fixed callback-safe error.
requireWav :: Bool -> Task Integration.IntegrationError Unit
requireWav valid =
  Task.unless valid (Task.throw (Integration.PermanentFailure "ffmpeg produced invalid or empty audio"))


-- | Read a bounded byte range after its surrounding chunk has been validated.
slice :: Int -> Int -> Bytes -> Bytes
slice offset count bytes = bytes |> Bytes.drop offset |> Bytes.take count


-- | Decode a validated RIFF integer field into the native size representation.
readLittleEndian :: Bytes -> Int
readLittleEndian bytes =
  bytes |> Bytes.unpack |> LinkedList.foldr (\byte total -> fromIntegral byte + 256 * total) 0


-- | Encode a nonnegative RIFF size field at its fixed width.
littleEndian :: Int -> Int -> Bytes
littleEndian width value =
  if width == 0
    then Bytes.empty
    else Bytes.append (Bytes.pack [fromIntegral (modBy 256 value)]) (littleEndian (width - 1) (value // 256))


-- | Finalize the retained format and sample payload with consistent RIFF sizes.
assembleWav :: Bytes -> Bytes -> Bytes
assembleWav format samples = do
  let dataSize = Bytes.length samples
  let formatSize = Bytes.length format
  Bytes.concat
    [ Text.toBytes "RIFF", littleEndian 4 (20 + formatSize + dataSize)
    , Text.toBytes "WAVEfmt ", littleEndian 4 formatSize, format
    , Text.toBytes "data", littleEndian 4 dataSize, samples
    ]


-- | Classify diagnostics without copying any subprocess-controlled text.
conversionFailure :: Text -> Text
conversionFailure diagnostic =
  if Text.contains "Invalid data found" diagnostic
    then "ffmpeg failed: Invalid data found"
    else "ffmpeg failed: unable to extract audio"


-- | Build the ffmpeg command arguments.
--
-- Drops the video stream (@-vn@), converts to the configured channel count
-- and sample rate, and stops writing at the configured size (@-fs@).
-- @-map_metadata -1@ keeps source tags out of the WAV: ffmpeg would write them
-- as a LIST/INFO chunk that consumes the byte budget, and @-bitexact@ alone
-- does not drop them.
--
-- >>> import Array qualified
-- >>> import Integration.Video.ExtractAudio (defaultConfig)
-- >>> Array.toLinkedList (buildFfmpegArgs defaultConfig "in.mp4" "out.wav")
-- ["-nostdin","-v","error","-i","in.mp4","-vn","-ac","1","-ar","16000","-fs","25000000","-map_metadata","-1","-bitexact","out.wav"]
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
    , "-map_metadata", "-1"
    , "-bitexact"
    , Path.toText outputPath
    ]


-- | The native bracket masks acquisition until directory cleanup is registered.
-- POSIX owner-only permissions isolate other users, not arbitrary media references.
withWorkspace ::
  forall value.
  (Workspace -> Task Integration.IntegrationError value) ->
  Task Integration.IntegrationError value
withWorkspace use = do
  let scoped :: Task Integration.IntegrationError (Result Integration.IntegrationError value)
      scoped = do
        parent <- GhcDir.getTemporaryDirectory |> Task.fromIO
        GhcTemp.withTempDirectory parent "neohaskell-video-" (\directory -> do
          let extraction = do
                workspace <- workspaceFromDirectory directory
                use workspace
          Task.runResult extraction)
          |> Task.fromIO
  outcome <- scoped |> Task.asResultSafe
  case outcome of
    Err _ -> Task.throw (Integration.UnexpectedError "Audio extraction workspace failed")
    Ok result -> case result of
      Err err -> Task.throw err
      Ok value -> Task.yield value


-- | Place fixed input and output filenames inside the acquired private directory.
workspaceFromDirectory :: LinkedList.LinkedList Char -> Task Integration.IntegrationError Workspace
workspaceFromDirectory directory = do
  workDir <- case Path.fromLinkedList directory of
    Nothing -> Task.throw (Integration.UnexpectedError "Invalid temp directory path")
    Just path -> Task.yield path
  inputPath <- tempFilePath workDir "input"
  outputPath <- tempFilePath workDir "output.wav"
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
  Subprocess.ProcessError _ ->
    Integration.UnexpectedError "Process error: unable to run audio extraction"
  Subprocess.TimeoutError _ ->
    Integration.PermanentFailure "Audio extraction timed out. The video may be too long."
  Subprocess.ToolNotFound _ ->
    Integration.ValidationError "Tool not found: ffmpeg. Please install ffmpeg or set ffmpegPath."


-- | Convert IntegrationError to Text for the onError callback.
integrationErrorToText :: Integration.IntegrationError -> Text
integrationErrorToText err = case err of
  Integration.ValidationError msg -> msg
  Integration.AuthenticationError msg -> msg
  Integration.NetworkError msg -> msg
  Integration.RateLimited _ -> "Rate limited"
  Integration.PermanentFailure msg -> msg
  Integration.UnexpectedError msg -> msg
