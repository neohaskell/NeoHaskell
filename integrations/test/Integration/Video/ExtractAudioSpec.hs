module Integration.Video.ExtractAudioSpec (spec) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Basics
import Bytes (Bytes)
import Bytes qualified
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import DateTime qualified
import File qualified
import Integration (ActionContext (..), CommandPayload (..))
import Integration (FileAccessContext (..))
import Integration qualified
import Integration.Video.ExtractAudio (Config (..), ExtractionResult (..), Request (..))
import Integration.Video.ExtractAudio qualified as VideoAudio
import Integration.Video.ExtractAudio.Internal (Runner (..), executeExtraction)
import Json qualified
import Lock (Lock)
import Map qualified
import Maybe (Maybe (..))
import Path qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.Event (Event (..), StreamPosition (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.InMemory qualified as InMemory
import Service.FileUpload.Core (FileAccessError (..), FileRef (..))
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Transport (EndpointHandler)
import Subprocess qualified
import Task (Task)
import Task qualified
import Test.Hspec
import Text (Text)
import Text qualified
import Uuid qualified
import Var (Var)
import Var qualified


spec :: Spec
spec = do
  describe "Integration.Video.ExtractAudio.defaultConfig" do
    it "defaults to 180 s, 16000 Hz, mono, 25 MB and ffmpeg from the PATH" do
      VideoAudio.defaultConfig `shouldBe` Config
        { timeoutSeconds = 180
        , sampleRateHz = 16000
        , channels = 1
        , maxOutputBytes = 25000000
        , ffmpegPath = Nothing
        }

  describe "Integration.Video.ExtractAudio.executeExtraction" do
    it "fails with an install hint when ffmpeg is missing" do
      (outcome, calls) <- Task.runOrPanic (runScenario missingFfmpeg VideoAudio.defaultConfig)
      case outcome of
        Err (Integration.ValidationError message) -> do
          Text.contains "ffmpeg" message `shouldBe` True
          Text.contains "install" message `shouldBe` True
        Err other ->
          expectationFailure [fmt|expected a ValidationError, got #{other}|]
        Ok _ ->
          expectationFailure "expected an error when ffmpeg is missing"
      Array.length calls `shouldBe` 0

    it "reports the ffmpeg failure to onError and removes the temp files on a non-zero exit" do
      (outcome, calls) <- Task.runOrPanic (runScenario (failingFfmpeg "Invalid data found") VideoAudio.defaultConfig)
      case outcome of
        Ok (Just payload) -> do
          let encoded = Json.encodeText payload.commandData
          Text.contains "ExtractionFailed" encoded `shouldBe` True
          Text.contains "Invalid data found" encoded `shouldBe` True
        _ ->
          expectationFailure "expected the onError command"
      Array.length calls `shouldBe` 1
      Task.runOrPanic (anyTempFileLeft calls) `shouldReturn` False

    it "returns the WAV bytes and audio/wav on success and removes the temp files" do
      (outcome, calls) <- Task.runOrPanic (runScenario succeedingFfmpeg VideoAudio.defaultConfig)
      case outcome of
        Ok (Just payload) -> do
          let encoded = Json.encodeText payload.commandData
          Text.contains "AudioExtracted" encoded `shouldBe` True
          Text.contains "audio/wav" encoded `shouldBe` True
          decodedAudio payload `shouldBe` Ok fakeWavBytes
        _ ->
          expectationFailure "expected the onSuccess command"
      Array.length calls `shouldBe` 1
      Task.runOrPanic (anyTempFileLeft calls) `shouldReturn` False

    it "writes the video to the input file before running ffmpeg" do
      (_, calls) <- Task.runOrPanic (runScenario succeedingFfmpeg VideoAudio.defaultConfig)
      let inputPresent = calls |> Array.map (\call -> call.callInputPresent)
      inputPresent `shouldBe` Array.wrap True
      let inputBytes = calls |> Array.map (\call -> call.callInputBytes)
      inputBytes `shouldBe` Array.wrap (Just fakeVideoBytes)

    it "round-trips binary WAV bytes through the Base64 command field" do
      let encodedAudio = fakeWavBytes |> Bytes.toBase64 |> Text.fromBytes
      decodeAudio encodedAudio `shouldBe` Ok fakeWavBytes
      Bytes.length fakeWavBytes `shouldBe` 268

    it "passes -vn -ac 1 -ar 16000 -fs 25000000 and the default 180 s timeout" do
      (_, calls) <- Task.runOrPanic (runScenario succeedingFfmpeg VideoAudio.defaultConfig)
      case Array.get 0 calls of
        Nothing -> expectationFailure "expected one ffmpeg call"
        Just call -> do
          let joined = Text.joinWith " " call.callArguments
          Text.contains "-vn -ac 1 -ar 16000 -fs 25000000" joined `shouldBe` True
          Text.contains "-nostdin" joined `shouldBe` True
          call.callTimeout `shouldBe` 180
          call.callExecutable `shouldBe` "/usr/bin/ffmpeg"

    it "passes the configured timeout, sample rate, channels and size cap" do
      let custom = VideoAudio.defaultConfig
            { timeoutSeconds = 42
            , sampleRateHz = 8000
            , channels = 2
            , maxOutputBytes = 1000
            }
      (_, calls) <- Task.runOrPanic (runScenario succeedingFfmpeg custom)
      case Array.get 0 calls of
        Nothing -> expectationFailure "expected one ffmpeg call"
        Just call -> do
          let joined = Text.joinWith " " call.callArguments
          Text.contains "-vn -ac 2 -ar 8000 -fs 1000" joined `shouldBe` True
          call.callTimeout `shouldBe` 42

    it "looks up and runs the configured ffmpegPath" do
      let custom = VideoAudio.defaultConfig {ffmpegPath = Just "/opt/ffmpeg/bin/ffmpeg"}
      (_, calls) <- Task.runOrPanic (runScenario succeedingFfmpeg custom)
      case Array.get 0 calls of
        Nothing -> expectationFailure "expected one ffmpeg call"
        Just call ->
          call.callExecutable `shouldBe` "/opt/ffmpeg/bin/ffmpeg"

    it "fails when file uploads are not enabled" do
      callsVar <- Task.runOrPanic (textTask (Var.new Array.empty))
      let runner = fakeRunner callsVar succeedingFfmpeg
      ctx <- Task.runOrPanic (makeContext Nothing)
      outcome <- Task.runOrPanic (textTask (executeExtraction runner ctx (makeRequest VideoAudio.defaultConfig) |> Task.asResult))
      case outcome of
        Err (Integration.ValidationError message) ->
          Text.contains "File uploads not enabled" message `shouldBe` True
        _ ->
          expectationFailure "expected a ValidationError for disabled file uploads"

    it "maps a missing file to a validation error without leaking the file reference" do
      callsVar <- Task.runOrPanic (textTask (Var.new Array.empty))
      let runner = fakeRunner callsVar succeedingFfmpeg
      let missingFile = FileAccessContext
            { retrieveFile = \ref -> Task.throw (FileNotFound ref)
            , getFileMetadata = \ref -> Task.throw (FileNotFound ref)
            }
      ctx <- Task.runOrPanic (makeContext (Just missingFile))
      outcome <- Task.runOrPanic (textTask (executeExtraction runner ctx (makeRequest VideoAudio.defaultConfig) |> Task.asResult))
      case outcome of
        Ok (Just payload) -> do
          let encoded = Json.encodeText payload.commandData
          Text.contains "ExtractionFailed" encoded `shouldBe` True
          Text.contains "File not found" encoded `shouldBe` True
          Text.contains "00000000-0000-0000-0000-000000000001" encoded `shouldBe` False
        _ ->
          expectationFailure "expected the onError command"
      calls <- Task.runOrPanic (textTask (Var.get callsVar))
      Array.length calls `shouldBe` 0


  describe "Integration.Video.ExtractAudio dispatcher timeout interaction" do
    it "delivers onSuccess past a short deadline when the dispatcher timeout is raised above the extraction" do
      let slowExtraction = succeedingFfmpeg {ffmpegDelayMs = slowExtractionMs}
      (commands, calls) <- Task.runOrPanic (runThroughDispatcher slowExtraction (Just raisedDispatcherTimeoutMs))
      case commands |> Array.get 0 of
        Just (AudioExtracted {audioBase64, mime}) -> do
          decodeAudio audioBase64 `shouldBe` Ok fakeWavBytes
          mime `shouldBe` "audio/wav"
        _ ->
          expectationFailure [fmt|expected the onSuccess command, got #{commands}|]
      Array.length commands `shouldBe` 1
      Array.length calls `shouldBe` 1

    it "cancels the extraction before onSuccess when the dispatcher timeout is shorter" do
      let slowExtraction = succeedingFfmpeg {ffmpegDelayMs = slowExtractionMs}
      (commands, calls) <- Task.runOrPanic (runThroughDispatcher slowExtraction (Just shortDispatcherTimeoutMs))
      commands `shouldBe` Array.empty
      Array.length calls `shouldBe` 1

    it "reaches onError with the extraction timeout when the dispatcher timeout is longer" do
      let timingOut = succeedingFfmpeg {ffmpegDelayMs = slowExtractionMs, ffmpegTimesOut = True}
      (commands, calls) <- Task.runOrPanic (runThroughDispatcher timingOut (Just raisedDispatcherTimeoutMs))
      case commands |> Array.get 0 of
        Just (ExtractionFailed {reason}) ->
          Text.contains "timed out" reason `shouldBe` True
        _ ->
          expectationFailure [fmt|expected the onError command, got #{commands}|]
      Array.length commands `shouldBe` 1
      Array.length calls `shouldBe` 1

    it "never reaches onError when the dispatcher timeout expires before the extraction timeout" do
      let timingOut = succeedingFfmpeg {ffmpegDelayMs = slowExtractionMs, ffmpegTimesOut = True}
      (commands, calls) <- Task.runOrPanic (runThroughDispatcher timingOut (Just shortDispatcherTimeoutMs))
      commands `shouldBe` Array.empty
      Array.length calls `shouldBe` 1


-- | How the fake ffmpeg behaves.
data Scenario = Scenario
  { ffmpegInstalled :: Bool
  , ffmpegExitCode :: Int
  , ffmpegStderr :: Text
  , ffmpegDelayMs :: Int
  -- ^ Wall-clock time the fake run takes, standing in for a slow video
  , ffmpegTimesOut :: Bool
  -- ^ After the delay, fail as 'Subprocess.runWithTimeout' does when ffmpeg hits 'timeoutSeconds'
  }


-- | One recorded invocation of the fake runner.
data Call = Call
  { callTimeout :: Int
  , callExecutable :: Text
  , callArguments :: Array Text
  , callInputPresent :: Bool
  , callInputBytes :: Maybe Bytes
  }
  deriving (Eq, Show)


missingFfmpeg :: Scenario
missingFfmpeg = succeedingFfmpeg {ffmpegInstalled = False}


failingFfmpeg :: Text -> Scenario
failingFfmpeg stderrText = succeedingFfmpeg {ffmpegExitCode = 1, ffmpegStderr = stderrText}


succeedingFfmpeg :: Scenario
succeedingFfmpeg = Scenario
  { ffmpegInstalled = True
  , ffmpegExitCode = 0
  , ffmpegStderr = ""
  , ffmpegDelayMs = 0
  , ffmpegTimesOut = False
  }


-- | An MP4-style header followed by every byte value, so the fixture holds
-- NUL bytes and bytes that are not valid UTF-8.
fakeVideoBytes :: Bytes
fakeVideoBytes =
  Bytes.append
    (Bytes.pack [0x00, 0x00, 0x00, 0x18, 0x66, 0x74, 0x79, 0x70, 0x69, 0x73, 0x6F, 0x6D])
    (Bytes.pack [0 .. 255])


-- | A RIFF/WAVE header followed by every byte value in descending order, so
-- the audio cannot survive a UTF-8 text round trip.
fakeWavBytes :: Bytes
fakeWavBytes =
  Bytes.append
    (Bytes.pack [0x52, 0x49, 0x46, 0x46, 0x24, 0x00, 0x00, 0x00, 0x57, 0x41, 0x56, 0x45])
    (Bytes.pack [255, 254 .. 0])


-- | A runner that never starts a process. It records the bytes of the input
-- file it was given; after 'ffmpegDelayMs' it either fails with a timeout or,
-- on exit code 0, writes 'fakeWavBytes' to the output path, like ffmpeg would.
fakeRunner :: Var (Array Call) -> Scenario -> Runner
fakeRunner callsVar scenario = Runner
  { which = \name ->
      if not scenario.ffmpegInstalled
        then Task.yield Nothing
        else if Text.startsWith "/" name
          then Task.yield (Just name)
          else Task.yield (Just [fmt|/usr/bin/#{name}|])
  , run = \timeout executable arguments _ -> do
      inputPresent <- case Array.get 4 arguments |> andThenPath of
        Nothing -> Task.yield False
        Just inputPath ->
          File.exists inputPath
            |> Task.mapError (\_ -> Subprocess.ProcessError "fake exists check failed")
      inputBytes <- readInputBytes arguments
      let call = Call
            { callTimeout = timeout
            , callExecutable = executable
            , callArguments = arguments
            , callInputPresent = inputPresent
            , callInputBytes = inputBytes
            }
      recorded <- Var.get callsVar
      Var.set (Array.push call recorded) callsVar
      AsyncTask.sleep scenario.ffmpegDelayMs
      Task.when scenario.ffmpegTimesOut do
        Task.throw (Subprocess.TimeoutError [fmt|ffmpeg exceeded #{timeout} seconds|])
      Task.when (scenario.ffmpegExitCode == 0) do
        case Array.last arguments |> andThenPath of
          Nothing -> Task.throw (Subprocess.ProcessError "fake runner: no output path")
          Just outputPath ->
            File.writeBytes outputPath fakeWavBytes
              |> Task.mapError (\_ -> Subprocess.ProcessError "fake runner: write failed")
      Task.yield Subprocess.Completion
        { exitCode = scenario.ffmpegExitCode
        , stdout = ""
        , stderr = scenario.ffmpegStderr
        }
  }


-- | The bytes ffmpeg would read from its @-i@ input file; Nothing when unreadable.
readInputBytes :: Array Text -> Task Subprocess.Error (Maybe Bytes)
readInputBytes arguments = case Array.get 4 arguments |> andThenPath of
  Nothing -> Task.yield Nothing
  Just inputPath ->
    File.readBytes inputPath
      |> Task.map Just
      |> Task.recover (\_ -> Task.yield Nothing)


andThenPath :: Maybe Text -> Maybe Path.Path
andThenPath maybeText = case maybeText of
  Nothing -> Nothing
  Just text -> Path.fromText text


-- | The input and output temp paths ffmpeg was given.
tempPathsOf :: Call -> Array Path.Path
tempPathsOf call = do
  let inputPath = Array.get 4 call.callArguments |> andThenPath
  let outputPath = Array.last call.callArguments |> andThenPath
  case (inputPath, outputPath) of
    (Just input, Just output) -> Array.fromLinkedList [input, output]
    _ -> Array.empty


-- | Pins the error type of a helper task to 'Text' so 'Task.runOrPanic' can show it.
textTask :: Task Text value -> Task Text value
textTask task = task


anyTempFileLeft :: Array Call -> Task Text Bool
anyTempFileLeft calls = do
  flags <- calls
    |> Array.flatMap tempPathsOf
    |> Task.mapArray (\path -> File.exists path |> Task.recover (\_ -> Task.yield False))
  Task.yield (Array.any (\present -> present) flags)


makeContext :: Maybe Integration.FileAccessContext -> Task Text ActionContext
makeContext fileAccess = do
  stubStore <- InMemorySecretStore.new
  emptyLocks <- ConcurrentMap.new :: Task Text (ConcurrentMap Text Lock)
  Task.yield ActionContext
    { secretStore = stubStore
    , providerRegistry = Integration.fromMap Map.empty
    , refreshLocks = emptyLocks
    , fileAccess = fileAccess
    , outboundDispatch = DispatchRegistry.empty
    }


runScenario ::
  Scenario ->
  Config ->
  Task Text (Result Integration.IntegrationError (Maybe CommandPayload), Array Call)
runScenario scenario config = do
  callsVar <- Var.new Array.empty
  let runner = fakeRunner callsVar scenario
  ctx <- makeContext (Just videoAccess)
  outcome <- executeExtraction runner ctx (makeRequest config) |> Task.asResult
  calls <- Var.get callsVar
  Task.yield (outcome, calls)


-- | Uploads that always hand back 'fakeVideoBytes'.
videoAccess :: FileAccessContext
videoAccess = FileAccessContext
  { retrieveFile = \_ -> Task.yield fakeVideoBytes
  , getFileMetadata = \_ -> Task.throw (StorageError "metadata not available")
  }


makeRequest :: Config -> Request TestCommand
makeRequest config = Request
  { fileRef = FileRef "00000000-0000-0000-0000-000000000001"
  , config = config
  , onSuccess = \result -> AudioExtracted
      { audioBase64 = result.audio |> Bytes.toBase64 |> Text.fromBytes
      , mime = result.mimeType
      }
  , onError = \reason -> ExtractionFailed {reason}
  }


-- | The decoding step the module documentation tells Jess to use.
decodeAudio :: Text -> Result Text Bytes
decodeAudio audioBase64 =
  audioBase64
    |> Text.toBytes
    |> Bytes.fromBase64


-- | The audio carried by an emitted onSuccess command, decoded from JSON and Base64.
decodedAudio :: CommandPayload -> Result Text Bytes
decodedAudio payload =
  case Json.decode payload.commandData of
    Ok (AudioExtracted {audioBase64}) -> decodeAudio audioBase64
    Ok other -> Err [fmt|expected AudioExtracted, got #{other}|]
    Err decodeError -> Err decodeError


-- | Longer than 'shortDispatcherTimeoutMs', far shorter than 'raisedDispatcherTimeoutMs'.
slowExtractionMs :: Int
slowExtractionMs = 400


shortDispatcherTimeoutMs :: Int
shortDispatcherTimeoutMs = 150


raisedDispatcherTimeoutMs :: Int
raisedDispatcherTimeoutMs = 3000


-- | Longest wait for the dispatcher to deliver a command. A command that has
-- not arrived by then was cancelled; the extraction itself ends long before.
commandWaitMs :: Int
commandWaitMs = 1500


-- | Run one extraction the way an application does: a dispatcher worker
-- processes a video event under the given 'eventProcessingTimeoutMs' and
-- sends the emitted command to its endpoint. Returns the commands received
-- and the ffmpeg runs that started.
runThroughDispatcher :: Scenario -> Maybe Int -> Task Text (Array TestCommand, Array Call)
runThroughDispatcher scenario dispatcherTimeoutMs = do
  callsVar <- Var.new Array.empty
  received <- ConcurrentVar.containing Array.empty
  context <- makeContext (Just videoAccess)
  eventStore <- InMemory.new |> Task.mapError (\err -> [fmt|#{err}|])
  dispatcher <-
    Dispatcher.newWithLifecycleConfig
      (dispatcherConfig dispatcherTimeoutMs)
      eventStore
      (Array.wrap (extractionOutboundRunner (fakeRunner callsVar scenario)))
      Array.empty
      (Map.empty |> Map.set "TestCommand" (recordingEndpoint received))
      context
  event <- videoUploadedEvent
  Dispatcher.dispatch dispatcher event
  waitForCommand received commandWaitMs
  Dispatcher.shutdown dispatcher
  commands <- ConcurrentVar.peek received
  calls <- Var.get callsVar
  Task.yield (commands, calls)


-- | Dispatcher settings for a deterministic test: no reaper, no retries.
dispatcherConfig :: Maybe Int -> Dispatcher.DispatcherConfig
dispatcherConfig eventProcessingTimeoutMs =
  Dispatcher.defaultConfig
    { Dispatcher.enableReaper = False
    , Dispatcher.eventProcessingTimeoutMs = eventProcessingTimeoutMs
    , Dispatcher.maxEventRetries = 0
    }


-- | The outbound runner an application derives for 'Request', with the fake process boundary.
extractionOutboundRunner :: Runner -> Dispatcher.OutboundRunner
extractionOutboundRunner runner = Dispatcher.OutboundRunner
  { entityTypeName = "Lecture"
  , processEvent = \ctx _eventStore _event -> do
      emitted <- executeExtraction runner ctx (makeRequest VideoAudio.defaultConfig)
        |> Task.mapError (\err -> [fmt|#{err}|])
      case emitted of
        Just payload -> Task.yield (Array.wrap payload)
        Nothing -> Task.yield Array.empty
  }


-- | A command endpoint that decodes and keeps every command it receives.
recordingEndpoint :: ConcurrentVar (Array TestCommand) -> EndpointHandler
recordingEndpoint received _requestContext commandBytes _respond =
  case Json.decodeBytes commandBytes of
    Ok command -> received |> ConcurrentVar.modify (Array.push command)
    Err decodeError -> Task.throw [fmt|undecodable command: #{decodeError}|]


-- | Poll until a command arrives or the wait runs out.
waitForCommand :: ConcurrentVar (Array TestCommand) -> Int -> Task Text Unit
waitForCommand received remainingMs = do
  commands <- ConcurrentVar.peek received
  if Array.length commands > 0 || remainingMs <= 0
    then Task.yield unit
    else do
      AsyncTask.sleep 25
      waitForCommand received (remainingMs - 25)


videoUploadedEvent :: Task Text (Event Json.Value)
videoUploadedEvent = do
  now <- DateTime.now
  Task.yield Event
    { entityName = EntityName "Lecture"
    , streamId = StreamId.fromTextUnsafe "lecture-1"
    , event = Json.encode ("VideoUploaded" :: Text)
    , metadata = EventMetadata
        { eventId = Uuid.nil
        , relatedUserSub = Nothing
        , correlationId = Nothing
        , causationId = Nothing
        , createdAt = now
        , localPosition = Just (StreamPosition 1)
        , globalPosition = Just (StreamPosition 1)
        }
    }


-- | Minimal command type satisfying the ToAction constraints.
data TestCommand
  = AudioExtracted {audioBase64 :: Text, mime :: Text}
  | ExtractionFailed {reason :: Text}
  deriving (Eq, Show, Generic)


instance Json.ToJSON TestCommand


instance Json.FromJSON TestCommand


type instance NameOf TestCommand = "TestCommand"
