module Service.FileUpload.BlobStore.S3Spec where

import Array qualified
import AsyncTask qualified
import Bytes qualified
import ConcurrentVar qualified
import Core
import Http.Client qualified as Http
import Http.Client.Internal qualified as HttpInternal
import Json qualified
import Network.Socket qualified as GhcSocket
import Network.Wai.Handler.Warp qualified as GhcWarp
import Result qualified
import Service.Application qualified as Application
import Service.EventStore.InMemory qualified as InMemory
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreError (..))
import Service.FileUpload.BlobStore.S3 (S3Config (..), createBlobStore)
import Service.FileUpload.BlobStore.S3 qualified as S3
import Service.FileUpload.BlobStore.S3.FakeS3 (FakeS3 (..), Mode (..), withFakeS3)
import Service.FileUpload.BlobStore.S3.FakeS3 qualified as FakeS3
import Service.FileUpload.Core (BlobKey (..), FileUploadConfig (..), FileStateStoreBackend (..))
import Service.Transport.Web qualified as Web
import Task qualified
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.BlobStore.S3" do
    describe "Application selection" do
      it "uploads and downloads through Application using signed S3 requests" \_ -> do
        withS3BlobStore \fake _ -> do
          count <- ConcurrentVar.containing (0 :: Int)
          let makeBackend _ = do
                ConcurrentVar.modify (\n -> n + 1) count
                createBlobStore (validConfig fake.endpoint)
          let app = Application.new
                |> Application.withFileUploadUsing @() (\_ -> applicationUploadConfig) makeBackend
          withUploadApplication app \endpoint -> do
            let payload = "application-selected-s3\0binary-content"
            let body = Text.toBytes [fmt|--upload-boundary\r\nContent-Disposition: form-data; name="file"; filename="proof.bin"\r\nContent-Type: application/octet-stream\r\n\r\n#{payload}\r\n--upload-boundary--\r\n|]
            let request = Http.request
                  |> Http.withUrl [fmt|#{endpoint}/files/upload|]
                  |> Http.addHeader "Content-Type" "multipart/form-data; boundary=upload-boundary"
            uploaded <- HttpInternal.sendRaw Http.Post request body |> Task.mapError toText
            uploaded.statusCode |> shouldBe 200
            receipt <- case Json.decodeBytes @UploadedFile uploaded.body of
              Ok value -> Task.yield value
              Err error -> Task.throw [fmt|Invalid upload response: #{error}|]
            let reference = receipt.fileRef
            downloaded <- HttpInternal.getRaw (Http.request |> Http.withUrl [fmt|#{endpoint}/files/#{reference}|]) |> Task.mapError toText
            downloaded.statusCode |> shouldBe 200
            downloaded.body |> shouldBe (Text.toBytes payload)
            keys <- FakeS3.objects fake
            Array.length keys |> shouldBe 1
            seen <- ConcurrentVar.peek fake.requests
            seen |> Array.map (\(method, _) -> method) |> shouldBe ["PUT", "GET"]
            seen |> Array.map (\(_, path) -> path) |> shouldBe (Array.append keys keys)
            ConcurrentVar.peek count |> Task.andThen (shouldBe 1)

    describe "Independent wire verifier" do
      it "accepts the published AWS GET signature independently" \_ -> do
        verifyAwsRequest "GET" "/test.txt" awsHeaders Bytes.empty |> shouldBe True

      it "rejects wrong-secret and body method path host tampering" \_ -> do
        let config = validConfig "https://s3.example.com"
        FakeS3.verifyRequest config.accessKeyId "WRONG" "GET" "/test.txt" awsHeaders Bytes.empty |> shouldBe False
        verifyAwsRequest "GET" "/test.txt" awsHeaders (Text.toBytes "tampered") |> shouldBe False
        verifyAwsRequest "PUT" "/test.txt" awsHeaders Bytes.empty |> shouldBe False
        verifyAwsRequest "GET" "/other.txt" awsHeaders Bytes.empty |> shouldBe False
        let changedHost = awsHeaders |> Array.map (\(name, value) -> (name, if name == "host" then "other.example.com" else value))
        verifyAwsRequest "GET" "/test.txt" changedHost Bytes.empty |> shouldBe False

    describe "Fixture readiness" do
      it "serves immediately on each owned socket without a startup delay" \_ -> do
        Array.repeat 10 () |> Task.forEach (\_ -> do
          withS3BlobStore \_ blobStore -> do
            blobStore.store (BlobKey "ready") Bytes.empty |> orFail
            exists <- blobStore.exists (BlobKey "ready") |> orFail
            exists |> shouldBe True)

    describe "Basic Operations" do
      it "store and retrieve roundtrips bytes through signed PUT and GET" \_ -> do
        withS3BlobStore \fake blobStore -> do
          let blobKey = BlobKey "test-blob-1"
          let content = "Hello, S3!" |> Text.toBytes
          blobStore.store blobKey content |> orFail
          retrieved <- blobStore.retrieve blobKey |> orFail
          retrieved |> shouldBe content
          seen <- ConcurrentVar.peek fake.requests
          seen |> shouldBe [("PUT", "/test-bucket/test-blob-1"), ("GET", "/test-bucket/test-blob-1")]

      it "store overwrites an existing blob" \_ -> do
        withS3BlobStore \_ blobStore -> do
          let blobKey = BlobKey "overwrite-me"
          blobStore.store blobKey ("first" |> Text.toBytes) |> orFail
          blobStore.store blobKey ("second" |> Text.toBytes) |> orFail
          retrieved <- blobStore.retrieve blobKey |> orFail
          retrieved |> shouldBe ("second" |> Text.toBytes)

      it "exists answers True after store and False for an unknown key" \_ -> do
        withS3BlobStore \fake blobStore -> do
          let blobKey = BlobKey "present"
          blobStore.store blobKey ("x" |> Text.toBytes) |> orFail
          present <- blobStore.exists blobKey |> orFail
          present |> shouldBe True
          missing <- blobStore.exists (BlobKey "absent") |> orFail
          missing |> shouldBe False
          seen <- ConcurrentVar.peek fake.requests
          seen |> Array.get 1 |> shouldBe (Just ("HEAD", "/test-bucket/present"))

      it "delete removes the blob and is idempotent" \_ -> do
        withS3BlobStore \fake blobStore -> do
          let blobKey = BlobKey "delete-me"
          blobStore.store blobKey ("bye" |> Text.toBytes) |> orFail
          blobStore.delete blobKey |> orFail
          blobStore.delete blobKey |> orFail
          remaining <- FakeS3.objects fake
          remaining |> shouldBe []

      it "stores and retrieves an empty blob" \_ -> do
        withS3BlobStore \_ blobStore -> do
          let blobKey = BlobKey "empty"
          blobStore.store blobKey Bytes.empty |> orFail
          retrieved <- blobStore.retrieve blobKey |> orFail
          retrieved |> shouldBe Bytes.empty

    describe "Error Mapping" do
      it "rejects a wrong signing secret without storing bytes" \_ -> do
        withFakeS3 "test-bucket" "AKIAIOSFODNN7EXAMPLE" "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" \fake -> do
          let config = (validConfig fake.endpoint) {secretAccessKey = "COMPLETELY-WRONG-SECRET"}
          blobStore <- createBlobStore config
          result <- blobStore.store (BlobKey "wrong-secret") (Text.toBytes "stored-with-wrong-secret") |> Task.asResult
          result |> shouldBe (Err (StorageError "S3 HTTP 403"))
          remaining <- FakeS3.objects fake
          remaining |> shouldBe []

      it "retrieve of a missing key is NotFound" \_ -> do
        withS3BlobStore \_ blobStore -> do
          result <- blobStore.retrieve (BlobKey "nope") |> Task.asResult
          result |> shouldBe (Err (NotFound (BlobKey "nope")))

      it "a 403 from the store is a StorageError carrying only the status" \_ -> do
        withS3BlobStore \fake blobStore -> do
          ConcurrentVar.swap Forbid fake.mode |> discard
          result <- blobStore.store (BlobKey "denied") ("x" |> Text.toBytes) |> Task.asResult
          result |> shouldBe (Err (StorageError "S3 HTTP 403"))
          existsResult <- blobStore.exists (BlobKey "denied") |> Task.asResult
          existsResult |> shouldBe (Err (StorageError "S3 HTTP 403"))

      it "an unreachable endpoint is a StorageError without the URL" \_ -> do
        blobStore <-
          createBlobStore (validConfig "http://127.0.0.1:9")
            |> Task.mapError (\e -> [fmt|createBlobStore failed: #{e}|])
        result <- blobStore.exists (BlobKey "any") |> Task.asResult
        case result of
          Err (StorageError message) -> Text.contains "127.0.0.1" message |> shouldBe False
          other -> fail [fmt|expected StorageError, got #{show other}|]

    describe "Key Validation" do
      it "encodes each key segment and keeps slashes as separators" \_ -> do
        withS3BlobStore \fake blobStore -> do
          blobStore.store (BlobKey "owner/a b$c") ("x" |> Text.toBytes) |> orFail
          seen <- ConcurrentVar.peek fake.requests
          seen |> shouldBe [("PUT", "/test-bucket/owner/a%20b%24c")]

      it "rejects dot segments and empty keys without sending a request" \_ -> do
        withS3BlobStore \fake blobStore -> do
          dotResult <- blobStore.store (BlobKey "a/../b") Bytes.empty |> Task.asResult
          dotResult |> shouldBe (Err (InvalidBlobKey "S3 key must not contain empty or dot path segments"))
          emptyResult <- blobStore.exists (BlobKey "") |> Task.asResult
          emptyResult |> shouldBe (Err (InvalidBlobKey "S3 key must contain 1 to 1024 UTF-8 bytes"))
          seen <- ConcurrentVar.peek fake.requests
          seen |> shouldBe []

    describe "Configuration" do
      it "rejects malformed endpoint authorities before creating a store" \_ -> do
        let endpoints =
              [ "http://[::1]:abc"
              , "https://127.0.0.1:"
              , "https://127.0.0.1:99999"
              , "http://[::1]evil.invalid"
              , "https://exa\tmple.invalid"
              , "https://[::1"
              , "https://127.0.0.1\r\nX-Injected:1"
              ]
        outcomes <- endpoints |> Task.mapArray (\endpoint -> configRejected (validConfig endpoint))
        outcomes |> shouldBe (endpoints |> Array.map (\_ -> True))
        positive <- createBlobStore (validConfig "https://s3.example.com") |> Task.asResult
        positive |> Result.map (\_ -> ()) |> shouldBe (Ok ())

      it "validates endpoint port bounds and bracket suffixes without network I/O" \_ -> do
        let invalid = ["https://127.0.0.1:0", "https://127.0.0.1:-1", "https://127.0.0.1:65536", "https://[::1]:", "https://[::1]:443junk", "https://[]", "https://[not-ip]", "https://127.0.0.1:18446744073709551617"]
        rejected <- invalid |> Task.mapArray (\endpoint -> configRejected (validConfig endpoint))
        rejected |> shouldBe (invalid |> Array.map (\_ -> True))
        let valid = ["http://localhost:9000", "https://127.0.0.1:1", "https://127.0.0.1:65535", "https://[2001:db8::1]:443", " https://example.com:443/ ", "https://example.com:000443"]
        accepted <- valid |> Task.mapArray (\endpoint -> do
          result <- createBlobStore (validConfig endpoint) |> Task.asResult
          Task.yield (result |> Result.map (\_ -> ())))
        accepted |> shouldBe (valid |> Array.map (\_ -> Ok ()))

      it "rejects header-bound controls before any HTTP request" \_ -> do
        withFakeS3 "test-bucket" "AKIAIOSFODNN7EXAMPLE" "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" \fake -> do
          let config = validConfig fake.endpoint
          outcomes <- [config {region = "us-east-1\n"}, config {S3.accessKeyId = "AKIA\r\nInjected: value"}] |> Task.mapArray configRejected
          outcomes |> shouldBe [True, True]
          seen <- ConcurrentVar.peek fake.requests
          seen |> shouldBe []
          blobStore <- createBlobStore config
          blobStore.store (BlobKey "valid-config-control") Bytes.empty |> orFail

      it "accepts nonempty secret material that is not an HTTP header" \_ -> do
        let config = (validConfig "https://s3.example.com") {secretAccessKey = "secret-\955-\n"}
        result <- createBlobStore config |> Task.asResult
        result |> Result.map (\_ -> ()) |> shouldBe (Ok ())

      it "rejects an http endpoint that is not loopback" \_ -> do
        result <- createBlobStore (validConfig "http://s3.example.com") |> Task.asResult
        result |> Result.map (\_ -> ()) |> shouldBe (Err "S3 requires HTTPS outside literal loopback")

      it "rejects an endpoint carrying a path, query or credentials" \_ -> do
        withPath <- createBlobStore (validConfig "https://s3.example.com/bucket") |> Task.asResult
        withPath |> Result.map (\_ -> ()) |> shouldBe (Err "S3 endpoint must not contain credentials, path, query or fragment")
        withUser <- createBlobStore (validConfig "https://user:pw@s3.example.com") |> Task.asResult
        withUser |> Result.map (\_ -> ()) |> shouldBe (Err "S3 endpoint must not contain credentials, path, query or fragment")

      it "rejects empty fields and non-DNS bucket names" \_ -> do
        emptyKey <- createBlobStore (configWith "" "test-bucket") |> Task.asResult
        emptyKey |> Result.map (\_ -> ()) |> shouldBe (Err "S3 configuration fields must not be empty")
        badBucket <- createBlobStore (configWith "AKIAIOSFODNN7EXAMPLE" "Bad_Bucket") |> Task.asResult
        badBucket |> Result.map (\_ -> ()) |> shouldBe (Err "S3 bucket must be a DNS-style bucket name")

      it "rejects bucket names with an empty label or a hyphen at a label edge" \_ -> do
        let invalidNames = ["a..b", ".ab", "ab.", "-ab", "ab-", "a.-b", "a-.b"]
        outcomes <- invalidNames |> Task.mapArray bucketOutcome
        outcomes |> shouldBe (invalidNames |> Array.map (\name -> (name, Err "S3 bucket must be a DNS-style bucket name")))

      it "accepts dotted and hyphenated bucket names" \_ -> do
        let validNames = ["my-bucket", "a-b.c-d", "logs.2026.v2", "0a.b9"]
        outcomes <- validNames |> Task.mapArray bucketOutcome
        outcomes |> shouldBe (validNames |> Array.map (\name -> (name, Ok ())))

      it "enforces the 3 to 63 character bucket name bounds" \_ -> do
        outcomes <- ["abc", Text.repeat 63 "a", "ab", Text.repeat 64 "a"] |> Task.mapArray bucketOutcome
        outcomes
          |> Array.map (\(_, outcome) -> outcome)
          |> shouldBe [Ok (), Ok (), Err "S3 bucket must be a DNS-style bucket name", Err "S3 bucket must be a DNS-style bucket name"]

      it "accepts an https endpoint without contacting it" \_ -> do
        result <- createBlobStore (validConfig "https://s3.example.com") |> Task.asResult
        result |> Result.map (\_ -> ()) |> shouldBe (Ok ())

      it "accepts loopback over plain http, including bracketed IPv6" \_ -> do
        ipv4 <- createBlobStore (validConfig "http://127.0.0.1:9000/") |> Task.asResult
        ipv4 |> Result.map (\_ -> ()) |> shouldBe (Ok ())
        ipv6 <- createBlobStore (validConfig "http://[::1]:9000") |> Task.asResult
        ipv6 |> Result.map (\_ -> ()) |> shouldBe (Ok ())


-- ==========================================================================
-- Test Infrastructure
-- ==========================================================================

-- | Published AWS GET Object vector, not generated by the signer under test.
awsHeaders :: Array (Text, Text)
awsHeaders =
  [ ("host", "examplebucket.s3.amazonaws.com")
  , ("range", "bytes=0-9")
  , ("x-amz-date", "20130524T000000Z")
  , ("x-amz-content-sha256", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  , ("authorization", "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41")
  ]


verifyAwsRequest :: Text -> Text -> Array (Text, Text) -> Bytes -> Bool
verifyAwsRequest method path headers body = do
  let config = validConfig "https://s3.example.com"
  FakeS3.verifyRequest config.accessKeyId config.secretAccessKey method path headers body


validConfig :: Text -> S3Config
validConfig endpoint =
  S3Config
    { endpoint
    , bucket = "test-bucket"
    , region = "us-east-1"
    , accessKeyId = "AKIAIOSFODNN7EXAMPLE"
    , secretAccessKey = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    }


configWith :: Text -> Text -> S3Config
configWith accessKeyId bucket =
  S3Config
    { endpoint = "https://s3.example.com"
    , bucket
    , region = "us-east-1"
    , accessKeyId
    , secretAccessKey = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    }


-- | Validation outcome of a config that differs from a valid one only in its bucket.
bucketOutcome :: Text -> Task Text (Text, Result Text Unit)
bucketOutcome bucket = do
  outcome <-
    createBlobStore (configWith (validConfig "https://s3.example.com").accessKeyId bucket)
      |> Task.asResult
  Task.yield (bucket, outcome |> Result.map (\_ -> ()))


withS3BlobStore :: (FakeS3 -> BlobStore -> Task Text Unit) -> Task Text Unit
withS3BlobStore action =
  withFakeS3 "test-bucket" "AKIAIOSFODNN7EXAMPLE" "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" \fake -> do
    blobStore <-
      createBlobStore (validConfig fake.endpoint)
        |> Task.mapError (\e -> [fmt|createBlobStore failed: #{e}|])
    action fake blobStore


configRejected :: S3Config -> Task Text Bool
configRejected config = do
  result <- createBlobStore config |> Task.asResult
  case result of
    Err message -> do
      Text.contains config.secretAccessKey message |> shouldBe False
      Text.contains config.accessKeyId message |> shouldBe False
      Task.yield True
    Ok _ -> Task.yield False


orFail :: Task BlobStoreError value -> Task Text value
orFail task = task |> Task.mapError (\e -> [fmt|blob store failed: #{show e}|])


data UploadedFile = UploadedFile
  { fileRef :: Text
  }
  deriving (Generic)

instance Json.FromJSON UploadedFile


applicationUploadConfig :: FileUploadConfig
applicationUploadConfig = FileUploadConfig
  { blobStoreDir = ""
  , stateStoreBackend = InMemoryStateStore
  , maxFileSizeBytes = 4096
  , pendingTtlSeconds = 60
  , cleanupIntervalSeconds = 30
  , allowedContentTypes = Nothing
  , storeOriginalFilename = True
  }


withUploadApplication :: Application.Application -> (Text -> Task Text Unit) -> Task Text Unit
withUploadApplication app action = do
  eventStore <- InMemory.new |> Task.mapError toText
  identifier <- Uuid.generate
  let healthPath = [fmt|s3-proof-#{Uuid.toText identifier}|]
  port <- Task.withCancellationDeferred \_ -> do
    (assignedPort, socket) <- Task.fromIO GhcWarp.openFreePort
    Task.fromIO (GhcSocket.close socket)
    Task.yield assignedPort
  let endpoint = [fmt|http://127.0.0.1:#{port}|]
  let configured = app
        |> Application.withTransport (Web.server {Web.port = port})
        |> Application.withHealthCheck @() (\_ -> healthPath)
  let serve = do
        Application.runWith eventStore configured
        Task.throw "Application stopped before its upload checks completed"
  let check = do
        awaitUploadApplication [fmt|#{endpoint}/#{healthPath}|] 100
        action endpoint
  let timeout = do
        AsyncTask.sleep 10000
        Task.throw "Application upload roundtrip timed out"
  _ <- AsyncTask.race timeout (AsyncTask.race serve check)
  Task.yield ()


awaitUploadApplication :: Text -> Int -> Task Text Unit
awaitUploadApplication healthUrl attempts = do
  result <- HttpInternal.getRaw (Http.request |> Http.withUrl healthUrl |> Http.withTimeout 1) |> Task.asResult
  case result of
    Ok response -> response.statusCode |> shouldBe 200
    Err error ->
      if attempts <= 0
        then fail [fmt|Application did not become ready: #{show error}|]
        else do
          AsyncTask.sleep 25
          awaitUploadApplication healthUrl (attempts - 1)
