-- | In-process fake of the S3 object API, just enough for the S3 BlobStore
-- facade: PUT / GET / HEAD / DELETE on @/bucket/key@ with ETag headers and
-- 200 / 204 / 404 / 403 answers. It speaks the S3 wire protocol and nothing
-- else.
--
-- Boundary (deliberate, per maintainer request): this module depends only on
-- wai/warp and Core. It imports nothing from the FileUpload service or from
-- other test infrastructure, and only the S3 specs import it, so it can move
-- with the S3 backend if the package is split.
--
-- Authentication is verified from the received method, path, headers and body
-- using Core hashes/HMAC, independently of the production SigV4 module.
-- An AWS published vector and tamper controls calibrate the verifier.
module Service.FileUpload.BlobStore.S3.FakeS3 (
  FakeS3 (..),
  Mode (..),
  withFakeS3,
  objects,
  verifyRequest,
) where

import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Crypto qualified
import IO (IO)
import Maybe (Maybe (..))
import Maybe qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified

-- Transport only: WAI/Warp are the HTTP server core already depends on.
import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.Async qualified as GhcAsync
import Control.Exception qualified as GhcException
import Data.ByteString.Lazy qualified as GhcLBS
import Data.CaseInsensitive qualified as GhcCI
import Network.HTTP.Types qualified as GhcHTTP
import Network.Socket qualified as GhcSocket
import Network.Wai qualified as GhcWai
import Network.Wai.Handler.Warp qualified as GhcWarp


-- | How the fake answers signed requests.
data Mode
  = Accept
  -- ^ Normal S3 behaviour.
  | Forbid
  -- ^ Every request answers 403 (wrong credentials / policy denial).
  deriving (Eq, Show)


data FakeS3 = FakeS3
  { port :: Int
  , endpoint :: Text
  -- ^ @http://127.0.0.1:PORT@
  , bucket :: Text
  , accessKeyId :: Text
  , expectedSecret :: Text
  , mode :: ConcurrentVar Mode
  , store :: ConcurrentMap Text Bytes
  -- ^ Objects by full path (@/bucket/key@) as received on the wire.
  , requests :: ConcurrentVar (Array (Text, Text))
  -- ^ (method, path) of every request seen, oldest first.
  }


-- | Keep one bound loopback socket from allocation through server shutdown.
withFakeS3 :: Text -> Text -> Text -> (FakeS3 -> Task Text Unit) -> Task Text Unit
withFakeS3 bucket accessKeyId expectedSecret action = do
  mode <- ConcurrentVar.containing Accept
  store <- ConcurrentMap.new
  requests <- ConcurrentVar.containing Array.empty
  Task.fromIOResult do
    GhcException.bracket openLoopbackSocket GhcSocket.close \socket -> do
      assignedPort <- GhcSocket.socketPort socket
      let port = fromIntegral assignedPort
      ready <- GhcConcurrent.newEmptyMVar
      let fake = FakeS3
            { port
            , endpoint = [fmt|http://127.0.0.1:#{port}|]
            , bucket
            , accessKeyId
            , expectedSecret
            , mode
            , store
            , requests
            }
      let settings = GhcWarp.defaultSettings |> GhcWarp.setBeforeMainLoop (GhcConcurrent.putMVar ready ())
      GhcAsync.withAsync (GhcWarp.runSettingsSocket settings socket (application fake)) \server -> do
        GhcAsync.link server
        GhcConcurrent.takeMVar ready
        action fake |> Task.runResult


-- | Current object paths, for assertions.
objects :: FakeS3 -> Task Text (Array Text)
objects fake = ConcurrentMap.keys fake.store


-- ==========================================================================
-- WAI application
-- ==========================================================================

application :: FakeS3 -> GhcWai.Application
application fake request respond = do
  let method = GhcWai.requestMethod request |> Bytes.fromLegacy |> Text.fromBytes
  let path = GhcWai.rawPathInfo request |> Bytes.fromLegacy |> Text.fromBytes
  body <- GhcWai.strictRequestBody request
  let headers =
        GhcWai.requestHeaders request
          |> Array.fromLinkedList
          |> Array.map (\(name, value) -> (headerNameText name, value |> Bytes.fromLegacy |> Text.fromBytes))
  response <-
    handle fake method path headers (body |> Bytes.fromLazyLegacy)
      |> Task.runOrPanic
  respond response


headerNameText :: GhcHTTP.HeaderName -> Text
headerNameText name = GhcCI.original name |> Bytes.fromLegacy |> Text.fromBytes |> Text.toLower


handle :: FakeS3 -> Text -> Text -> Array (Text, Text) -> Bytes -> Task Text GhcWai.Response
handle fake method path headers body = do
  ConcurrentVar.modify (Array.push (method, path)) fake.requests
  mode <- ConcurrentVar.peek fake.mode
  let bucket = fake.bucket
  let bucketPrefix = [fmt|/#{bucket}/|] :: Text
  let authorized = verifyRequest fake.accessKeyId fake.expectedSecret method path headers body
  if not authorized || mode == Forbid
    then Task.yield (xmlError GhcHTTP.status403 "AccessDenied")
    else serveBucket fake bucketPrefix method path body


serveBucket :: FakeS3 -> Text -> Text -> Text -> Bytes -> Task Text GhcWai.Response
serveBucket fake bucketPrefix method path body =
  if Text.startsWith bucketPrefix path
    then dispatch fake method path body
    else Task.yield (xmlError GhcHTTP.status404 "NoSuchBucket")


-- | Independent wire verifier. This deliberately does not import the production
-- SigV4 module; the AWS GET reference vector calibrates the canonicalization.
verifyRequest :: Text -> Text -> Text -> Text -> Array (Text, Text) -> Bytes -> Bool
verifyRequest accessKey secret method path headers body = do
  let authorization = headerValue "authorization" headers
  let attributes = authorization |> Text.dropLeft 17 |> Text.split ", "
  let credential = attributeValue "Credential=" attributes
  let scopeParts = credential |> Text.split "/"
  let date = scopeParts |> Array.get 1 |> Maybe.withDefault ""
  let region = scopeParts |> Array.get 2 |> Maybe.withDefault ""
  let scope = [fmt|#{date}/#{region}/s3/aws4_request|]
  let signedNames = attributeValue "SignedHeaders=" attributes
  let names = Text.split ";" signedNames
  let payloadHash = body |> Crypto.sha256 |> Crypto.toHex
  let expected = wireSignature secret method path headers scope signedNames date region
  Text.startsWith "AWS4-HMAC-SHA256 " authorization
    && credential == [fmt|#{accessKey}/#{scope}|]
    && not (Text.isEmpty date || Text.isEmpty region)
    && Text.startsWith date (headerValue "x-amz-date" headers)
    && not (Array.any (\name -> Text.isEmpty (headerValue name headers)) names)
    && not (Array.any (\name -> not (Array.any (\signed -> name == signed) names)) ["host", "x-amz-content-sha256", "x-amz-date"])
    && headerValue "x-amz-content-sha256" headers == payloadHash
    && attributeValue "Signature=" attributes == expected


headerValue :: Text -> Array (Text, Text) -> Text
headerValue name headers =
  headers |> Array.find (\(key, _) -> key == name) |> Maybe.map (\(_, value) -> value) |> Maybe.withDefault ""


attributeValue :: Text -> Array Text -> Text
attributeValue prefix attributes =
  attributes |> Array.find (Text.startsWith prefix) |> Maybe.map (Text.dropLeft (Text.length prefix)) |> Maybe.withDefault ""


wireSignature :: Text -> Text -> Text -> Array (Text, Text) -> Text -> Text -> Text -> Text -> Text
wireSignature secret method path headers scope signedNames date region = do
  let canonicalHeaders =
        signedNames |> Text.split ";" |> Array.map (\name -> do
          let value = headerValue name headers |> Text.words |> Text.joinWith " "
          [fmt|#{name}:#{value}\n|]) |> Text.joinWith ""
  let payloadHash = headerValue "x-amz-content-sha256" headers
  let canonical = [method, path, "", canonicalHeaders, signedNames, payloadHash] |> Text.joinWith "\n"
  let requestHash = canonical |> Text.toBytes |> Crypto.sha256 |> Crypto.toHex
  let timestamp = headerValue "x-amz-date" headers
  let stringToSign = ["AWS4-HMAC-SHA256", timestamp, scope, requestHash] |> Text.joinWith "\n"
  let dateKey = Text.toBytes date |> Crypto.hmacSha256 (Text.toBytes [fmt|AWS4#{secret}|])
  let regionKey = Text.toBytes region |> Crypto.hmacSha256 dateKey
  let serviceKey = Text.toBytes "s3" |> Crypto.hmacSha256 regionKey
  let signingKey = Text.toBytes "aws4_request" |> Crypto.hmacSha256 serviceKey
  stringToSign |> Text.toBytes |> Crypto.hmacSha256 signingKey |> Crypto.toHex


dispatch :: FakeS3 -> Text -> Text -> Bytes -> Task Text GhcWai.Response
dispatch fake method path body =
  case method of
    "PUT" -> do
      ConcurrentMap.set path body fake.store
      Task.yield (plain GhcHTTP.status200 [("ETag", etagOf body)] Bytes.empty)
    "GET" -> do
      stored <- ConcurrentMap.get path fake.store
      case stored of
        Nothing -> Task.yield (xmlError GhcHTTP.status404 "NoSuchKey")
        Just bytes -> Task.yield (plain GhcHTTP.status200 [("ETag", etagOf bytes)] bytes)
    "HEAD" -> do
      stored <- ConcurrentMap.get path fake.store
      case stored of
        Nothing -> Task.yield (plain GhcHTTP.status404 [] Bytes.empty)
        Just bytes -> Task.yield (plain GhcHTTP.status200 [("ETag", etagOf bytes)] Bytes.empty)
    "DELETE" -> do
      ConcurrentMap.remove path fake.store
      Task.yield (plain GhcHTTP.status204 [] Bytes.empty)
    _ -> Task.yield (xmlError GhcHTTP.status405 "MethodNotAllowed")


-- | Opaque but deterministic per content, like a real single-part ETag.
etagOf :: Bytes -> Text
etagOf bytes = do
  let size = Bytes.length bytes
  let tag = bytes |> Bytes.toBase64 |> Text.fromBytes |> Text.left 16
  [fmt|"#{tag}-#{size}"|]


plain :: GhcHTTP.Status -> Array (Text, Text) -> Bytes -> GhcWai.Response
plain status headers body =
  GhcWai.responseLBS
    status
    (headers |> Array.map (\(name, value) -> (name |> Text.toBytes |> Bytes.unwrap |> GhcCI.mk, value |> Text.toBytes |> Bytes.unwrap)) |> Array.toLinkedList)
    (Bytes.toLazyLegacy body)


xmlError :: GhcHTTP.Status -> Text -> GhcWai.Response
xmlError status code =
  GhcWai.responseLBS
    status
    [(GhcHTTP.hContentType, "application/xml")]
    (GhcLBS.fromStrict ([fmt|<Error><Code>#{code}</Code></Error>|] |> Text.toBytes |> Bytes.unwrap))


-- | The socket stays owned by the surrounding bracket; no release/rebind gap.
openLoopbackSocket :: IO GhcSocket.Socket
openLoopbackSocket =
  GhcException.bracketOnError
    (GhcSocket.socket GhcSocket.AF_INET GhcSocket.Stream GhcSocket.defaultProtocol)
    GhcSocket.close
    (\socket -> do
      GhcSocket.bind socket (GhcSocket.SockAddrInet 0 (GhcSocket.tupleToHostAddress (127, 0, 0, 1)))
      GhcSocket.listen socket GhcSocket.maxListenQueue
      pure socket) -- HOOK-ALLOW: IO boundary of the owned fake server
