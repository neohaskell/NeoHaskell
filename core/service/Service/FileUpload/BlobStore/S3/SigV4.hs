-- | Sign S3 HTTP requests with AWS Signature Version 4 ("s3", single-chunk payloads).
-- Supply 'Credentials', a UTC 'DateTime', and a 'CanonicalRequest' to 'authorizationHeader'.
module Service.FileUpload.BlobStore.S3.SigV4 (
  -- * Inputs
  Credentials (..),
  CanonicalRequest (..),

  -- * Steps
  canonicalRequest,
  stringToSign,
  signature,
  signedHeaders,
  authorizationHeader,

  -- * Encoding helpers
  sha256Hex,
  uriEncode,
  formatAmzDate,
) where

import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import Char (Char)
import Char qualified
import Crypto qualified
import Data.Word (Word8)
import DateTime (DateTime)
import DateTime qualified
import LinkedList qualified
import Maybe qualified
import Text (Text)
import Text qualified


-- | Static access credentials for S3 request signing.
data Credentials = Credentials
  { accessKeyId :: Text,
    secretAccessKey :: Text,
    region :: Text
  }


-- | The parts of an HTTP request that SigV4 covers.
-- Headers must already include @host@, @x-amz-date@, and @x-amz-content-sha256@.
data CanonicalRequest = CanonicalRequest
  { method :: Text,
    -- | Absolute path, already URI-encoded per segment ("/bucket/a%20b").
    path :: Text,
    -- | Query pairs, raw (unencoded) names and values.
    query :: Array (Text, Text),
    -- | Header names and values, raw. Names are lower-cased during canonicalization.
    headers :: Array (Text, Text),
    -- | Lowercase hex SHA-256 of the payload.
    payloadHash :: Text
  }


-- | Step 1 of the AWS signing process.
canonicalRequest :: CanonicalRequest -> Text
canonicalRequest request = do
  let canonicalQuery =
        request.query
          |> Array.map (\(name, value) -> (uriEncode name, uriEncode value))
          |> sortPairs
          |> Array.map (\(name, value) -> [fmt|#{name}=#{value}|])
          |> Text.joinWith "&"
  let canonicalHeaders =
        normalizedHeaders request.headers
          |> Array.map (\(name, value) -> [fmt|#{name}:#{value}\n|])
          |> Text.concat
  let signed = signedHeaders request.headers
  let method = request.method
  let path = request.path
  let payloadHash = request.payloadHash
  [fmt|#{method}\n#{path}\n#{canonicalQuery}\n#{canonicalHeaders}\n#{signed}\n#{payloadHash}|]


-- | Semicolon-joined, sorted, lowercase header names.
signedHeaders :: Array (Text, Text) -> Text
signedHeaders headers =
  normalizedHeaders headers
    |> Array.map (\(name, _) -> name)
    |> Text.joinWith ";"


-- | Step 2 of the AWS signing process for the @s3@ service at the given instant.
stringToSign :: Credentials -> DateTime -> CanonicalRequest -> Text
stringToSign credentials instant request = do
  let amzDate = formatAmzDate instant
  let scope = credentialScope credentials instant
  let hashedRequest = canonicalRequest request |> Text.toBytes |> sha256Hex
  [fmt|AWS4-HMAC-SHA256\n#{amzDate}\n#{scope}\n#{hashedRequest}|]


-- | Step 3 of the AWS signing process, producing the lowercase hex signature.
signature :: Credentials -> DateTime -> CanonicalRequest -> Text
signature credentials instant request = do
  let secret = credentials.secretAccessKey
  let dateKey = hmac (Text.toBytes [fmt|AWS4#{secret}|]) (Text.toBytes (dateStamp instant))
  let regionKey = hmac dateKey (Text.toBytes credentials.region)
  let serviceKey = hmac regionKey (Text.toBytes "s3")
  let signingKey = hmac serviceKey (Text.toBytes "aws4_request")
  hmac signingKey (stringToSign credentials instant request |> Text.toBytes)
    |> hex


-- | Step 4 of the AWS signing process, producing the @Authorization@ header value.
authorizationHeader :: Credentials -> DateTime -> CanonicalRequest -> Text
authorizationHeader credentials instant request = do
  let scope = credentialScope credentials instant
  let signed = signedHeaders request.headers
  let sig = signature credentials instant request
  let accessKey = credentials.accessKeyId
  [fmt|AWS4-HMAC-SHA256 Credential=#{accessKey}/#{scope}, SignedHeaders=#{signed}, Signature=#{sig}|]


-- | @YYYYMMDD'T'HHMMSS'Z'@ in UTC, the @x-amz-date@ wire format.
formatAmzDate :: DateTime -> Text
formatAmzDate instant = do
  let (year, month, day, hour, minute, second) = civilFromEpoch (DateTime.toEpochSeconds instant)
  let two value = Text.fromInt value |> Text.padLeft 2 '0'
  let yearText = Text.fromInt year
  let monthText = two month
  let dayText = two day
  let hourText = two hour
  let minuteText = two minute
  let secondText = two second
  [fmt|#{yearText}#{monthText}#{dayText}T#{hourText}#{minuteText}#{secondText}Z|]


-- | Lowercase hex SHA-256 of raw bytes.
--
-- >>> Text.toBytes "" |> sha256Hex
-- "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
sha256Hex :: Bytes -> Text
sha256Hex bytes =
  bytes
    |> Crypto.sha256
    |> Crypto.toHex


-- | AWS URI encoding where unreserved characters pass through and everything else
-- (including @/@) becomes uppercase percent escapes of its UTF-8 bytes.
uriEncode :: Text -> Text
uriEncode text =
  text
    |> Text.toArray
    |> Array.map encodeChar
    |> Text.concat


credentialScope :: Credentials -> DateTime -> Text
credentialScope credentials instant = do
  let date = dateStamp instant
  let region = credentials.region
  [fmt|#{date}/#{region}/s3/aws4_request|]


dateStamp :: DateTime -> Text
dateStamp instant = formatAmzDate instant |> Text.left 8


normalizedHeaders :: Array (Text, Text) -> Array (Text, Text)
normalizedHeaders headers =
  headers
    |> Array.map (\(name, value) -> (Text.toLower name, collapseSpaces value))
    |> sortPairs


collapseSpaces :: Text -> Text
collapseSpaces value = Text.words value |> Text.joinWith " "


sortPairs :: Array (Text, Text) -> Array (Text, Text)
sortPairs pairs =
  pairs
    |> Array.toLinkedList
    |> LinkedList.sort
    |> Array.fromLinkedList


encodeChar :: Char -> Text
encodeChar char =
  if isUnreserved char
    then Text.fromChar char
    else
      Text.fromChar char
        |> Text.toBytes
        |> Bytes.unpack
        |> LinkedList.map (\byte -> "%" |> Text.append (byteHex byte))
        |> Array.fromLinkedList
        |> Text.concat


-- RFC 3986 unreserved characters allowed unescaped by AWS SigV4.
isUnreserved :: Char -> Bool
isUnreserved char =
  (Char.isAlphaNum char && Char.toCode char < 128) || char == '-' || char == '_' || char == '.' || char == '~'


byteHex :: Word8 -> Text
byteHex byte = do
  let value = fromIntegral byte :: Int
  let digit nibble = "0123456789ABCDEF" |> Text.toArray |> Array.get nibble |> Maybe.withDefault '0'
  Text.fromArray [digit (value // 16), digit (modBy 16 value)]


hex :: Bytes -> Text
hex = Crypto.toHex


hmac :: Bytes -> Bytes -> Bytes
hmac = Crypto.hmacSha256


-- Howard Hinnant's proleptic Gregorian days-from-civil inverse.
civilFromEpoch :: Int64 -> (Int, Int, Int, Int, Int, Int)
civilFromEpoch epoch = do
  let total = fromIntegral epoch :: Int
  let days = total // 86400
  let secondsOfDay = modBy 86400 total
  let z = days + 719468
  let era = (if z >= 0 then z else z - 146096) // 146097
  let dayOfEra = z - era * 146097
  let yearOfEra = (dayOfEra - dayOfEra // 1460 + dayOfEra // 36524 - dayOfEra // 146096) // 365
  let dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra // 4 - yearOfEra // 100)
  let monthIndex = (5 * dayOfYear + 2) // 153
  let day = dayOfYear - (153 * monthIndex + 2) // 5 + 1
  let month = if monthIndex < 10 then monthIndex + 3 else monthIndex - 9
  let year = yearOfEra + era * 400 + (if month <= 2 then 1 else 0)
  (year, month, day, secondsOfDay // 3600, modBy 60 (secondsOfDay // 60), modBy 60 secondsOfDay)
