-- | SigV4 against the worked examples in the AWS S3 API reference
-- ("Authenticating Requests: Using the Authorization Header", examples
-- GET Object, PUT Object, GET Bucket Lifecycle, GET Bucket (list objects)).
-- All examples use access key AKIAIOSFODNN7EXAMPLE, the documented example
-- secret, region us-east-1 and the instant 2013-05-24T00:00:00Z.
module Service.FileUpload.BlobStore.S3.SigV4Spec where

import Bytes qualified
import Core
import DateTime qualified
import Service.FileUpload.BlobStore.S3.SigV4 (CanonicalRequest (..), Credentials (..))
import Service.FileUpload.BlobStore.S3.SigV4 qualified as SigV4
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.BlobStore.S3.SigV4" do
    describe "AWS reference vectors" do
      it "hashes the empty payload to the documented SHA-256" \_ -> do
        SigV4.sha256Hex Bytes.empty |> shouldBe emptyPayloadHash

      it "formats x-amz-date as YYYYMMDD'T'HHMMSS'Z'" \_ -> do
        SigV4.formatAmzDate exampleInstant |> shouldBe "20130524T000000Z"

      it "GET Object: canonical request hash, string to sign and signature match AWS" \_ -> do
        let request =
              CanonicalRequest
                { method = "GET"
                , path = "/test.txt"
                , query = []
                , headers =
                    [ ("Host", "examplebucket.s3.amazonaws.com")
                    , ("Range", "bytes=0-9")
                    , ("x-amz-content-sha256", emptyPayloadHash)
                    , ("x-amz-date", "20130524T000000Z")
                    ]
                , payloadHash = emptyPayloadHash
                }
        SigV4.canonicalRequest request |> Text.toBytes |> SigV4.sha256Hex
          |> shouldBe "7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972"
        SigV4.stringToSign exampleCredentials exampleInstant request
          |> shouldBe "AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/aws4_request\n7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972"
        SigV4.signature exampleCredentials exampleInstant request
          |> shouldBe "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
        SigV4.authorizationHeader exampleCredentials exampleInstant request
          |> shouldBe "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"

      it "PUT Object: signs a non-empty payload with extra headers" \_ -> do
        let payloadHash = "Welcome to Amazon S3." |> Text.toBytes |> SigV4.sha256Hex
        payloadHash |> shouldBe "44ce7dd67c959e0d3524ffac1771dfbba87d2b6b4b4e99e42034a8b803f8b072"
        let request =
              CanonicalRequest
                { method = "PUT"
                , path = "/test%24file.text"
                , query = []
                , headers =
                    [ ("Date", "Fri, 24 May 2013 00:00:00 GMT")
                    , ("Host", "examplebucket.s3.amazonaws.com")
                    , ("x-amz-content-sha256", payloadHash)
                    , ("x-amz-date", "20130524T000000Z")
                    , ("x-amz-storage-class", "REDUCED_REDUNDANCY")
                    ]
                , payloadHash
                }
        SigV4.canonicalRequest request |> Text.toBytes |> SigV4.sha256Hex
          |> shouldBe "9e0e90d9c76de8fa5b200d8c849cd5b8dc7a3be3951ddb7f6a76b4158342019d"
        SigV4.signature exampleCredentials exampleInstant request
          |> shouldBe "98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd"

      it "GET Bucket Lifecycle: encodes a valueless query parameter" \_ -> do
        let request = bucketRequest [("lifecycle", "")]
        SigV4.canonicalRequest request |> Text.toBytes |> SigV4.sha256Hex
          |> shouldBe "9766c798316ff2757b517bc739a67f6213b4ab36dd5da2f94eaebf79c77395ca"
        SigV4.signature exampleCredentials exampleInstant request
          |> shouldBe "fea454ca298b7da1c68078a5d1bdbfbbe0d65c699e0f91ac7a200a0136783543"

      it "GET Bucket: sorts query parameters by name" \_ -> do
        let request = bucketRequest [("prefix", "J"), ("max-keys", "2")]
        SigV4.canonicalRequest request |> Text.toBytes |> SigV4.sha256Hex
          |> shouldBe "df57d21db20da04d7fa30298dd4488ba3a2b47ca3a489c74750e0f1e7df1b9b7"
        SigV4.signature exampleCredentials exampleInstant request
          |> shouldBe "34b48302e7b5fa45bde8084f4b7868a86f0a534bc59db6670ed5711ef69dc6f7"

    describe "uriEncode" do
      it "keeps unreserved characters and percent-encodes the rest in uppercase" \_ -> do
        SigV4.uriEncode "AZaz09-_.~" |> shouldBe "AZaz09-_.~"
        SigV4.uriEncode "a b/c$d" |> shouldBe "a%20b%2Fc%24d"
        SigV4.uriEncode "\228" |> shouldBe "%C3%A4"

    describe "signedHeaders" do
      it "lower-cases and sorts header names" \_ -> do
        SigV4.signedHeaders [("X-Amz-Date", "a"), ("Host", "b"), ("Content-Type", "c")]
          |> shouldBe "content-type;host;x-amz-date"


-- ==========================================================================
-- Fixtures
-- ==========================================================================

emptyPayloadHash :: Text
emptyPayloadHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"


exampleCredentials :: Credentials
exampleCredentials =
  Credentials
    { accessKeyId = "AKIAIOSFODNN7EXAMPLE"
    , secretAccessKey = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    , region = "us-east-1"
    }


-- | 2013-05-24T00:00:00Z
exampleInstant :: DateTime
exampleInstant = DateTime.fromEpochSeconds 1369353600


bucketRequest :: Array (Text, Text) -> CanonicalRequest
bucketRequest query =
  CanonicalRequest
    { method = "GET"
    , path = "/"
    , query
    , headers =
        [ ("Host", "examplebucket.s3.amazonaws.com")
        , ("x-amz-content-sha256", emptyPayloadHash)
        , ("x-amz-date", "20130524T000000Z")
        ]
    , payloadHash = emptyPayloadHash
    }
