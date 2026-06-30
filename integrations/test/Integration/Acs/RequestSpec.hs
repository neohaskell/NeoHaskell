{-# LANGUAGE ImplicitParams #-}

module Integration.Acs.RequestSpec (spec) where

import Array qualified
import Basics
import Integration.Acs qualified as Acs
import Integration.Acs.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..), send)
import Json qualified
import Maybe (Maybe (..))
import Redacted (Redacted)
import Redacted qualified
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "Integration.Acs.sender" do
    it "constructs Sender from email string with no display name (happy path)" do
      let result = Acs.sender "noreply@myapp.com"
      result `shouldBe` Sender (Address { name = Nothing, email = "noreply@myapp.com" })

    it "constructs Sender with empty email (edge: empty string input)" do
      let result = Acs.sender ""
      result `shouldBe` Sender (Address { name = Nothing, email = "" })

    it "constructs Sender with unicode email (edge: multibyte UTF-8 characters)" do
      let result = Acs.sender "用户@example.com"
      result `shouldBe` Sender (Address { name = Nothing, email = "用户@example.com" })

  describe "Integration.Acs.recipient" do
    it "constructs Recipient from email string with no display name (happy path)" do
      let result = Acs.recipient "user@example.com"
      result `shouldBe` Recipient (Address { name = Nothing, email = "user@example.com" })

    it "constructs Recipient with empty email (edge: empty string input)" do
      let result = Acs.recipient ""
      result `shouldBe` Recipient (Address { name = Nothing, email = "" })

    it "constructs Recipient with unicode email (edge: multibyte UTF-8 characters)" do
      let result = Acs.recipient "用户@example.com"
      result `shouldBe` Recipient (Address { name = Nothing, email = "用户@example.com" })

  describe "Integration.Acs.send" do
    it "constructs Request with all fields from parameters (happy path: https endpoint, HtmlBody, single recipient)" do
      let testCfg = TestConfig { acsAccessToken = Redacted.wrap "test-token-abc123" }
      let ?config = testCfg
      let request = send
            "https://my-acs.communication.azure.com"
            (Acs.sender "noreply@myapp.com")
            (Acs.recipient "user@example.com")
            "Welcome to ACS"
            (HtmlBody "<h1>Welcome</h1>")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      request.endpoint `shouldBe` "https://my-acs.communication.azure.com"
      request.subject `shouldBe` "Welcome to ACS"
      request.body `shouldBe` HtmlBody "<h1>Welcome</h1>"
      request.to `shouldBe` Array.wrap (Acs.recipient "user@example.com")

    it "constructs Request with TextBody variant (edge: TextBody instead of HtmlBody)" do
      let testCfg = TestConfig { acsAccessToken = Redacted.wrap "test-token" }
      let ?config = testCfg
      let request = send
            "https://my-acs.communication.azure.com"
            (Acs.sender "sender@example.com")
            (Acs.recipient "recipient@example.com")
            "Subject"
            (TextBody "Plain text body")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      request.body `shouldBe` TextBody "Plain text body"

    it "constructs Request with config-provided access token (edge: implicit ?config parameter)" do
      let testCfg = TestConfig { acsAccessToken = Redacted.wrap "bearer-token-xyz" }
      let ?config = testCfg
      let request = send
            "https://my-acs.communication.azure.com"
            (Acs.sender "from@example.com")
            (Acs.recipient "to@example.com")
            "Test"
            (HtmlBody "<p>Hi</p>")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      Redacted.unwrap request.accessToken `shouldBe` "bearer-token-xyz"

    it "stores access token as Redacted without unwrapping in Request (edge: Redacted wrapper preserved)" do
      let testCfg = TestConfig { acsAccessToken = Redacted.wrap "secret-token" }
      let ?config = testCfg
      let request = send
            "https://my-acs.communication.azure.com"
            (Acs.sender "from@example.com")
            (Acs.recipient "to@example.com")
            "Test"
            (HtmlBody "<p>Hi</p>")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      let shown = show request.accessToken
      shown `shouldBe` "<redacted>"

  describe "Integration.Acs.Address" do
    it "constructs Address with both name and email (happy path)" do
      let addr = Address { name = Just "Alice Smith", email = "alice@example.com" }
      addr.name `shouldBe` Just "Alice Smith"
      addr.email `shouldBe` "alice@example.com"

    it "constructs Address with email only, name=Nothing (edge: optional name absent)" do
      let addr = Address { name = Nothing, email = "alice@example.com" }
      addr.name `shouldBe` Nothing
      addr.email `shouldBe` "alice@example.com"

    it "constructs Address with unicode name and email (edge: multibyte characters)" do
      let addr = Address { name = Just "李明", email = "li@example.com" }
      addr.name `shouldBe` Just "李明"
      addr.email `shouldBe` "li@example.com"

    it "round-trips Address to JSON and back (serialization: ToJSON / FromJSON)" do
      let addr = Address { name = Just "Bob Jones", email = "bob@example.com" }
      let encoded = Json.encodeText addr
      let decoded = Json.decodeText encoded :: Result Text Address
      decoded `shouldBe` Result.Ok addr

  describe "Integration.Acs.Sender" do
    it "constructs Sender newtype wrapping Address (happy path)" do
      let addr = Address { name = Nothing, email = "sender@example.com" }
      let s = Sender addr
      s `shouldBe` Sender (Address { name = Nothing, email = "sender@example.com" })

    it "Show Sender redacts the email address (edge: no PII leak via Show)" do
      let s = Sender (Address { name = Nothing, email = "secret@example.com" })
      -- Exact, fixed output: the email string cannot appear in it.
      show s `shouldBe` "Sender <redacted>"

  describe "Integration.Acs.Recipient" do
    it "constructs Recipient newtype wrapping Address (happy path)" do
      let addr = Address { name = Nothing, email = "user@example.com" }
      let r = Recipient addr
      r `shouldBe` Recipient (Address { name = Nothing, email = "user@example.com" })

    it "Show Recipient redacts the email address (edge: no PII leak via Show)" do
      let r = Recipient (Address { name = Nothing, email = "user@example.com" })
      -- Exact, fixed output: the email string cannot appear in it.
      show r `shouldBe` "Recipient <redacted>"

  describe "Integration.Acs.Body" do
    describe "HtmlBody variant" do
      it "constructs HtmlBody with HTML content (happy path)" do
        let b = HtmlBody "<h1>Welcome</h1><p>Content</p>"
        b `shouldBe` HtmlBody "<h1>Welcome</h1><p>Content</p>"

      it "constructs HtmlBody with empty HTML (edge: empty string)" do
        let b = HtmlBody ""
        b `shouldBe` HtmlBody ""

      it "round-trips HtmlBody to JSON and back (serialization: ToJSON / FromJSON)" do
        let b = HtmlBody "<h1>Title</h1>"
        let encoded = Json.encodeText b
        let decoded = Json.decodeText encoded :: Result Text Body
        decoded `shouldBe` Result.Ok b

    describe "TextBody variant" do
      it "constructs TextBody with plain text (happy path)" do
        let b = TextBody "Welcome to ACS!"
        b `shouldBe` TextBody "Welcome to ACS!"

      it "constructs TextBody with empty text (edge: empty string)" do
        let b = TextBody ""
        b `shouldBe` TextBody ""

      it "round-trips TextBody to JSON and back (serialization: ToJSON / FromJSON)" do
        let b = TextBody "Welcome"
        let encoded = Json.encodeText b
        let decoded = Json.decodeText encoded :: Result Text Body
        decoded `shouldBe` Result.Ok b

  describe "Integration.Acs.Request" do
    it "constructs Request record with all fields (happy path)" do
      let req = Request
            { endpoint = "https://my-acs.communication.azure.com"
            , sender = Sender (Address { name = Nothing, email = "from@example.com" })
            , to = Array.wrap (Recipient (Address { name = Nothing, email = "to@example.com" }))
            , subject = "Test"
            , body = HtmlBody "<p>Hello</p>"
            , accessToken = Redacted.wrap "token"
            , onSuccess = \_ -> ("success" :: Text)
            , onError = \_ -> ("error" :: Text)
            }
      req.subject `shouldBe` "Test"
      req.endpoint `shouldBe` "https://my-acs.communication.azure.com"

    it "stores redacted access token without unwrapping (edge: Redacted wrapper preserved)" do
      let req = Request
            { endpoint = "https://my-acs.communication.azure.com"
            , sender = Sender (Address { name = Nothing, email = "from@example.com" })
            , to = Array.wrap (Recipient (Address { name = Nothing, email = "to@example.com" }))
            , subject = "Test"
            , body = HtmlBody "<p>Hello</p>"
            , accessToken = Redacted.wrap "secret"
            , onSuccess = \_ -> ("success" :: Text)
            , onError = \_ -> ("error" :: Text)
            }
      let shown = show req.accessToken
      shown `shouldBe` "<redacted>"

  describe "Integration.Acs.Address serialization" do
    it "round-trips Address through JSON encoding/decoding" do
      let original = Address { name = Just "Alice", email = "alice@example.com" }
      let encoded = Json.encodeText original
      let decoded = Json.decodeText encoded :: Result Text Address
      decoded `shouldBe` Result.Ok original

  describe "Integration.Acs.Body serialization" do
    it "round-trips HtmlBody through JSON encoding/decoding" do
      let original = HtmlBody "<h1>Welcome</h1>"
      let encoded = Json.encodeText original
      let decoded = Json.decodeText encoded :: Result Text Body
      decoded `shouldBe` Result.Ok original

    it "round-trips TextBody through JSON encoding/decoding" do
      let original = TextBody "Welcome"
      let encoded = Json.encodeText original
      let decoded = Json.decodeText encoded :: Result Text Body
      decoded `shouldBe` Result.Ok original


-- | Minimal test config that satisfies HasField "acsAccessToken" constraint.
data TestConfig = TestConfig
  { acsAccessToken :: Redacted Text
  }
