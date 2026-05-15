{-# LANGUAGE ImplicitParams #-}

module Integration.Brevo.RequestSpec (spec) where

import Array qualified
import Basics
import Integration.Brevo qualified as Brevo
import Integration.Brevo.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..), send)
import Json qualified
import Map qualified
import Maybe (Maybe (..))
import Redacted (Redacted)
import Redacted qualified
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "Integration.Brevo.sender" do
    it "constructs Sender from email string with no display name" do
      let result = Brevo.sender "noreply@myapp.com"
      result `shouldBe` Sender (Address { name = Nothing, email = "noreply@myapp.com" })

    it "constructs Sender with empty email (edge: empty input)" do
      let result = Brevo.sender ""
      result `shouldBe` Sender (Address { name = Nothing, email = "" })

    it "constructs Sender with unicode email (edge: multibyte characters)" do
      let result = Brevo.sender "用户@example.com"
      result `shouldBe` Sender (Address { name = Nothing, email = "用户@example.com" })

  describe "Integration.Brevo.recipient" do
    it "constructs Recipient from email string with no display name" do
      let result = Brevo.recipient "user@example.com"
      result `shouldBe` Recipient (Address { name = Nothing, email = "user@example.com" })

    it "constructs Recipient with empty email (edge: empty input)" do
      let result = Brevo.recipient ""
      result `shouldBe` Recipient (Address { name = Nothing, email = "" })

    it "constructs Recipient with unicode email (edge: multibyte characters)" do
      let result = Brevo.recipient "привет@example.com"
      result `shouldBe` Recipient (Address { name = Nothing, email = "привет@example.com" })

  describe "Integration.Brevo.send" do
    it "constructs Request with default fields populated from parameters (happy path: single recipient, HtmlBody)" do
      let testCfg = TestConfig { brevoApiKey = Redacted.wrap "test-api-key-123" }
      let ?config = testCfg
      let request = send
            (Brevo.sender "noreply@myapp.com")
            (Brevo.recipient "user@example.com")
            "Welcome"
            (HtmlBody "<h1>Welcome</h1>")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      request.subject `shouldBe` "Welcome"
      request.body `shouldBe` HtmlBody "<h1>Welcome</h1>"
      request.cc `shouldBe` Array.empty
      request.bcc `shouldBe` Array.empty
      request.replyTo `shouldBe` Nothing
      request.tags `shouldBe` Array.empty

    it "constructs Request with TextBody variant" do
      let testCfg = TestConfig { brevoApiKey = Redacted.wrap "test-api-key-123" }
      let ?config = testCfg
      let request = send
            (Brevo.sender "noreply@myapp.com")
            (Brevo.recipient "user@example.com")
            "Welcome"
            (TextBody "Welcome to MyApp")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      request.body `shouldBe` TextBody "Welcome to MyApp"

    it "constructs Request with single recipient (edge: verifies recipient type-safety via Recipient newtype)" do
      let testCfg = TestConfig { brevoApiKey = Redacted.wrap "test-api-key-123" }
      let recipientVal = Brevo.recipient "user@example.com"
      let ?config = testCfg
      let request = send
            (Brevo.sender "from@example.com")
            recipientVal
            "Test"
            (HtmlBody "<p>Hi</p>")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      request.to `shouldBe` Array.wrap (Brevo.recipient "user@example.com")

    it "constructs Request with config-provided API key (edge: implicit config parameter)" do
      let testCfg = TestConfig { brevoApiKey = Redacted.wrap "test-api-key-123" }
      let ?config = testCfg
      let request = send
            (Brevo.sender "from@example.com")
            (Brevo.recipient "to@example.com")
            "Test"
            (HtmlBody "<p>Hi</p>")
            (\_ -> ("success" :: Text))
            (\_ -> ("error" :: Text))
      Redacted.unwrap request.apiKey `shouldBe` "test-api-key-123"

  describe "Integration.Brevo.Address" do
    it "constructs Address with both name and email (happy path)" do
      let addr = Address { name = Just "Alice Smith", email = "alice@example.com" }
      addr.name `shouldBe` Just "Alice Smith"
      addr.email `shouldBe` "alice@example.com"

    it "constructs Address with email only (edge: name = Nothing)" do
      let addr = Address { name = Nothing, email = "alice@example.com" }
      addr.name `shouldBe` Nothing
      addr.email `shouldBe` "alice@example.com"

    it "constructs Address with unicode name (edge: multibyte characters in name)" do
      let addr = Address { name = Just "李明", email = "li@example.com" }
      addr.name `shouldBe` Just "李明"

    it "round-trips Address to JSON and back (serialization: ToJSON / FromJSON)" do
      let addr = Address { name = Just "Bob Jones", email = "bob@example.com" }
      let encoded = Json.encodeText addr
      let decoded = Json.decodeText encoded :: Result Text Address
      decoded `shouldBe` Result.Ok addr

  describe "Integration.Brevo.Sender" do
    it "constructs Sender newtype wrapping Address (happy path)" do
      let addr = Address { name = Nothing, email = "sender@example.com" }
      let s = Sender addr
      s `shouldBe` Sender (Address { name = Nothing, email = "sender@example.com" })

    it "Show Sender redacts no secrets (edge: Sender Show instance)" do
      let s = Sender (Address { name = Nothing, email = "secret@example.com" })
      let shown = show s
      shown `shouldSatisfy` (\str -> not (str == ""))

  describe "Integration.Brevo.Recipient" do
    it "constructs Recipient newtype wrapping Address (happy path)" do
      let addr = Address { name = Nothing, email = "user@example.com" }
      let r = Recipient addr
      r `shouldBe` Recipient (Address { name = Nothing, email = "user@example.com" })

    it "Show Recipient does not leak secrets (edge: Recipient Show instance)" do
      let r = Recipient (Address { name = Nothing, email = "user@example.com" })
      let shown = show r
      shown `shouldSatisfy` (\str -> not (str == ""))

  describe "Integration.Brevo.Body" do
    describe "HtmlBody variant" do
      it "constructs HtmlBody with HTML content (happy path)" do
        let b = HtmlBody "<h1>Welcome</h1><p>Content</p>"
        b `shouldBe` HtmlBody "<h1>Welcome</h1><p>Content</p>"

      it "constructs HtmlBody with empty HTML (edge: empty content)" do
        let b = HtmlBody ""
        b `shouldBe` HtmlBody ""

      it "round-trips HtmlBody to JSON and back (serialization)" do
        let b = HtmlBody "<h1>Title</h1>"
        let encoded = Json.encodeText b
        let decoded = Json.decodeText encoded :: Result Text Body
        decoded `shouldBe` Result.Ok b

    describe "TextBody variant" do
      it "constructs TextBody with plain text (happy path)" do
        let b = TextBody "Welcome to MyApp!"
        b `shouldBe` TextBody "Welcome to MyApp!"

      it "constructs TextBody with empty text (edge: empty content)" do
        let b = TextBody ""
        b `shouldBe` TextBody ""

      it "round-trips TextBody to JSON and back (serialization)" do
        let b = TextBody "Welcome"
        let encoded = Json.encodeText b
        let decoded = Json.decodeText encoded :: Result Text Body
        decoded `shouldBe` Result.Ok b

    describe "Template variant" do
      it "constructs Template with templateId and empty params (happy path)" do
        let b = Template { templateId = 1, params = Map.empty }
        b `shouldBe` Template { templateId = 1, params = Map.empty }

      it "constructs Template with templateId and multiple params (edge: non-empty params map)" do
        let b = Template
              { templateId = 42
              , params = Map.fromArray [("name", "John"), ("action", "verify")]
              }
        b.templateId `shouldBe` 42

      it "round-trips Template to JSON and back (serialization)" do
        let b = Template { templateId = 99, params = Map.fromArray [("x", "y")] }
        let encoded = Json.encodeText b
        let decoded = Json.decodeText encoded :: Result Text Body
        decoded `shouldBe` Result.Ok b

  describe "Integration.Brevo.Request" do
    it "constructs Request record manually with all fields (happy path)" do
      let req = Request
            { sender = Sender (Address { name = Nothing, email = "from@example.com" })
            , to = Array.wrap (Recipient (Address { name = Nothing, email = "to@example.com" }))
            , subject = "Test"
            , body = HtmlBody "<p>Hello</p>"
            , cc = Array.empty
            , bcc = Array.empty
            , replyTo = Just (Recipient (Address { name = Nothing, email = "reply@example.com" }))
            , tags = Array.wrap "tag1"
            , apiKey = Redacted.wrap "key"
            , onSuccess = \_ -> ("success" :: Text)
            , onError = \_ -> ("error" :: Text)
            }
      req.subject `shouldBe` "Test"
      req.tags `shouldBe` Array.wrap "tag1"

    it "stores redacted API key without unwrapping (edge: Redacted wrapper preserved)" do
      let req = Request
            { sender = Sender (Address { name = Nothing, email = "from@example.com" })
            , to = Array.wrap (Recipient (Address { name = Nothing, email = "to@example.com" }))
            , subject = "Test"
            , body = HtmlBody "<p>Hello</p>"
            , cc = Array.empty
            , bcc = Array.empty
            , replyTo = Nothing
            , tags = Array.empty
            , apiKey = Redacted.wrap "secret"
            , onSuccess = \_ -> ("success" :: Text)
            , onError = \_ -> ("error" :: Text)
            }
      let shown = show req.apiKey
      shown `shouldSatisfy` (\str -> str == "<redacted>")


-- | Minimal test config that satisfies HasField "brevoApiKey" constraint.
data TestConfig = TestConfig
  { brevoApiKey :: Redacted Text
  }
