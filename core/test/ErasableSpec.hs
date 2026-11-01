{-| Check protected-value identity and display through command decisions.
Run 'spec' in the core test suite.
-}
module ErasableSpec (spec) where

import Array qualified
import Core
import Decider (Decision (..), DecisionContext (..))
import Decider qualified
import Erasable qualified
import Json qualified
import Schema qualified
import Schema.JsonSchema qualified
import Task qualified
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = parallel do
  describe "Erasable.new" do
    it "advertises only public availability states" \_ -> do
      let entry tag contents =
            Json.object
              [ ("type", Json.toJSON ("object" :: Text))
              , ("properties", Json.object
                  [ ("tag", Json.object [("const", Json.toJSON (tag :: Text))])
                  , ("contents", Json.object [("type", Json.toJSON (contents :: Text))])
                  ])
              , ("required", Json.toJSON (["tag", "contents"] :: [Text]))
              , ("additionalProperties", Json.toJSON False)
              ]
      let expected = Json.object
            [("oneOf", Json.toJSON ([entry "Available" "string", entry "Erased" "null"] :: [Json.Value]))]
      let schema = Schema.JsonSchema.toJsonSchema (Schema.toSchema @(Erasable.Erasable Text))
      schema |> shouldBe expected

    it "creates independent references for equal values in one command" \_ -> do
      let marker = "Álvaro ñ 😀"
      let decision = do
            first <- Erasable.new marker
            second <- Erasable.new marker
            Accept AnyStreamState [first, second]
      (outcome, keys) <-
        Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
      case outcome of
        RejectCommand _ -> fail "Expected accepted command"
        AcceptCommand _ values -> do
          case Array.toLinkedList values of
            [first, second] -> do
              Erasable.reference first |> shouldNotBe (Erasable.reference second)
              Json.encodeText first |> shouldSatisfy (\jsonText -> not (Text.contains marker jsonText))
              Text.fromLinkedList (show first) |> shouldSatisfy (\shown -> not (Text.contains marker shown))
            _ -> fail "Expected two protected values"
      Array.length keys |> shouldBe 2

    it "does not return generated keys from a rejected decision" \_ -> do
      let decision = do
            _ <- Erasable.new ("private" :: Text)
            Reject "not accepted"
      (_, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
      Array.length keys |> shouldBe 0

    it "keeps a reference when mapping and does not evaluate an erased value" \_ -> do
      let initial = do
            first <- Erasable.new ("private" :: Text)
            Accept AnyStreamState [first]
      (created, _) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") initial
      first <- case created of
        RejectCommand _ -> Task.throw "Expected protected value"
        AcceptCommand _ values ->
          case Array.first values of
            Nothing -> Task.throw "Expected protected value"
            Just value -> Task.yield value
      let unavailable :: Text -> Text
          unavailable _ = panic "Erased map evaluated its function"
      let mappedDecision = do
            mapped <- Erasable.map unavailable first
            Accept AnyStreamState [mapped]
      (mappedResult, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.yield Decider.Erased) mappedDecision
      case mappedResult of
        RejectCommand _ -> fail "Expected mapped wrapper"
        AcceptCommand _ values ->
          case Array.first values of
            Nothing -> fail "Expected mapped wrapper"
            Just mapped -> Erasable.reference mapped |> shouldBe (Erasable.reference first)
      Array.length keys |> shouldBe 0
