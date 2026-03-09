module NeoQL.ParserSpec where

import Basics
import NeoQL (Expr (..), FieldName (..), Value (..))
import NeoQL qualified
import Result (Result (..))
import Result qualified
import Test.Hspec qualified as Hspec
import Text qualified


spec :: Hspec.Spec
spec = do
  Hspec.describe "NeoQL.Parser" do
    Hspec.describe "valid field access" do
      Hspec.it "parses .status" do
        let parsed = NeoQL.parse ".status"
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldAccess (FieldName "status"))

      Hspec.it "parses .createdAt" do
        let parsed = NeoQL.parse ".createdAt"
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldAccess (FieldName "createdAt"))

      Hspec.it "parses .customer_name" do
        let parsed = NeoQL.parse ".customer_name"
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldAccess (FieldName "customer_name"))

    Hspec.describe "valid equality (string)" do
      Hspec.it "parses .status == \"pending\"" do
        let parsed = NeoQL.parse ".status == \"pending\""
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "status") (StringValue "pending"))

      Hspec.it "parses .name == \"Alice\"" do
        let parsed = NeoQL.parse ".name == \"Alice\""
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "name") (StringValue "Alice"))

    Hspec.describe "valid equality (number)" do
      Hspec.it "parses .count == 42" do
        let parsed = NeoQL.parse ".count == 42"
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "count") (NumberValue 42))

      Hspec.it "parses .price == 99.99" do
        let parsed = NeoQL.parse ".price == 99.99"
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "price") (NumberValue 99.99))

      Hspec.it "parses .balance == -10" do
        let parsed = NeoQL.parse ".balance == -10"
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "balance") (NumberValue (-10)))

    Hspec.describe "invalid syntax rejection" do
      Hspec.it "rejects 'status' (no dot)" do
        NeoQL.parse "status" `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects '.123' (digit start)" do
        NeoQL.parse ".123" `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects '.status =' (single equals)" do
        NeoQL.parse ".status =" `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects '.status == ' (trailing operator)" do
        NeoQL.parse ".status == " `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects empty string" do
        NeoQL.parse "" `Hspec.shouldSatisfy` Result.isErr

    Hspec.describe "out-of-scope syntax rejection" do
      Hspec.it "rejects '.a and .b'" do
        NeoQL.parse ".a and .b" `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects '.x > 5'" do
        NeoQL.parse ".x > 5" `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects '.a.b' (nested)" do
        NeoQL.parse ".a.b" `Hspec.shouldSatisfy` Result.isErr

      Hspec.it "rejects '.status != \"x\"'" do
        NeoQL.parse ".status != \"x\"" `Hspec.shouldSatisfy` Result.isErr

    Hspec.describe "whitespace handling" do
      Hspec.it "allows spaces around ==" do
        let parsed = NeoQL.parse ".status   ==   \"pending\""
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "status") (StringValue "pending"))

    Hspec.describe "edge cases" do
      Hspec.it "handles very long identifiers" do
        let identifier = Text.repeat 256 "a"
        let query = [fmt|.#{identifier}|]
        let parsed = NeoQL.parse query
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldAccess (FieldName identifier))

      Hspec.it "handles unicode in string values" do
        let parsed = NeoQL.parse ".name == \"こんにちは\""
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "name") (StringValue "こんにちは"))

      Hspec.it "handles empty string value" do
        let parsed = NeoQL.parse ".name == \"\""
        parsed `Hspec.shouldSatisfy` Result.isOk
        parsed `Hspec.shouldBe` Result.Ok (FieldEquals (FieldName "name") (StringValue ""))
