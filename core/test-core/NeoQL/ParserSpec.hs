module NeoQL.ParserSpec where

import Core
import NeoQL (Expr (..), FieldName (..), Value (..))
import NeoQL qualified
import Result (Result (..))
import Result qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "NeoQL.Parser" do
    describe "valid field access" do
      it "parses .status" \_ -> do
        let parsed = NeoQL.parse ".status"
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldAccess (FieldName "status")))

      it "parses .createdAt" \_ -> do
        let parsed = NeoQL.parse ".createdAt"
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldAccess (FieldName "createdAt")))

      it "parses .customer_name" \_ -> do
        let parsed = NeoQL.parse ".customer_name"
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldAccess (FieldName "customer_name")))

    describe "valid equality (string)" do
      it "parses .status == \"pending\"" \_ -> do
        let parsed = NeoQL.parse ".status == \"pending\""
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "status") (StringValue "pending")))

      it "parses .name == \"Alice\"" \_ -> do
        let parsed = NeoQL.parse ".name == \"Alice\""
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "name") (StringValue "Alice")))

    describe "valid equality (number)" do
      it "parses .count == 42" \_ -> do
        let parsed = NeoQL.parse ".count == 42"
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "count") (NumberValue 42)))

      it "parses .price == 99.99" \_ -> do
        let parsed = NeoQL.parse ".price == 99.99"
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "price") (NumberValue 99.99)))

      it "parses .balance == -10" \_ -> do
        let parsed = NeoQL.parse ".balance == -10"
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "balance") (NumberValue (-10))))

    describe "invalid syntax rejection" do
      it "rejects 'status' (no dot)" \_ -> do
        NeoQL.parse "status" |> shouldSatisfy Result.isErr

      it "rejects '.123' (digit start)" \_ -> do
        NeoQL.parse ".123" |> shouldSatisfy Result.isErr

      it "rejects '.status =' (single equals)" \_ -> do
        NeoQL.parse ".status =" |> shouldSatisfy Result.isErr

      it "rejects '.status == ' (trailing operator)" \_ -> do
        NeoQL.parse ".status == " |> shouldSatisfy Result.isErr

      it "rejects empty string" \_ -> do
        NeoQL.parse "" |> shouldSatisfy Result.isErr

    describe "out-of-scope syntax rejection" do
      it "rejects '.a and .b'" \_ -> do
        NeoQL.parse ".a and .b" |> shouldSatisfy Result.isErr

      it "rejects '.x > 5'" \_ -> do
        NeoQL.parse ".x > 5" |> shouldSatisfy Result.isErr

      it "rejects '.a.b' (nested)" \_ -> do
        NeoQL.parse ".a.b" |> shouldSatisfy Result.isErr

      it "rejects '.status != \"x\"'" \_ -> do
        NeoQL.parse ".status != \"x\"" |> shouldSatisfy Result.isErr

    describe "whitespace handling" do
      it "allows spaces around ==" \_ -> do
        let parsed = NeoQL.parse ".status   ==   \"pending\""
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "status") (StringValue "pending")))

    describe "edge cases" do
      it "handles very long identifiers" \_ -> do
        let identifier = Text.repeat 256 "a"
        let query = [fmt|.#{identifier}|]
        let parsed = NeoQL.parse query
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldAccess (FieldName identifier)))

      it "handles unicode in string values" \_ -> do
        let parsed = NeoQL.parse ".name == \"こんにちは\""
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "name") (StringValue "こんにちは")))

      it "handles empty string value" \_ -> do
        let parsed = NeoQL.parse ".name == \"\""
        parsed |> shouldSatisfy Result.isOk
        parsed |> shouldBe (Result.Ok (FieldEquals (FieldName "name") (StringValue "")))
