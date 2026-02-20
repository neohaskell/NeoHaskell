module Service.Transport.Cli.OutputSpec where

import Core
import Service.Response (CommandResponse (..))
import Service.Transport.Cli.Output (OutputFormat (..))
import Service.Transport.Cli.Output qualified as Output
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Transport.Cli.Output" do
    describe "formatResponse" do
      -- ======================================================================
      -- JsonOutput
      -- ======================================================================
      describe "JsonOutput" do
        it "formats Accepted response as JSON containing entityId" \_ -> do
          let response = Accepted {entityId = "entity-123"}
          let result = Output.formatResponse JsonOutput response
          result |> shouldSatisfy (Text.contains "entity-123")

        it "formats Rejected response as JSON containing reason" \_ -> do
          let response = Rejected {reason = "invalid input"}
          let result = Output.formatResponse JsonOutput response
          result |> shouldSatisfy (Text.contains "invalid input")

        it "formats Failed response as JSON containing error" \_ -> do
          let response = Failed {error = "something went wrong"}
          let result = Output.formatResponse JsonOutput response
          result |> shouldSatisfy (Text.contains "something went wrong")

      -- ======================================================================
      -- PrettyOutput
      -- ======================================================================
      describe "PrettyOutput" do
        it "formats Accepted response as JSON containing entityId" \_ -> do
          let response = Accepted {entityId = "entity-456"}
          let result = Output.formatResponse PrettyOutput response
          result |> shouldSatisfy (Text.contains "entity-456")

        it "formats Rejected response as JSON containing reason" \_ -> do
          let response = Rejected {reason = "not allowed"}
          let result = Output.formatResponse PrettyOutput response
          result |> shouldSatisfy (Text.contains "not allowed")

        it "formats Failed response as JSON containing error" \_ -> do
          let response = Failed {error = "internal error"}
          let result = Output.formatResponse PrettyOutput response
          result |> shouldSatisfy (Text.contains "internal error")

      -- ======================================================================
      -- QuietOutput
      -- ======================================================================
      describe "QuietOutput" do
        it "extracts entityId from Accepted response" \_ -> do
          let response = Accepted {entityId = "entity-789"}
          let result = Output.formatResponse QuietOutput response
          result |> shouldBe "entity-789"

        it "extracts reason from Rejected response" \_ -> do
          let response = Rejected {reason = "duplicate entry"}
          let result = Output.formatResponse QuietOutput response
          result |> shouldBe "duplicate entry"

        it "extracts error from Failed response" \_ -> do
          let response = Failed {error = "timeout exceeded"}
          let result = Output.formatResponse QuietOutput response
          result |> shouldBe "timeout exceeded"
