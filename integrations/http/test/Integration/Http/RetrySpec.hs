{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Http.RetrySpec where

import Array qualified
import Core
import Integration.Http.Retry qualified as Retry
import Test


spec :: Spec Unit
spec = do
  describe "Integration.Http.Retry" do
    describe "defaultRetry" do
      it "has 3 max attempts" \_ -> do
        Retry.defaultRetry.maxAttempts |> shouldBe 3

      it "has 1000ms initial delay" \_ -> do
        Retry.defaultRetry.initialDelayMs |> shouldBe 1000

      it "has 30000ms max delay" \_ -> do
        Retry.defaultRetry.maxDelayMs |> shouldBe 30000

      it "includes standard retryable status codes" \_ -> do
        let statuses = Retry.defaultRetry.retryableStatuses
        statuses |> Array.contains 429 |> shouldBe True
        statuses |> Array.contains 500 |> shouldBe True
        statuses |> Array.contains 502 |> shouldBe True
        statuses |> Array.contains 503 |> shouldBe True
        statuses |> Array.contains 504 |> shouldBe True

      it "does not include client error codes" \_ -> do
        let statuses = Retry.defaultRetry.retryableStatuses
        statuses |> Array.contains 400 |> shouldBe False
        statuses |> Array.contains 401 |> shouldBe False
        statuses |> Array.contains 403 |> shouldBe False
        statuses |> Array.contains 404 |> shouldBe False

    describe "noRetry" do
      it "has 1 max attempt (no retries)" \_ -> do
        Retry.noRetry.maxAttempts |> shouldBe 1

      it "has 0ms initial delay" \_ -> do
        Retry.noRetry.initialDelayMs |> shouldBe 0

      it "has 0ms max delay" \_ -> do
        Retry.noRetry.maxDelayMs |> shouldBe 0

      it "has empty retryable statuses" \_ -> do
        Retry.noRetry.retryableStatuses |> Array.length |> shouldBe 0

    describe "withRetries" do
      it "sets custom max attempts" \_ -> do
        let retry = Retry.withRetries 5
        retry.maxAttempts |> shouldBe 5

      it "preserves default initial delay" \_ -> do
        let retry = Retry.withRetries 5
        retry.initialDelayMs |> shouldBe 1000

      it "preserves default max delay" \_ -> do
        let retry = Retry.withRetries 5
        retry.maxDelayMs |> shouldBe 30000

      it "preserves default retryable statuses" \_ -> do
        let retry = Retry.withRetries 5
        retry.retryableStatuses |> shouldBe Retry.defaultRetry.retryableStatuses

      it "handles 0 retries" \_ -> do
        let retry = Retry.withRetries 0
        retry.maxAttempts |> shouldBe 0

      it "handles 1 retry (same as noRetry attempts)" \_ -> do
        let retry = Retry.withRetries 1
        retry.maxAttempts |> shouldBe 1
        -- But still has default backoff config unlike noRetry
        retry.initialDelayMs |> shouldBe 1000

      it "handles large retry counts" \_ -> do
        let retry = Retry.withRetries 100
        retry.maxAttempts |> shouldBe 100
