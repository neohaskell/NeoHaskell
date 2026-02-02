module Auth.HostnameSpec where

import Auth.Hostname qualified as Hostname
import Core
import Test


spec :: Spec Unit
spec = do
  describe "Auth.Hostname" do
    describe "isLocalhost" do
      -- Positive cases: these should be recognized as localhost
      describe "accepts valid localhost patterns" do
        it "returns True for 'localhost'" \_ ->
          Hostname.isLocalhost "localhost" |> shouldBe True

        it "returns True for 'localhost:8080'" \_ ->
          Hostname.isLocalhost "localhost:8080" |> shouldBe True

        it "returns True for 'localhost:3000'" \_ ->
          Hostname.isLocalhost "localhost:3000" |> shouldBe True

        it "returns True for '127.0.0.1'" \_ ->
          Hostname.isLocalhost "127.0.0.1" |> shouldBe True

        it "returns True for '127.0.0.1:8080'" \_ ->
          Hostname.isLocalhost "127.0.0.1:8080" |> shouldBe True

        it "returns True for '[::1]'" \_ ->
          Hostname.isLocalhost "[::1]" |> shouldBe True

        it "returns True for '[::1]:443'" \_ ->
          Hostname.isLocalhost "[::1]:443" |> shouldBe True

        it "returns True for '[::1]:8180'" \_ ->
          Hostname.isLocalhost "[::1]:8180" |> shouldBe True

      -- Negative cases: SECURITY CRITICAL
      -- These should NOT be recognized as localhost
      describe "rejects non-localhost patterns (security critical)" do
        it "returns False for 'localhost.evil.com'" \_ ->
          Hostname.isLocalhost "localhost.evil.com" |> shouldBe False

        it "returns False for 'localhost.localdomain'" \_ ->
          Hostname.isLocalhost "localhost.localdomain" |> shouldBe False

        it "returns False for 'example.com'" \_ ->
          Hostname.isLocalhost "example.com" |> shouldBe False

        it "returns False for '127.0.0.2'" \_ ->
          -- Only 127.0.0.1 is considered localhost, not other loopback IPs
          Hostname.isLocalhost "127.0.0.2" |> shouldBe False

        it "returns False for '10.0.0.1'" \_ ->
          Hostname.isLocalhost "10.0.0.1" |> shouldBe False

        it "returns False for '192.168.1.1'" \_ ->
          Hostname.isLocalhost "192.168.1.1" |> shouldBe False

        it "returns False for empty string" \_ ->
          Hostname.isLocalhost "" |> shouldBe False

        it "returns False for 'LOCALHOST' (case sensitive)" \_ ->
          -- The function is case-sensitive by design
          -- Callers should normalize to lowercase before calling
          Hostname.isLocalhost "LOCALHOST" |> shouldBe False

        it "returns False for '::1' (unbracket IPv6)" \_ ->
          -- IPv6 loopback must be bracketed for URI compatibility
          Hostname.isLocalhost "::1" |> shouldBe False

        it "returns False for 'local'" \_ ->
          Hostname.isLocalhost "local" |> shouldBe False

        it "returns False for 'host'" \_ ->
          Hostname.isLocalhost "host" |> shouldBe False
