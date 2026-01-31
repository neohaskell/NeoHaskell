module Service.Transport.Web.SwaggerUISpec where

import Basics
import Service.Transport.Web.SwaggerUI qualified as SwaggerUI
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "SwaggerUI Security" do
    describe "XSS Prevention" do
      it "escapes script tags in API title" \_ -> do
        let maliciousTitle = "<script>alert('XSS')</script>"
        let html = SwaggerUI.scalarHtml maliciousTitle
        -- Should NOT contain raw script tag
        (html |> Text.contains "<script>alert") |> shouldBe False
        -- Should contain escaped version
        (html |> Text.contains "&lt;script&gt;") |> shouldBe True

      it "escapes HTML entities in API title" \_ -> do
        let titleWithEntities = "API & Documentation <test>"
        let html = SwaggerUI.scalarHtml titleWithEntities
        -- Ampersand should be escaped
        (html |> Text.contains "API &amp; Documentation") |> shouldBe True
        -- Angle brackets should be escaped
        (html |> Text.contains "&lt;test&gt;") |> shouldBe True

      it "escapes quotes in API title" \_ -> do
        let titleWithQuotes = "API \"test\" with 'quotes'"
        let html = SwaggerUI.scalarHtml titleWithQuotes
        -- Double quotes should be escaped
        (html |> Text.contains "&quot;test&quot;") |> shouldBe True
        -- Single quotes should be escaped
        (html |> Text.contains "&#x27;quotes&#x27;") |> shouldBe True

      it "handles img onerror XSS attempt" \_ -> do
        let xssPayload = "<img src=x onerror=alert(1)>"
        let html = SwaggerUI.scalarHtml xssPayload
        -- Should NOT contain raw img tag
        (html |> Text.contains "<img") |> shouldBe False
        -- Should contain escaped version
        (html |> Text.contains "&lt;img") |> shouldBe True

    describe "CDN Security" do
      it "includes SRI integrity hash" \_ -> do
        let html = SwaggerUI.scalarHtml "Test API"
        (html |> Text.contains "integrity=\"sha384-") |> shouldBe True

      it "includes crossorigin attribute" \_ -> do
        let html = SwaggerUI.scalarHtml "Test API"
        (html |> Text.contains "crossorigin=\"anonymous\"") |> shouldBe True

      it "pins specific version of Scalar" \_ -> do
        let html = SwaggerUI.scalarHtml "Test API"
        -- Should NOT use unversioned URL
        (html |> Text.contains "npm/@scalar/api-reference\"") |> shouldBe False
        -- Should include version number
        (html |> Text.contains "@scalar/api-reference@1.25.0") |> shouldBe True

    describe "HTML Structure" do
      it "generates valid HTML document" \_ -> do
        let html = SwaggerUI.scalarHtml "My API"
        (html |> Text.contains "<!DOCTYPE html>") |> shouldBe True
        (html |> Text.contains "<html lang=\"en\">") |> shouldBe True
        (html |> Text.contains "</html>") |> shouldBe True

      it "includes proper meta tags" \_ -> do
        let html = SwaggerUI.scalarHtml "My API"
        (html |> Text.contains "charset=\"UTF-8\"") |> shouldBe True
        (html |> Text.contains "viewport") |> shouldBe True

      it "configures Scalar to fetch from /openapi.json" \_ -> do
        let html = SwaggerUI.scalarHtml "My API"
        (html |> Text.contains "data-url=\"/openapi.json\"") |> shouldBe True

    describe "Edge Cases" do
      it "handles empty title" \_ -> do
        let html = SwaggerUI.scalarHtml ""
        (html |> Text.contains "<title> - API Documentation</title>") |> shouldBe True

      it "handles very long title" \_ -> do
        let longTitle = Text.repeat 1000 "a"
        let html = SwaggerUI.scalarHtml longTitle
        -- Should still produce valid HTML
        (html |> Text.contains "</html>") |> shouldBe True

      it "handles unicode in title" \_ -> do
        let unicodeTitle = "API 日本語 中文 한국어 🚀"
        let html = SwaggerUI.scalarHtml unicodeTitle
        -- Unicode should pass through (only HTML special chars escaped)
        (html |> Text.contains "日本語") |> shouldBe True
        (html |> Text.contains "🚀") |> shouldBe True
