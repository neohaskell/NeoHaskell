---
title: Extract document text and transcribe audio
description: Turn attachments into reviewable information without treating extracted text as fact.
sidebar:
  order: 6
---

A PDF or audio recording contains information you want to use in an application. Extraction can turn it into searchable text or a reviewable draft, while the original attachment remains the evidence. Deciding whether the result is accurate enough is a separate step.

NeoHaskell includes local PDF text extraction, AI-assisted document extraction, and audio transcription. Local extraction keeps that processing on your server. AI paths send file content to an external provider and add cost, latency, and accuracy considerations.

Prerequisites: [file uploads](/connect/files/), [integration lifecycle](/connect/), and an outcome command for recording results. For practice, use a sample PDF describing mugs and extract draft catalogue information from it.

## Start with a digital PDF

This **partial builder** uses the real PDF API. `attachment`, `recordExtraction`, and `recordFailure` are your application values. The two callbacks produce one registered command type.

```haskell
import Integration qualified
import Integration.Pdf.ExtractText qualified as PdfExtract
import Integration.Pdf.ExtractText.Internal ()

Integration.outbound PdfExtract.Request
  { fileRef = attachment
  , config = PdfExtract.defaultConfig
      { PdfExtract.layout = PdfExtract.PreserveLayout
      , PdfExtract.pageRange = Just (1, 2)
      }
  , onSuccess = recordExtraction
  , onError = recordFailure
  }
```

The explicit instance import is necessary with this module layout. Register the enclosing handler as shown in [workflows](/connect/workflows/).

Install `pdftotext` and `pdfinfo` in the application's runtime environment. The integration retrieves file bytes, writes a temporary PDF, runs those tools, and returns text plus page count and optional metadata. `PreserveLayout` retains positioning, `RawText` removes that layout preference, and `Table` uses a fixed-pitch extraction option. The result is text, not parsed records such as products or document entries.

If metadata extraction fails but text extraction succeeds, the current implementation can return page count `0` and metadata `Nothing`. That means unavailable metadata, not necessarily a zero-page document.

A scanned page may have no selectable text. Local PDF extraction is not OCR. Check empty output before treating extraction as a useful application result.

## Use AI when the content needs interpretation

`Integration.Ocr.Ai.Request` takes `fileRef`, `mimeType`, `model`, `config`, `onSuccess`, and `onError`. Import `Integration.Ocr.Ai.Internal ()` to bring its execution instance into scope.

The configuration offers `FullText`, `Summary`, and `Structured` extraction modes. `Structured` changes the prompt; it does not turn the returned `Text` into validated application data. Parse the result and apply the same rules you would apply to human input; in the example, those are the product rules.

Choose a currently supported model for your file type and supply `OPENROUTER_API_KEY`. The adapter submits the full file as an attachment. `maxPages` is an instruction in the prompt, not a payload truncation mechanism or a hard spending limit. The implementation currently returns `Nothing` for both `pageCount` and `confidence`.

## Add audio only when it solves a real need

For recorded notes, `Integration.Audio.Transcribe.Request` uses the same file-reference pattern. Import `Integration.Audio.Transcribe.Internal ()` for execution. Its configuration includes a language hint and `maxDurationSeconds`; the latter asks the model to limit transcription but still uploads the full file.

The current result supplies transcript text while `duration`, `confidence`, and `language` are all `Nothing`. There is no chunked transcription or streaming in this implementation. Verify the selected provider/model accepts the actual attachment encoding and media type before building a workflow around it.

## Budget the entire operation

The default integration dispatcher timeout is 30 seconds. OCR defaults to a 120-second request timeout and audio to 180 seconds. A longer request timeout alone cannot extend the enclosing event-processing timeout.

This **application wiring fragment** gives the overall event work four minutes; tune it to measured behaviour and concurrency needs:

```haskell
import Service.Integration.Dispatcher qualified as Dispatcher

    |> Application.withDispatcherConfig @()
        (\_ -> Dispatcher.defaultConfig
          { Dispatcher.eventProcessingTimeoutMs = Just 240000 })
```

Some preparation failures—disabled file uploads, missing files, or a missing PDF executable—raise integration errors before the result callback. Monitor runtime failures as well as outcome commands; otherwise a document can remain “processing” indefinitely.

## Exercise: a wrong product dimension

Extend the practice project with a workflow where extracted dimensions require review before publication. Play both roles: upload the sample document, then inspect the proposed values before approving them.

<details>
<summary>Suggested checks</summary>

Check a clean digital PDF, a scan, an empty extraction, a missing file, an unavailable executable, a timeout, malformed structured text, and a plausible but wrong dimension. Preserve the original attachment and processing attempt identity. Approving a draft should be a separate command with its own rules; the model's confidence is not available from these adapters.

</details>

Next, [use AI for application features](/connect/ai/) with the same separation between a generated suggestion and accepted application data.

## Implementation and examples

- [integrations/Integration/Pdf/ExtractText.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Pdf/ExtractText.hs)
- [integrations/Integration/Pdf/ExtractText/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Pdf/ExtractText/Internal.hs)
- [integrations/Integration/Ocr/Ai.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Ocr/Ai.hs)
- [integrations/Integration/Ocr/Ai/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Ocr/Ai/Internal.hs)
- [integrations/Integration/Audio/Transcribe.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Audio/Transcribe.hs)
- [integrations/Integration/Audio/Transcribe/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Audio/Transcribe/Internal.hs)
- [core/service/Service/Application.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application.hs)
- [core/service/Service/Integration/Dispatcher.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Integration/Dispatcher.hs)
- [testbed/src/Testbed/Examples/PdfExtraction.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Examples/PdfExtraction.hs)
