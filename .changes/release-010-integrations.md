---
group: platform
component: Integrations
impact: compatible
category: Added
---

## Summary

Connect app events to external services through the new `nhintegrations`
  package. Configure an integration's request and the commands to run on success
  or failure; the framework handles dispatch. These integrations are additions
  since 0.9.0, so there are no old integration imports to rename in a 0.9 app.

  - **HTTP and email:** call external HTTP endpoints with `Integration.Http`,
    send transactional email with `Integration.Brevo`, or use Azure Communication
    Services through `Integration.Acs`.
  - **AI conversations and tools:** use `Integration.OpenRouter` or
    `Integration.AzureAI` for chat completions. `Integration.Agent` lets a model
    call typed application commands as tools. Register only the commands and
    permissions you intend the agent to have.
  - **Documents and recordings:** `Integration.Pdf.ExtractText` extracts text
    from digital PDFs with `pdftotext`; `Integration.Ocr.Ai` extracts text from
    documents/images through a selected multimodal model. `Integration.Audio.Transcribe`
    supports WAV, MP3, M4A and OGG recordings. AI extraction/transcription sends
    content to the selected provider through OpenRouter. Configure the MIME type
    and model for the actual input; large-file chunking and streaming transcription
    are not supplied. For long transcription requests, make the integration
    dispatcher's timeout at least as long as the request timeout.
  - **Oura:** connect an Oura account with OAuth2 and access typed sleep, activity,
    readiness and other Oura API v2 data through `Integration.Oura`.

  Add `nhintegrations` to your app's dependencies, configure the chosen service's
  credentials and required local tools, and register its outbound handler. Use
  an isolated test account or a fake integration to check the success and failure
  commands before sending real emails or changing external data. Also test token
  refresh where applicable. Provider accounts and usage are separate from a
  Codex subscription. The release process itself requires no paid AI API.
