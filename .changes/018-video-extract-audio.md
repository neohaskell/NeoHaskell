---
group: platform
component: Integrations
impact: compatible
category: Added
---

## Summary

You can now pull the audio out of an uploaded video with the new
`Integration.Video.ExtractAudio` integration. Give it the video's file
reference and it returns mono 16 kHz WAV bytes (`audio/wav`). Upload those
bytes as a new file, save the returned `FileRef`, and pass that reference to
`Integration.Audio.Transcribe` with `mimeType = "audio/wav"`. By default,
extraction allows 180 seconds and stops at 25 MB; both limits are in the
config. It needs `ffmpeg` on the machine that runs your app (for example
`brew install ffmpeg`). If `ffmpeg` is missing, the action fails with a
message that tells you to install it. If the video itself cannot be
converted, your `onError` command receives the reason.

The dispatcher's `eventProcessingTimeoutMs` must cover video retrieval and
extraction. Raise it above the extraction timeout and expected retrieval time.
For example, `Just 200000` may fit a 180-second extraction, but it is not a
guarantee for every upload. The 30-second default can stop a slow operation
before your `onSuccess` or `onError` command is sent.
To keep the audio in a command, store it as Base64 text
(`result.audio |> Bytes.toBase64 |> Text.fromBytes`). The new
`Bytes.fromBase64` turns it back into the exact audio bytes
(`audioBase64 |> Text.toBytes |> Bytes.fromBase64`) and returns an error
instead of different bytes if the text is not valid Base64.

Existing applications do not need any changes. From a NeoHaskell checkout,
run `./.agents/skills/tdd/scripts/test-match 'Integration.Video' nhintegrations-test`
and confirm the extraction examples pass. They use a stand-in for `ffmpeg`,
so you do not need it installed to run them.
