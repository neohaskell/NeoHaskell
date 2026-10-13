# Change 024: Return usable video audio within the promised byte limit

Video extraction returns usable audio within the configured byte limit. This main-based follow-up repairs the existing guarantees from merged Change018 without changing public signatures, defaults or accepted input formats. Core Change023/PR911 remains an explicit subprocess lifecycle dependency; develop this video repair independently, then integrate that fix forward before final verification. No merge or release is authorized.

```yaml spec
issue: adhoc:video-audio-contract
kind: bug
touches: [outbound-integrations]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
```

The existing promise is a usable first part no larger than `maxOutputBytes` (default25,000,000 decimal bytes). These tests preserve that default,180second timeout,16kHz mono and existing API. A real successful result must contain positive complete PCM frames and decode independently; file magic alone is not useful audio. Nick’s October9 review explicitly requested enforcing exact size and usable audio or narrowing the promise; the user’s preserve-promises instruction selects exact usable first-part output. NickSeagull approved audited fixes to existing guarantees on October13 in https://github.com/neohaskell/NeoHaskell/pull/907#issuecomment-6150729681. Native continuation is recorded after this follow-up has a PR-bound red receipt. No soft-bound fallback or accepted-format narrowing is selected. A standard PCM WAV minimum is44 header bytes plus complete sample frames; it is a representability boundary, not a requirement to discard valid optional chunks when the cap has room. Validate the WAV structure, PCM frame alignment and lengths before success. Preserve a valid in-cap WAV byte-for-byte; for an over-cap output, rebuild a valid header around its earliest complete PCM16 frames. A minimum-budget extraction must still decode enough frames rather than trusting ffmpeg -fs to produce audio within the header budget. Invalid or empty output reaches onError.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Real long audio respects the existing default exact binary-byte cap | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#keeps the promised exact default audio byte cap with real ffmpeg` | integration | subprocess:real |
| C2 | Real short audio returns positive PCM16 frames and independently decodes | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#returns nonempty independently decodable audio from real short media` | integration | subprocess:real |
| C3 | A zero-frame audio track does not become successful usable audio | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#rejects a real empty audio track instead of reporting success` | integration | subprocess:real |
| C4 | Non-media input reaches the error callback, not audio success | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#rejects nonmedia input through the real ffmpeg boundary` | integration | subprocess:real |
| C5 | Real bounded output contains exactly the beginning of source PCM | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#preserves the first complete source frames in capped real audio` | integration | subprocess:real |
| C6 | Configured decimal cap32044 is enforced on usable real output | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#enforces a configured exact cap on usable real audio` | integration | subprocess:real |
| C7 | Minimum monoPCM16 budget46 contains one playable frame | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#returns a usable first frame when the cap permits only one PCM frame` | integration | subprocess:real |
| C8 | Budget45 cannot be successful usable monoPCM16 WAV | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#rejects a cap smaller than a WAV header and one complete PCM frame` | integration | subprocess:real |
| C9 | Malformed output cannot be accepted solely because process exit is0 | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#rejects malformed output even when the process reports success` | unit | none |

Review principle: check complete real output, not merely a fake/header. The fixture validates RIFF length, PCM16 mono16kHz format, positive sample count and payload length, and asks real ffmpeg to decode the returned WAV. Tests generate only synthetic local input in private temporary fixtures. Existing application action/JSON/Base64 path is exercised using realRunner; this is not a full Application/upload/network acceptance test.

## User impact

Public input/output types, defaults and accepted media formats stay unchanged. The long synthetic input is900seconds/28,800,000 PCM bytes; short3seconds and empty0frames. Runtime tools ffmpeg and Python3 are required, never skipped. No paid/provider service or real user media. Temporary media moves into a securely created owner-only workspace with whole-directory cleanup. This repairs the demonstrated shared-directory sibling exposure, not arbitrary-file references or a general ffmpeg sandbox. No new format/protocol allowlist is introduced. Raw subprocess diagnostics are not returned to commands; fixed short category messages preserve the existing missing-tool, file-access and timeout meanings.

## ADR

Not required: no new public API, dependency or input-format restriction. Proposed ADR0081 and its format allowlist are not part of this repair.

## Baseline and remaining gates

The unchanged-main full nhintegrations-test baseline ran once:621examples,2Oura.SyncAll failures,3pending. Both error-path fixtures accidentally invoke a real external token endpoint because refresh is not mocked. No rerun until fixture network ownership is corrected, no assertion weakening. New tests run exact video-only selectors with empty provider registry and local file access; no Oura/HTTP action path. Full release/025 gates remain red.

Core911 owns drains/cancel/reap; this draft does not duplicate them. Reference-policy negatives need a correctly isolated relative-reference fixture, not an absolute-path rejection that ffmpeg already performs for a different reason. C9 is deliberately a fake-runner malformed-output branch check, not subprocess:real. Reference restrictions that would reject previously accepted formats are isolated for a separate compatibility decision and do not block the exact-cap repair. Workspace/privacy coverage is added before those repairs. No product implementation before native Gate1 plus local security design review.

## Fixture reconciliation and implementation plan

The old268-byte fake WAV has an invalid RIFF size and lacks fmt/data chunks. Replace only its fixture bytes with a valid268-byte PCM16 WAV (44-byte header plus224-byte binary payload). Preserve every existing equality,268-byte count,MIME,argument and cleanup assertion. C9 continues deliberately returning malformed four-byte RIFF data. This corrects the test setup to the existing usable-WAV guarantee; it does not weaken the output test.

Files: existing `Integration/Video/ExtractAudio/Internal.hs` and facade; existing `ExtractAudioSpec.hs`; scoped dependency/HLint declarations only if required by the native secure-temp primitive. Copy-adapt the existing conversion/error/task pipeline and the existing scenario/real-media helpers. Resolve Bytes slices/pack/unpack, Task validation/finally and native secure-temp APIs through codemap/API lookup. No new public symbol or product constant/default is introduced. ffmpeg -fs remains a coarse process stop, never the final byte-bound proof.
