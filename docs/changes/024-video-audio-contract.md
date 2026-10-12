# Change 024: Prove usable audio within the promised video byte limit

This pre-approval follow-up exposes real output-size and empty-audio regressions in merged Change018. No product implementation. It depends on Core subprocess Change023/PR911; running these tests on unchanged main does not flatten that dependency. The native pipeline currently accepts only main-based intake, so no false stack/base record is created here.

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

The existing promise is a usable first part no larger than `maxOutputBytes` (default25,000,000 decimal bytes). These tests preserve that default,180second timeout,16kHz mono and existing API. A real successful result must contain positive complete PCM frames and decode independently; file magic alone is not useful audio. This draft does not implement or approve D2-A exact usable-first-part versus soft-bound/rejection alternatives, format narrowing, workspace/reference policy or a canonical44byte header. Those are separate decisions in the retained proposal and require full criteria before final approval. If the chosen follow-up changes accepted formats/config/errors, declare its compatibility impact before implementation rather than hiding it under this repro's flags.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Real long audio respects the existing default exact binary-byte cap | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#keeps the promised exact default audio byte cap with real ffmpeg` | integration | subprocess:real |
| C2 | Real short audio returns positive PCM16 frames and independently decodes | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#returns nonempty independently decodable audio from real short media` | integration | subprocess:real |
| C3 | A zero-frame audio track does not become successful usable audio | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#rejects a real empty audio track instead of reporting success` | integration | subprocess:real |
| C4 | Non-media input reaches the error callback, not audio success | `hspec:nhintegrations-test:integrations/test/Integration/Video/ExtractAudioSpec.hs#rejects nonmedia input through the real ffmpeg boundary` | integration | subprocess:real |

Review principle: check complete real output, not merely a fake/header. The fixture validates RIFF length, PCM16 mono16kHz format, positive sample count and payload length, and asks real ffmpeg to decode the returned WAV. Tests generate only synthetic local input in private temporary fixtures. Existing application action/JSON/Base64 path is exercised using realRunner; this is not a full Application/upload/network acceptance test.

## User impact

No product change in this draft. The long synthetic input is900seconds/28,800,000 PCM bytes; short3seconds and empty0frames. Runtime tools ffmpeg and Python3 are required, never skipped. No paid/provider service or real user media. This test preparation does not claim private workspace isolation for production or a sandbox.

## ADR

Not required for the current no-API regression-only draft. Proposed ADR0081 and broader024 contract remain deferred until the actual format/compatibility choice; they are not approved here.

## Baseline and remaining gates

The unchanged-main full nhintegrations-test baseline ran once:621examples,2Oura.SyncAll failures,3pending. Both error-path fixtures accidentally invoke a real external token endpoint because refresh is not mocked. No rerun until fixture network ownership is corrected, no assertion weakening. New tests run exact video-only selectors with empty provider registry and local file access; no Oura/HTTP action path. Full release/025 gates remain red.

Core911 owns drains/cancel/reap; this draft does not duplicate them. Reference-policy negatives need a correctly isolated relative-reference fixture, not an absolute-path rejection that ffmpeg already performs for a different reason. Other containers, no-audio video, malformed output, workspace permissions/cleanup and configuration boundaries remain future executable coverage before any broader approval. No product implementation before Gate1 plus required design reviews.
