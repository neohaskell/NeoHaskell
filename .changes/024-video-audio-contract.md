---
group: platform
component: Integrations
impact: compatible
category: Fixed
---

## Summary

Video audio extraction now returns playable audio within `maxOutputBytes`, including the default 25,000,000-byte limit. Long videos keep their first complete audio frames. Empty or malformed audio reaches `onError` instead of appearing successful.

Temporary media uses an owner-only folder on Linux and macOS, removed when extraction exits. Error commands no longer include temporary paths or raw ffmpeg diagnostics. This does not sandbox ffmpeg or restrict accepted media formats.

No application changes are needed. Extract a long video and check that the returned WAV plays and its byte length does not exceed your configured limit. Keep the documented dispatcher timeout above the extraction timeout.
