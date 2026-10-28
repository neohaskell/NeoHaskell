---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

JSON text decoding now preserves names and messages with accented characters, ñ, Japanese text, and emoji. Existing apps need no code changes. For example, decoding `"niño"` now returns `niño` instead of an error.
