---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

The Neo CLI now uses an updated TLS library that rejects malformed TLS 1.3 handshake messages sent at the wrong encryption level. This hardens HTTPS connections. Existing commands and configuration need no changes.
