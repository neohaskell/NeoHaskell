---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

Secure URL validation checks both IPv4 and IPv6 DNS records under one timeout. When the timeout expires, it stops both lookups. Existing calls to `validateSecureUrlWithDns` keep the same type and validation errors.
