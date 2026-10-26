---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

`Http.Client` can now send PUT, HEAD, DELETE, POST and PATCH requests with a raw byte body and read the status code and headers of every response, including 4xx and 5xx. Use `Http.Client.sendSecure method request body` for HTTPS services and `Http.Client.Internal.sendRaw` for trusted loopback endpoints. `Http.getSecure` and `getRaw` keep working exactly as before.

HTTPS byte requests load the system certificate store while keeping certificate
and hostname checks and TLS 1.2 or later. Byte-level requests ignore environment
proxies and stop retaining response chunks as soon as the configured size limit
is exceeded, closing the response instead of waiting for the entire body.
The limit is not a fixed process-memory cap; network buffers can contain extra
bytes. The raw internal API remains caller-restricted, not a loopback validator.

Existing applications do not need code changes. To verify locally, run `./dev test "Http.Client" nhcore-test-core` and confirm the `sendRaw` and `sendSecure` examples pass.
