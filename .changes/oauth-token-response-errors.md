---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

When an OAuth2 provider rejects a token request, your app now receives the
error that describes the rejection. Before this fix, a rejection such as an
expired refresh token or a reused authorization code crashed the request with
an uncaught JSON exception, so your `onFailure` callback and your token refresh
error handling never ran.

The errors are the existing `OAuth2Error` values:

- `InvalidGrant`, `InvalidClient`, and `ScopeDenied` when the provider returns a
  standard OAuth2 error. The message is the provider's description. If the
  provider sends no description, the message is the OAuth2 error code.
- `TokenRequestFailed` when the provider returns another error status. The
  message names the HTTP status only.
- `MalformedResponse` when the provider reports success and the response is not
  a valid token response.
- `NetworkError` when the provider cannot be reached.

This applies to the authorization code exchange and to the token refresh. Error
messages never contain the provider's raw response, so tokens cannot leak into
your logs through them. A token response larger than 10 MB now returns
`NetworkError`.

Existing applications do not need code changes. To check your app, connect an
account, revoke the app's access at the provider, and trigger a token refresh.
Your app receives a `RefreshFailed` error and does not crash. Most providers
report a revoked refresh token as `InvalidGrant`.

Framework code that imports `Http.Client.Internal` can also call the new
`postFormRaw`. It sends the form request that `Http.postForm` sends and returns
the status code and the undecoded body of every response.
