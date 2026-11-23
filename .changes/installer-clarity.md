---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

The installer explains when an existing `neo` or `neoctl` command blocks installation and identifies its path without replacing it. Installation, update, rollback, and uninstall messages explain the outcome and the next safe action in plain language. Installation errors remain visible when terminal cleanup also fails, and repeated restoration warnings appear only once.

The download launcher distinguishes its report from the installer's report. It no longer suggests an automatic retry after the installer has started. Noninteractive piped installation no longer passes the remaining shell script to the installer as input.
