---
group: platform
component: CLI
impact: none
category: Improved
---

## Summary

Maintainers can build the native installer entrypoint separately from Neo and
prepare a Nix output containing exactly `neo` and `neoctl`. The native entrypoint
currently provides help and version only. Both package outputs use the platform
version and exclude Intel macOS from the installer target set.
