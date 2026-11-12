---
group: platform
component: Framework
impact: none
category: Improved
---

## Summary

Framework users need no app changes. A trusted main-branch workflow now realizes and publishes the Linux CI and default shells and the macOS default shell to Cachix. Framework contributors can list local test jobs and run the documented ADR index check through the Nix-packaged `act` command. The testbed job uses its existing Nix-provided Hurl and no longer installs an unused system package that fails on Ubuntu 26.04. These CI and contributor-tool changes do not affect application behavior or public APIs.
