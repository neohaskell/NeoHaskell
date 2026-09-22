---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

Applications can use the framework's declaration helpers without adding language
pragmas to their source files. Neo now enables the required deriving strategy in
the project settings it generates for application, library, and test components.

After updating Neo, run `neo build` and `neo test` from your application directory.
Neo refreshes the generated settings; no manual Cabal configuration is needed.
