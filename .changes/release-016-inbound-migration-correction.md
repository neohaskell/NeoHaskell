---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

Correct the unreleased 0.16.0 migration instructions for typed inbound workers. An Event Model Slice already registers its worker when the application starts through `EventModel.run`. An application assembled directly with `Application` still needs explicit worker registration. This note correction changes no runtime behavior.
