---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

An inbound worker can now emit several command types, including commands for different entities. List each command and its target in a typed worker, then register the handler with `Application.withInboundHandler`. The compiler rejects undeclared commands and wrong targets. Startup reports a missing receiving service before the worker runs. Existing `Integration.inbound`, `Timer.every`, and `Application.withInbound` calls keep working.
