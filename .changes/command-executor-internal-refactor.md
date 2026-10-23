---
group: platform
component: Framework
impact: none
category: Improved
---

## Summary

The framework now keeps command attribution, execution outcomes, and result reporting in smaller internal modules. Applications still define commands with the `command` marker and register them with `Service.command`. They add services with `Application.withService`. These application APIs and the command response JSON are unchanged. App authors need no code or data migration. This internal refactor does not need a release announcement.
