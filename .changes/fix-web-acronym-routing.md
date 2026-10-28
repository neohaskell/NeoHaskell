---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

Web commands with acronym names now use the expected kebab-case URL. For example, `SaveIBAN` responds at `POST /commands/save-iban`, and OpenAPI lists the same path. Internal dispatch and MCP command names retain their original spelling.

If two different command names produce the same Web URL, service registration rejects the duplicate instead of choosing one command. Rename one of the commands before starting the service.
