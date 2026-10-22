---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

Concurrent commands that update the same entity now use the entity revision they actually read. If another command writes first, the stale command retries with fresh state instead of appending an outdated event. New-entity commands also prevent duplicate stream creation, while unconditional appends keep their existing behavior.

Existing applications do not need code or data migrations. To verify the fix locally, run `./dev test 'Retry Logic' nhcore-test-service` and confirm the command-handler retry examples pass for the in-memory and PostgreSQL backends.
