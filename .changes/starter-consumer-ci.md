---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

A newly generated project now uses framework APIs that match the framework
revision pinned by its starter. Generate a project with `neo --ci new my-app`,
then run `neo --ci build` and `neo --ci test` from its directory. Both commands
should pass. Existing applications do not need edits for this fix.
