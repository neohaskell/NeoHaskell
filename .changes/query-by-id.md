---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Read one query row with `GET /queries/{query-name}/{id}` using the UUID chosen by the query's `queryId`. A successful read returns the row as JSON. Missing rows and rows hidden by `canView` return the same HTTP 404 response. Query access checks still run before storage access. Existing query declarations need no changes, and collection endpoints remain available.
