---
group: platform
component: IDE
impact: compatible
category: Added
---

## Summary

View and edit your app's event model in the browser with `neo ide`. The IDE is
bundled into the Neo CLI, opens locally at `127.0.0.1:2323` by default, and
saves the diagram in `event-model.json`. It can show commands, events, queries
and their connections, validate references, and synchronize information from
source files. This brings the IDE already available in the independent Neo
CLI series into the coordinated NeoHaskell release.

From the project root, run `neo ide` and open its printed URL. Review autosaved
changes before committing the model. You can keep working in your usual source
editor; adopting the diagram is optional.

Use `neo validate` to check an existing model without opening the browser;
expect exit code 0 for a valid file. `neo inspect domains` and
`neo inspect wiring` show what Neo can discover from your source. When you
intend to update the model from code, run `neo inspect sync` and review the
resulting diff: that command writes to `event-model.json`. A missing or invalid
model makes validation fail; it is not treated as a successful check.
