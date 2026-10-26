---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

`AsyncTask.runWithCancellationEnabled` starts a child that can receive cancellation while its parent protects a handoff. It replaces `AsyncTask.runUnmasked`. Apps that call the old name need to update that call.

## Migration

Replace `AsyncTask.runUnmasked task` with `AsyncTask.runWithCancellationEnabled task`. The child still runs with cancellation enabled even when its parent starts it during a protected handoff. Keep ordinary `AsyncTask.run` calls when the child should inherit the parent's cancellation state.

### Verify

Search the app for `runUnmasked` and update each call. Run `neo build` and `neo test --ci` in the app. Both commands should finish successfully, and the search should find no old calls.

## Agent prompt

```text
In this NeoHaskell app, replace every AsyncTask.runUnmasked call with AsyncTask.runWithCancellationEnabled. This is an API rename: the child still receives cancellation while its parent protects a handoff. Do not replace ordinary AsyncTask.run calls. Search for remaining runUnmasked uses, run neo build and neo test --ci, and report the changed files and command results. If a call relies on a different cancellation contract, report it instead of changing that call automatically.
```
