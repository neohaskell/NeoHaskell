---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Application modules can describe their Event Model with an ordinary value from Core. The executable launcher handles output buffering and reports validation failures before starting services.

## Migration

Change the type of your application's declaration from `Result Text EventModel.EventModel` to `EventModel`. Import `Core` for model configuration. Keep importing `Service.EventModel` for Feature declarations. Keep your existing app name, store, transports, Features and permissions.

In the executable launcher, import `Service.EventModel.Launcher qualified as EventModel` and run `appEventModel |> EventModel.run |> Task.runOrPanic`. Remove explicit stdout and stderr buffering from the launcher. Neo generates the executable dependency on `nhcore:launcher`; add that dependency to the executable stanza if you manage Cabal yourself.

Tests that previously matched on `appEventModel` should match on `Service.EventModel.validate appEventModel`. Its successful result can be passed to the rule runner or lowered with `toApplication`. Existing persisted event sums can keep their current encoders. When switching an existing sum to derived named payloads, verify its historical tags and field encoding first.

### Verify

Run `neo build` and `neo test` in the upgraded project. Start the executable with `neo run` and exercise an existing command and query. Check validation tests still report the expected model errors.

## Agent prompt

```text
Upgrade this application's Event Model and launcher together. Keep the existing domain behavior and configuration. Change the model declaration to EventModel, import its configuration builders from Core, and move the invocation of run to Service.EventModel.Launcher in the executable. Update the executable dependency and any tests that inspect the validated declaration. Preserve old stored event decoding. Run neo build and neo test. Start the application and exercise an existing command and query. Report the changed files and observed outcomes.
```
