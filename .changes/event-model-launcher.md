---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Your application now declares its Event Model as a plain value. Update existing Event Model declarations, launchers, and tests that inspect the model. The launcher sets output buffering and reports invalid declarations before starting services. Your commands, queries, configuration, and stored events do not need to change.

## Migration

1. In your application module, change `Result Text EventModel.EventModel` to `EventModel`. Import `Core` for the type and configuration builders. For example, replace `EventModel.eventModel`, `EventModel.appName`, and `EventModel.features` with `eventModel`, `appName`, and `features`. Keep the existing builder body, app name, store factory, transports, Features, and permissions. Keep `Service.EventModel` imports in Feature modules and wherever you use validation or model inspection.

   The application declaration changes as follows. The omitted builder body stays unchanged.

   ```diff
   -appEventModel :: Result Text EventModel.EventModel
   -appEventModel = EventModel.eventModel do
   +appEventModel :: EventModel
   +appEventModel = eventModel do
   ```

2. In `launcher/Launcher.hs`, replace the old `Service.EventModel` import with the launcher import shown below. Remove the explicit stdout and stderr buffering calls and their unused `System.IO` import. Keep `import Core`, `import App (appEventModel)`, and `import Task qualified`.

   ```haskell
   import Service.EventModel.Launcher qualified as EventModel

   main :: IO ()
   main = appEventModel |> EventModel.run |> Task.runOrPanic
   ```

   The old `Service.EventModel.run :: Result Text EventModel.EventModel -> Task Text Unit` is removed. The replacement `Service.EventModel.Launcher.run :: EventModel -> Task Text Unit` validates the declaration before starting it.

   Upgrade the CLI and framework together. Neo adds `nhcore:launcher` to the generated executable dependencies. If you manage Cabal yourself, add `build-depends: nhcore:launcher` inside the executable stanza. Add the same dependency to any other component that imports `Service.EventModel.Launcher`. Keep the application's existing dependencies.

3. Validate declarations before inspecting them in tests or custom tooling. Replace `case appEventModel of` with `case EventModel.validate appEventModel of`, with `Service.EventModel qualified as EventModel` imported. Keep handling `Err problem` as a validation failure. The `Ok model` value now has type `EventModel.ValidatedEventModel`. Change explicit type annotations for already validated models to that type. Pass it to `EventModel.modelName`, `EventModel.modelFeatures`, `EventModel.toApplication`, and `Service.EventModel.RuleRunner.declaredRules`, `runRules`, or `specFor`. Keep validation tests asserting their existing expected errors.

4. Preserve your event store, database settings, storage directory, event tags, and JSON encoding. This launcher migration requires no stored-data rewrite and no switch to the new derived event declarations. Keep custom event encoders and decoders. If you also choose to adopt named event payloads, check historical event tags and field encoding separately. Test historical decoding and replay on a copy of existing data before changing those declarations. Report unknown compatibility instead of deleting or rewriting history.

### Verify

Run these commands in your upgraded application directory.

1. Run `neo build`. It must exit successfully with the new launcher import and dependency.
2. Run `neo test`. Existing tests must pass, including tests for invalid model declarations and any rule-runner tests.
3. Run `neo run`. Exercise an existing command and query. Confirm that the command succeeds and the query returns the expected state. Stop and restart the app with its existing persistent store, if it has one. Confirm that the query still returns the previously stored state.

If a command fails or historical decoding is unverified, retain the failing test or data and report the unresolved case. Include the file, diagnostic, command, and observed outcome.

## Agent prompt

```text
Migrate this existing NeoHaskell application's Event Model declaration and launcher. Preserve its domain behavior, app name, configuration, event store factory and directory, transports, Features, permissions, and custom event encoders and decoders. Do not replace the application with the Counter starter.

1. In the application module, replace the declaration type Result Text EventModel.EventModel with EventModel from Core. Replace configuration calls such as EventModel.eventModel, EventModel.appName, and EventModel.features with eventModel, appName, and features imported from Core. Keep the existing builder body. Keep Service.EventModel imports in Feature modules and wherever validation or inspection needs them.

2. In launcher/Launcher.hs, replace import Service.EventModel qualified as EventModel with import Service.EventModel.Launcher qualified as EventModel. Keep import App (appEventModel), import Core, and import Task qualified. Replace main with:
main :: IO ()
main = appEventModel |> EventModel.run |> Task.runOrPanic
Remove explicit stdout and stderr buffering calls and the unused System.IO import. The removed Service.EventModel.run accepted Result Text EventModel.EventModel. Service.EventModel.Launcher.run accepts the plain EventModel and validates it before startup. Upgrade the CLI and framework together. Neo generates the executable dependency nhcore:launcher. For a manually maintained Cabal file, add build-depends: nhcore:launcher inside the executable stanza. Add it to any other component importing Service.EventModel.Launcher. Preserve existing dependencies.

3. In tests and custom tooling, replace case appEventModel of with case EventModel.validate appEventModel of, using import Service.EventModel qualified as EventModel. Preserve Err handling and expected error assertions. Ok model is now EventModel.ValidatedEventModel. Update annotations for validated values. Pass that value to EventModel.modelName, EventModel.modelFeatures, EventModel.toApplication, and Service.EventModel.RuleRunner.declaredRules, runRules, or specFor.

4. Do not delete or rewrite stored events or change their tags, JSON fields, database settings, or storage directory. This migration does not require adopting derived named event payloads. Preserve custom codecs. If payload derivation is also requested, inspect historical encoding and test decoding and replay on a copy before changing event declarations. Report uncertain compatibility and leave the data intact.

5. In the upgraded application directory, run neo build and neo test. Both must exit successfully, including existing validation and rule tests. Run neo run and exercise an existing command and query. Confirm the expected state. For an existing persistent store, restart and confirm the old state is still queryable. Keep failing tests. Report changed files, the commands and their exit statuses, runtime results, and every unresolved case with its file and diagnostic. Identify checks you could not run. Do not claim success for a skipped or failing check.
```
