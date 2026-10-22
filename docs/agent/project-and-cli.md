# Project and CLI

`neo.json` is the input to project reconciliation. `neo build`, `neo run`, and `neo test` reconcile before their main operation. Reconciliation discovers source modules and generates Cabal and Nix files. Edit `neo.json` for package settings and dependencies rather than relying on edits to generated build files. See the reconciler in `neo/src/reconcile/mod.rs`, dependency resolver in `neo/src/reconcile/resolve.rs`, and configuration type in `neo/src/config.rs`.

## Create and run an application

1. Create the project.

   ```sh
   neo new my-app --ci
   ```

2. Enter the generated directory.

   ```sh
   cd my-app
   ```

3. Build the application.

   ```sh
   neo build --ci
   ```

4. Start the application.

   ```sh
   neo run --ci
   ```

The CLI argument definitions in `neo/src/cli.rs` own these commands. `neo new` without a name opens an interactive interview. `neo new my-library --library --ci` creates a library project. The creation command in `neo/src/commands/new.rs` owns the generated layout.

## Application files

`src/App.hs` composes the application. Domain modules live under `src/`. `launcher/Launcher.hs` runs the executable application. Hspec specifications and Hurl scenarios live under `tests/`. The starter tree in `neo/starter` provides the reference layout.

`neo build --watch`, `neo run --watch`, and `neo test --watch` select watch mode. `--ci` disables interactive prompts and animations. See CLI options in `neo/src/cli.rs` and the watch implementation in `neo/src/commands/watch_common.rs`.

Run direct Haskell tools from the generated project root through its Nix shell.

```sh
nix develop --command cabal build
```

The generated flake template in `neo/assets/templates/flake.nix.j2` and Cabal template in `neo/assets/templates/project.cabal.j2` define that toolchain. Use [testing](testing.md) for the consumer test command.
