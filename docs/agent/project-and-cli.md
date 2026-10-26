# Project and CLI

`neo.json` is the input to project reconciliation. `neo build`, `neo run`, and `neo test` reconcile before their main operation. Reconciliation discovers source modules and generates Cabal and Nix files. Edit `neo.json` for package settings and dependencies rather than relying on edits to generated build files.

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

`neo new` without a name opens an interactive interview. `neo new my-library --library --ci` creates a library project.

## Application files

`src/App.hs` composes the application. Domain modules live under `src/`. `launcher/Launcher.hs` runs the executable application. Hspec specifications and Hurl scenarios live under `tests/`.

`neo build --watch`, `neo run --watch`, and `neo test --watch` select watch mode. `--ci` disables interactive prompts and animations.

Run direct Haskell tools from the generated project root through its Nix shell.

```sh
nix develop --command cabal build
```

The generated flake and Cabal files define that toolchain. Use [testing](testing.md) for the project test command.
