# Mug-shop documentation checkpoints

These are the source files taught by the human Build documentation. They are
application files for a project created with `neo new mug-shop`; the reader does
not need the framework repository or its development tools.

- `first-cart/`: complete first-cart lesson (creation, empty summary, tests).
- `src/` and `tests/`: end of Build (Cart additions, Stock, configuration, tests).
- `persistence/`: optional PostgreSQL overlay maintained with the persistence lesson.

## Reproduce a checkpoint

Use the Neo CLI build accompanying this documentation change and run
`neo new mug-shop` from a temporary parent
directory. Enter the generated project. Keep `neo.json`, `launcher/`, and the
build setup. Remove the supplied `src/Starter` and Counter-specific test files;
keep `tests/Spec.hs`. Add the checkpoint's `src` and `tests` contents at the same
relative paths, replacing `src/App.hs`.

For an automated verification checkout it is also safe to replace the generated
`tests/` directory with the checkpoint's complete `tests/` directory. Do this only
in a disposable, freshly generated project.

Run from the generated project:

```sh
neo build
neo test
neo run
```

Stop `neo run` before `neo test`; the CLI starts its own HTTP server on port 8080.
For the end-of-Build checkpoint, leave `PERSIST_EVENTS` unset (default false).
Both checkpoints deliberately start with an in-memory event store and local
public access policies. End-of-Build has an internal ReserveStock decision but
no reservation integration yet; use the following Connect lessons to add it.

## API compatibility and provenance

The verification projects use framework revision
`25bd7027a85b8f2999602f66b46b4b6b133c22e8`. The CLI must supply
`DerivingStrategies` through its generated compiler configuration; the stock
0.10.0 CLI lacks that setting. This change corrects the CLI preset. Application
modules deliberately contain no language pragmas.

The files copy-adapt the public Cart/Stock domain APIs, starter application
composition, and starter decision/Hurl testing patterns. Events and commands
use their derivation markers; queries place `deriveQuery` before `QueryOf`.
Each domain separates its Entity, Event, and individual Events modules; Core is
a small re-export facade. Cart entries use `stockId` and `quantity` directly. Every accepted addition
appends one entry, even when its quantity is more than one.

The docs manifest ties focused teaching excerpts to these complete files. The local
verification record is reported with the documentation change; do not treat
source presence alone as a passing build or an operational deployment.
