# NeoHaskell VSCode Extension (Phase 0)

This package provides the initial NeoHaskell extension scaffold for VSCode.

## Language Configuration Subtleties

- `<` and `>` are intentionally excluded from `brackets` to avoid false bracket matching in expressions like `a < b`.
- `<` and `>` are included in `autoClosingPairs` so generic syntax like `Result<ok, err>` auto-closes.
- `autoCloseBefore` is set to `;:.,=}])>` followed by backtick, space, newline, and tab to allow generic auto-close behavior in common contexts.
- `/**` has its own `autoClosingPairs` entry separate from `/*`, so typing doc comments closes as expected.
- `wordPattern` includes apostrophes to support tick-marked identifiers like `x'`.
