# Bundle Authoring — Phase 0

This document contains the operational content for the missing context bundles in the Atomic Orchestration design.

## 3.4.11 `<ghc_internals>`

Used only in `core/core/` modules to bridge NeoHaskell types to GHC/Base.

1. **Monad Transformers**: `ExceptT` is the underlying error carrier for `Task`. Use `Except.throwE` and `Except.runExceptT`.
2. **IO Bridge**: Raw `IO` is allowed here. Use `Monad.liftIO` to bring `IO` into the `ExceptT` stack.
3. **Vector Ops**: `Array` wraps `Data.Vector.Vector`. Use `Data.Vector.fromList`, `Data.Vector.map`, `Data.Vector.indexed`, etc.
4. **Text Bridge**: `Text` is `Data.Text.Internal`. Use `Data.Text.IO` for raw output if needed in `Task.runMain`.
5. **Exceptions**: Use `Control.Exception` for `SomeException`, `SomeAsyncException`, `finally`, and `try`.
6. **Unsafe Escapes**: `unsafeCoerce` and `unsafePerformIO` are allowed only with a safety comment and `{-# NOINLINE #-}`.
7. **Prelude Shadowing**: `import Prelude qualified` is mandatory to avoid collisions with NeoHaskell's `Core`.

**Patterns:**
- `newtype Task err value = Task { runTask :: (ExceptT err IO) value }`
- `newtype Array a = Array (Data.Vector.Vector a)`
- `result <- io |> Exception.try @exception |> Monad.liftIO |> Task`

---

## 3.4.12 `<bridge_layer>`

Used in `core/Core.hs` and boundary modules to translate between internal GHC types and public NeoHaskell types.

1. **Re-exports**: Every core type must be exported unqualified, but its module qualified.
2. **Naming Alignment**: Ensure `Either` -> `Result`, `[]` -> `Array`, `IO` -> `Task` is consistent.
3. **Conversion Functions**: Provide `fromEither`, `toEither`, `fromIO`, `runResult`, `unwrap`.
4. **TH Splices**: Place splices (e.g., `makeLenses`) after definitions to avoid staging issues.
5. **Type Instances**: Define `EntityOf`, `EventOf`, `CommandOf` instances in the same module as the type.

**Patterns:**
- `import Array (Array); import Array qualified`
- `result |> Result.toEither`
- `Array.fromLegacy [1, 2, 3]`

---

## 3.4.13 `<ide_layer>`

Used for syntax, parser, and IDE tooling (`feat(syntax)`, `feat(parser)`, `feat(ide)`).

1. **Parser Combinators**: Use `megaparsec` or internal `Parser` type. 
2. **Layout Rules**: Follow "case-only" and "do-let" strictly in the parser. 
3. **Pipes**: `|>` is a first-class citizen in the grammar.
4. **TextMate Scopes**: Map NeoHaskell keywords to standard scopes (`keyword.control.neohaskell`, `entity.name.type.neohaskell`).
5. **LSP**: Handle `textDocument/hover`, `textDocument/definition`, and `textDocument/completion` using nhcore's `Result` and `Task`.

**Patterns:**
- `function_call = identifier <|> (expression |> pipe_op |> function_name)`
- `keyword "do" >> some_let_bindings`
