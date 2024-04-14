# NeoHaskell Code Style

Code written in this codebase must follow the NeoHaskell code style.

The NeoHaskell code style is as follows:

- `NoImplicitPrelude` is enabled, take that into account. It is enabled in the cabal file so don't write the pragma on top of the file.
- Primitive types like`Int`, `Double`, `Char`, `Bool`, etc. are imported from their own NeoHaskell modules, which are named as the type itself. Not `Data.*`.
- When importing collections, you will import them from the NeoHaskell module too, e.g. `Vector` instead of `Data.Vector` and `Maybe` instead of `Data.Maybe`.
- Type parameters in functions will be **always** defined in a `forall` clause.
- Type parameters will be always named as their intent, and not with short letters. For example, instead of `Array a`, one will write `Array value`.
- Modules won't be imported unqualified unless it is for types or classes.
- Modules will be always imported qualified in order to use functions from them.
- Modules will be always imported qualified with a meaningful name. For example `Data.Text` will be imported as `Text` and never as `T`. `GHC.Prim` will be imported as `Prim` and not as `P`.
- Code **WILL NEVER** be written as point-free style, and instead it will always be typed as explicit function application.
- The operators `$` and `.` make an unnatural switch of reading order, the `&` is **ALWAYS** used instead of those.
- Modules from `base` will be always imported with a `Ghc` prefix in their qualification, in order to explicitly set boundaries.
- Functions will always have doc comments as explained in [the documentation.md file](./documentation.md).
- When relying in complex math or syntactic concepts of Haskell which could make some junior TypeScript dev not understand the code, a comment will be left in order to improve that. E.g. `-- TODO: Figure out a way of improve this API`.
