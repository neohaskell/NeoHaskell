# Run the service-code checks

Use HLint 3.10, matching the checksum-pinned binary in [CI](../../../../.github/workflows/checks.yml). The gate rejects other versions because their parser and hint sets differ.

```sh
python3 .agents/skills/neohaskell-code-guidelines/scripts/check.py --hlint /path/to/hlint
HLINT=/path/to/hlint python3 .agents/skills/neohaskell-code-guidelines/tests/hlint_contract.py -v
```

The gate rejects all reported HLint suggestions, warnings, and errors. It also rejects literal `.agents-state` references in application and test source. Existing violations fail the same check as new ones. Exit code 1 means a code violation. Exit code 2 means the check could not run, including parse errors or invalid HLint output.

The `NeoHaskell ownership.` prefix in restriction messages marks ownership rules whose findings remain errors even under local HLint suppression. `Task.mapError`, yielded-unit conditionals, Boolean cases and redundant `fmt` conversions also remain errors under suppression. Existing style and false-positive suppressions retain their behavior. Review marker changes and ownership exceptions as gate changes.

Run the owned gate instead of invoking HLint alone. HLint emits intermediate observations named `NeoHaskell candidate:`. The checker validates their constructor imports or quasiquote argument before reporting a violation. HLint alone reports broader candidates that include valid code.

Store captured output under `.agents-state/<task>/`. Put concrete cleanup work in the issue tracker. Fix code through the owning APIs and rerun the checks. Skills retain reusable policy and fixtures.

## Match Cabal language settings

[cabal_extensions.py](../scripts/cabal_extensions.py) reads tracked and unignored Cabal files. It follows unconditional common-stanza imports and component source directories. It applies `default-language`, `default-extensions`, then `ghc-options` language flags. It also maps the maintained starter to its [Cabal template](../../../../neo/assets/templates/project.cabal.j2). Components keep separate profiles. `other-extensions` declares extension usage and does not enable defaults.

The gate groups parser invocations by profile. HLint 3.10 does not use its command-line `Strict` setting for the redundant-irrefutable-pattern hint. The checker therefore normalizes only `Strict` and `NoStrict` in temporary parser inputs. Source pragmas take precedence in source order. A generated `LINE` pragma preserves the original diagnostic path and line. Source files remain unchanged.

Conditional Cabal settings, conflicting component profiles, unknown template conditions, unmapped sources in configured projects, source `LINE` directives and source `OPTIONS_GHC` language flags fail as setup errors. Literate sources with inherited settings also fail. This reader supports the maintained unconditional configuration. It does not resolve Cabal flags, project-wide compiler overrides or arbitrary templates.

## Preserve evaluation timing when resolving lint findings

Inspect the module's effective compiler extensions before changing a lazy pattern. Include Cabal defaults, component overrides, source pragmas, and compiler flags. When compilation already enables `Strict`, make that extension explicit in the source so HLint sees the same mode. HLint 3.10 can still report `Redundant irrefutable pattern` when `Strict` is supplied only through its command line.

Keep a necessary `~value` pattern and the tests that distinguish construction from execution. Removing the pattern under `Strict` can demand the value before a deferred action runs. Never enable `Strict` in a lazy module merely to silence lint. For a real evaluation change, test both construction and demanded execution at the supported optimization settings.

For an illustrative deferred calculation, `data Deferred value = Deferred (() -> value)` stores a callback. Under `Strict`, `defer ~value = Deferred (\_ -> value)` can construct the wrapper before demanding `value`. A timing test constructs a wrapper around a failing calculation, then checks that only invoking its callback raises the failure. The lazy pattern preserves that contract.

## Understand the coverage limits

The scanner reads tracked and unignored new source files with extensions declared in `SOURCE_SUFFIXES` in [check.py](../scripts/check.py). This includes Haskell, Python, JavaScript, TypeScript, shell, C, Rust, and Go sources across the repository. `.agents` contains the checker, its negative fixtures, and evidence tools. `.github` contains CI tooling. Those two directories are outside runtime-source coverage. Documentation is outside the extension set.

The workspace rule counts literal text even in source comments. It does not evaluate dynamically constructed paths or inspect binary files. HLint checks syntax and imported names, not Haskell types or cancellation semantics. The function list covers the named exception, IORef, and thread-inspection operations in `.hlint.yaml`. An unlisted operation still needs review. The exact `GHC.IO.Exception` allowance permits imports in its listed core owners. Separate finite function restrictions reject the listed throwing operations in system owners. The `System.Directory.Internal.Prelude` import is rejected in every module. The exact `DirectorySpec` allowance for `System.Directory` permits an independent filesystem oracle.

The exact concurrency import restrictions live in [.hlint.yaml](../../../../.hlint.yaml). They do not catch indirect reexports, unlisted packages such as `StmContainers.Map`, or raw MVar modules. They do not inspect the representation of exported types.

[source_rules.py](../scripts/source_rules.py) checks explicit top-level function signatures for direct `LinkedList Char` parameters, including callback inputs. Split declarations and parenthesized operator signatures are covered. Result-only occurrences, record field types and type synonym declarations are outside this rule. Exact conversion exceptions require the configured path, module and declaration together. The scanner recognizes direct imports and the named core reexports. It does not infer types, expand aliases, or cover local signatures, container-wrapped parameters, equivalent `String` or `[Char]` spellings, or arbitrary reexports.

Boolean-case findings require two explicit `True` and `False` alternatives and known constructor imports. Wildcard alternatives are outside coverage. The `fmt` rule requires a direct quasiquote argument from a known provider. Neither rule proves Haskell types. Unsupported import syntax and unlisted reexports need review. HLint's simple CPP processing does not establish which compiler branch is active.

HLint owner exceptions use module names. They do not prove that a file claiming `module Task` is the maintained Task owner. Review module ownership and Cabal registration together. HLint uses simple CPP processing, so this gate does not validate every preprocessor branch.

Review the exact module and function restrictions in [.hlint.yaml](../../../../.hlint.yaml) before changing ownership coverage. Existing exceptions for dialect rules are not a precedent for broad ownership exceptions.

For design decisions beyond mechanical coverage, apply [Name for the reader](../../principle-name-for-the-reader/SKILL.md) and [service design](../../neohaskell-best-practices/references/readable-services.md). Review names, argument structure, atomicity, cancellation, closed outcomes, serialization, module responsibility, and scoped resources. The fixtures prove lint acceptance and rejection. They do not compile their illustrative modules or prove runtime behavior.
