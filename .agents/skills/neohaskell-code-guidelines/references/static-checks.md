# Run the service-code checks

Use HLint 3.10, matching the checksum-pinned binary in [CI](../../../../.github/workflows/checks.yml). The gate rejects other versions because their parser and hint sets differ.

```sh
python3 .agents/skills/neohaskell-code-guidelines/scripts/check.py --hlint /path/to/hlint
HLINT=/path/to/hlint python3 .agents/skills/neohaskell-code-guidelines/tests/hlint_contract.py -v
```

The gate rejects all reported HLint suggestions, warnings, and errors. It also rejects literal `.agents-state` references in application and test source. Existing violations fail the same check as new ones. Exit code 1 means a code violation. Exit code 2 means the check could not run, including parse errors or invalid HLint output.

The `NeoHaskell ownership.` prefix in restriction messages marks ownership rules whose findings remain errors even under local HLint suppression. `Task.mapError` findings also remain errors under suppression. Existing style and false-positive suppressions retain their behavior. Review marker changes and ownership exceptions as gate changes.

Store captured output under `.agents-state/<task>/`. Put concrete cleanup work in the issue tracker. Fix code through the owning APIs and rerun the checks. Skills retain reusable policy and fixtures.

## Understand the coverage limits

The scanner reads tracked and unignored new source files with extensions declared in `SOURCE_SUFFIXES` in [check.py](../scripts/check.py). This includes Haskell, Python, JavaScript, TypeScript, shell, C, Rust, and Go sources across the repository. `.agents` contains the checker, its negative fixtures, and evidence tools. `.github` contains CI tooling. Those two directories are outside runtime-source coverage. Documentation is outside the extension set.

The workspace rule counts literal text even in source comments. It does not evaluate dynamically constructed paths or inspect binary files. HLint checks syntax and imported names, not Haskell types or cancellation semantics. The function list covers the named exception, IORef, and thread-inspection operations in `.hlint.yaml`. An unlisted operation still needs review.

HLint owner exceptions use module names. They do not prove that a file claiming `module Task` is the maintained Task owner. Review module ownership and Cabal registration together. HLint uses simple CPP processing, so this gate does not validate every preprocessor branch.

Review the exact module and function restrictions in [.hlint.yaml](../../../../.hlint.yaml) before changing ownership coverage. Existing exceptions for dialect rules are not a precedent for broad ownership exceptions.

For design decisions beyond mechanical coverage, apply [Name for the reader](../../principle-name-for-the-reader/SKILL.md) and [service design](../../neohaskell-best-practices/references/readable-services.md). Review names, argument structure, atomicity, cancellation, closed outcomes, serialization, module responsibility, and scoped resources. The fixtures prove lint acceptance and rejection. They do not compile their illustrative modules or prove runtime behavior.
