# Run the service-code checks

Use HLint 3.10, matching the checksum-pinned binary in [CI](../../../../.github/workflows/checks.yml). The gate rejects other versions because their parser and hint sets differ.

```sh
python3 .agents/skills/neohaskell-code-guidelines/scripts/check.py --hlint /path/to/hlint
HLINT=/path/to/hlint python3 .agents/skills/neohaskell-code-guidelines/tests/hlint_contract.py -v
```

The gate preserves HLint suggestions, warnings, and errors. It uses the HLint parser and import resolution for ownership rules. It also rejects new literal `.agents-state` references in application and test source. Exit code 1 means a code violation or stale debt. Exit code 2 means the check could not run, including parse errors.

## Remove existing debt as code migrates

The [debt inventory](lint-debt.json) records findings from the source revision in its `base` field. Each entry binds the path, declaration, parsed source, source line, rule, and occurrence count. Blank lines and line-number shifts preserve the identity. A duplicate use, a different alias, a changed source line, or a new declaration needs its own identity and fails.

Fix new findings first. After removing existing violations, run the gate with `--prune-debt`. That mode only removes resolved entries. It fails without writing when new findings exist. Review the deletion with the production change. Do not regenerate the inventory to accept added debt.

Use `--report <file>` to inspect the current findings. The report does not change the accepted inventory. A change to the inventory or `.hlint.yaml` needs review like any other gate change.

## Understand the coverage limits

The scanner reads tracked and unignored new source files with extensions declared in `SOURCE_SUFFIXES` in [check.py](../scripts/check.py). This includes Haskell, Python, JavaScript, TypeScript, shell, C, Rust, and Go sources across the repository. `.agents` contains the checker, its negative fixtures, and evidence tools. `.github` contains CI tooling. Those two directories are outside runtime-source coverage. Documentation is outside the extension set. Deleted files do not keep stale debt alive.

The workspace rule counts the literal text even in source comments. It does not evaluate dynamically constructed paths or inspect binary files. The HLint rules check syntax and imported names, not Haskell types or cancellation semantics. The function list covers the named exception, IORef, and thread-inspection operations in `.hlint.yaml`. A new operation in an already indebted raw import still needs review if it is not on that list.

HLint owner exceptions use module names. They do not prove that a file claiming `module Task` is the maintained Task owner. Review module ownership and Cabal registration together. HLint uses simple CPP processing, so this gate does not validate every preprocessor branch.

Exact debt is a migration constraint, not a proof of unchanged behavior. A change to a later argument line outside a reported symbol's source line can retain that symbol's identity. Replacing one identical occurrence with another in the same declaration can retain its count. Runtime and type-contract tests remain required. The gate reads ignored ownership and Task.mapError findings too, so a local HLint suppression cannot hide those new rules. Existing style and false-positive suppressions retain their behavior.

Historical module exceptions in the older dialect rules remain visible in `.hlint.yaml`. This change adds no broad exceptions for the new ownership rules. The older exceptions are not a precedent for new ones.

For rule ownership, source examples, and manual review obligations, use the [enforcement matrix](enforcement-matrix.md). For the next production stage, use the [handoff](production-handoff.md).

Read [validation evidence](validation.md) for executed commands, retained transcripts, and review limits.
