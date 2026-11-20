# Classify application impact

Complete this audit before choosing impact for a changed exported or internal
symbol. Classify each symbol's supported use, then select the highest impact
across the change. A Cabal `exposed-modules` entry establishes importability.
It does not by itself establish a supported app-authoring contract.

## Trace the boundary

1. Record the exact owning base and candidate head. Inspect their diff and list
   the changed signatures, generated contracts, defaults, and observable behavior.
   Run [the inventory helper](../scripts/app-boundary-audit.py) with a Git extended
   regex covering the changed symbols, app entrypoints, and their import aliases.
   It searches all tracked text at both commits and writes JSON to stdout.

   ```sh
   python3 .agents/skills/neohaskell-release/scripts/app-boundary-audit.py \
     --base <owning-base> --head <candidate-head> --pattern '<symbol-or-alias-regex>'
   ```

   Save the report in the caller's ignored evidence directory. Uncommitted edits
   are absent from this inventory. Review them separately and rerun at the final
   commit. Empty matches establish only that the supplied pattern found nothing.
2. Read actual application setup in `neo/starter/src` and `testbed/src`. Follow
   registered services into commands, queries, and integrations. Search maintained
   public examples and documentation for supported paths those apps omit.
   Inspect public reexports, markers, generated wiring, and surrounding source.
   Expand the search for aliases and unqualified calls. Record searched paths and
   absent uses as well as positive matches. Neither absence from the starter nor
   an internal-looking module name establishes that an API is unsupported.
3. Trace each changed symbol back to its framework runtime owner and forward
   from app-authored setup. Distinguish app-authored calls, marker-generated
   wiring, framework runtime calls, and tests or tooling. Check documented
   extension points as supported contracts even when maintained apps omit them.
   Resolve stale examples against current signatures and generated output.
4. Record one row per changed contract in the audit table below. Read the actual
   call sites at both revisions before assigning a role. Account for application
   behavior, configuration, persisted data, and generated code as well as types.
   If support or behavior remains uncertain, gather further evidence or report
   the classification unresolved. Do not default uncertainty to `none`.
5. Apply the impact rules below. Put the conclusion and any direct-import caveat
   in the PR body. Keep the detailed inventory with the run evidence for review.

| Changed contract | App-authored use and support evidence | Generated wiring and runtime owner | Required app action or behavior change | Impact and reason |
| --- | --- | --- | --- | --- |
| Symbol and base/head signature or behavior | Revision, path, line, or searched scope with no use found | Revision, path, line, and call chain | Concrete edit, observable change, or unchanged | `none`, `compatible`, `breaking`, or unresolved |

## Choose impact from the supported contract

- Use `none` for an internal refactor when supported app APIs, generated contracts,
  and observable app behavior remain unchanged. Explain that boundary in Summary.
  A changed exposed runtime constructor can qualify when the framework owns its
  invocation and supported application setup stays unchanged. Omit migration text
  for unaffected apps.
- Use `compatible` for an app-visible feature, fix, or performance improvement
  that preserves supported existing use. Unchanged app source alone does not
  make a behavior improvement `none`.
- Use `breaking` when supported application code, configuration, data, generated
  contracts, or relied-on behavior require migration. This includes app-facing
  markers and documented extension points regardless of their module names.
  A changed app builder that requires callers to wrap a value in a factory is
  breaking even when the framework's internal callers have all been migrated.
- Disclose direct external import compatibility separately in the PR body. Name
  the changed symbol, its old-to-new calling shape, the affected custom callers,
  and the evidence for the unchanged supported app path. Say that such callers
  must adapt when true. Do not claim universal source or binary compatibility.
  A documented supported direct call is app-facing and requires `breaking` when
  incompatible. Mere import possibility does not create an ordinary app migration.

The audit does not override commit-message impact, fragment requirements,
generated-bookkeeping validation, or CI gates. Keep release metadata and commit
messages consistent with the reviewed change. A `none` fragment cannot cancel a
releasable commit message.

## Prove a breaking migration

Use [NeoHaskell consumer verification](../../neohaskell-consumer-verification/SKILL.md)
for same-PR starter and testbed migration and exact-revision consumer checks.
Base migration examples on real supported application code. Record a compiling
before example against the exact base and a compiling after example against the
exact candidate framework revision. For a source incompatibility, also capture
why the old call fails against the candidate. Attribute failures to the changed
contract rather than an unavailable toolchain or unrelated diagnostic.

Run the generated consumer contract and affected testbed checks required by that
skill. Exercise changed runtime behavior when compilation cannot prove it.
Record commands, revisions, exit statuses, and relevant diagnostics. An import
inventory or source comparison is classification evidence, not compiling-consumer
proof. Missing builds leave migration verification incomplete, even when the
breaking classification is already clear. Write the Migration, Verify, and
Agent prompt sections from this evidence.

## Check the inventory helper

Run its local fixtures after changing the helper.

```sh
python3 -m unittest discover -s .agents/skills/neohaskell-release/tests -p 'test_app_boundary_audit.py'
```

These fixtures check revision-specific search results and failure propagation.
They do not validate semantic release judgments or compile Haskell consumers.
