# Independent comparison

Both candidate authors completed their designs before the independent cross-judge read them. The root coordinator read both artifacts in full and agrees with A as the base.

| Criterion against written designs | A | B |
| --- | --- | --- |
| Root, fan-out, retry, sibling and historical correctness | 3 | 4 |
| Attribution cannot supply authorization or tenancy | 5 | 5 |
| Single owner and small API | 5 | 3 |
| Explicit propagation, bounded lifetime, no ancestry reads | 4 | 5 |
| Public migration and maintainer cost | 4 | 2 |

These are design judgments on a five-point rubric, not runtime measurements. The judge recommended A with B's nil-root normalization and explicit origin lifetime. The root coordinator accepts that recommendation. The judge and coordinator reject both designs' speculative reinterpretation of present historical correlation text and nil source IDs.

## Synthesis

Keep A's context carrier and single lineage owner. Apply B's once-per-invocation normalization before retries and source-scoped capture before fan-out. Treat any present stored correlation as opaque. Fall back only on absence. Preserve existing persisted source identity as immediate cause. Keep log scope design for the logging ticket.

## Comment review

The independent judge reviewed Baseline.hs, Contracts.hs, run_haskell.py, check_walkthrough.cjs and lineage.html under the no-comments instructions. It found no prose comments, suppressions or semantic directives requiring action. No files were changed. Deletions, restorations, reruns, reshape flags, encoding offers and open comment constraints were all zero.

## Evidence limits

The contract runner shares proposed normalization and propagation helpers across two representations. It proves those helper semantics with real framework metadata and in-memory persistence. It does not execute a production command treatment, actual callback dispatch, conflict handling or tenant authorization with the proposed API. The independent reviewer explicitly identified that limit. The real baseline command does reproduce the defect.
