### Eval

**You own the experiment design. Plan, blind, run, synthesize.**

**Non-negotiables for blinding:**

- Hide experiment-identifying labels such as `eval`, `judge`, `rubric` or `score` in coordinator-created directories, files and prompts the candidate sees. Keep legitimate project paths, domain vocabulary and ordinary task requirements, including test files and requests to add tests.
- The candidate prompt looks like an organic user request. State the goal, not the meta.
- No chain-eliciting cues. Don't ask the candidate to list which skills, principles, or files they applied. Ask for design notes generally and grade chain-following from code shape, not self-report.
- Sanitize directory and slug names. Use project-shaped names a user might pick.
- Don't tell the candidate other candidates exist.
- The judge can know it's judging but sees outputs by sanitized label only, never by variant identity or execution setup.
- Comparing two variants: one judge scores both sets in a single pass on one scale, blind to which set each came from.

**Steps:**

1. **Frame.** State what variant is under test and what behavior counts as success. Write the rubric (3-6 concrete criteria) for the judge only. Hold it back from candidates.
2. **Set up sanitized environments.** Per-candidate working dir with the variant in place. Plant any context an organic task would have: a project skeleton, the skills the candidate would naturally read. Hold the baseline and relevant inputs constant except for the declared variant, recording revisions/setup for the coordinator. Use project-shaped working directories under repository-root `.agents-state/` and keep rubric/scoring material outside candidate-visible workspaces and contexts.
3. **Author one organic prompt.** What a user would type. No leakage of what's being measured.
4. **Spawn N parallel candidates** per the **arena** skill's Phase B, using the roles and delegation facilities in `AGENTS.md`. Each works in its own sanitized dir and fresh context without the coordinator's rubric or other candidates' outputs. Same prompt to each. Run all requested candidates within available capacity, batching when necessary.
5. **Spawn one blinded judge** independent of the candidates per the **arena** skill's Phase C and `AGENTS.md`. Judge sees outputs by sanitized label and the rubric, never variant identity or execution setup.
6. **Verify the chain from transcripts, not self-report.** Use the current environment's available histories/tool evidence for this run's candidates only, and state any coverage gaps. Look at which files each candidate actually opened and grade chain-following from those records plus the artifacts, never from its own claims. For NeoHaskell code tasks, corroborate correctness with relevant compiler-consumer, executed behavior or meaningful property checks; a file-open record or judge score alone is not proof.
7. **Read every candidate output yourself** end to end. Compare to the judge's verdict. Disagreement means a model is biased or the rubric is ambiguous. Synthesize.

**Reply:** variant under test, rubric, per-candidate notes, judge's verdict, your synthesis, and a recommendation for whether to promote the variant.
