# Measurement protocol

The JSON object has `claim`, `kind`, `completion`, `settings`, `samples`, `warmups`, `timeout_seconds`, `record`, `metric`, `unit`, `baseline` and `treatment`.

`kind` is `runtime`, `compile` or `suite`. `settings` names workload, compiler and dependency versions, build flags, runtime settings and cache state. Record assumptions explicitly. `samples` is at least five measured samples per side. Warmups run first and remain in the evidence. Every sample runs baseline followed by treatment.

Each side has `cwd`, `argv` and `inputs`. Commands are JSON arrays of literal arguments. `cwd` is a baseline or treatment checkout. `inputs` lists files relative to that directory whose contents identify the measured code, workload and configuration. Include relevant untracked fixtures. The runner hashes the files before and after the run and rejects input drift. Add a `prepare` argument array when each sample needs a defined clean state or edit. Preparation is outside the timer and remains in its own log.

Choose one result contract.

| `record` | Observed result |
| --- | --- |
| `driver-json` | The final output line is JSON containing `metric`, `unit`, finite nonnegative `value`, positive `completed`, zero `errors`, nonnegative `rejected` and `retries`, and `outcome_verified: true`. The driver performs the actual outcome assertions. |
| `hspec` | One Hspec summary reports positive examples and zero failures. The metric is `elapsed` in `s`, timed around the complete command. |
| `compiler` | The command exits successfully and its output matches the protocol's `work_pattern`. The metric is `elapsed` in `s`. Set `scenario` to `clean`, `incremental` or `unchanged`. Only `unchanged` may omit a work pattern. |

For a runtime driver, count successful operations, rejected operations, errors and retries separately. An exit code or a printed boolean does not independently prove the workload's correctness. Inspect the driver's assertions and the underlying completion behavior before relying on its report.

Relative `cwd` values resolve from the protocol's directory. The output is a new run directory under `.agents-state/framework-benchmark/`. `--repo <root>` selects the repository and `--output <path>` selects a new directory within its `.agents-state/`.

The result contains exact argument arrays, revisions where available, content hashes, raw sample logs, child exit codes, successful-work counts, all warmups, measured samples, medians and ranges. It does not calculate requests per second from suite duration or interpret compilation timings as runtime performance.
