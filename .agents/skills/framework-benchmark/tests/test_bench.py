import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import patch

sys_path = Path(__file__).resolve().parents[1] / "scripts/bench.py"
spec = importlib.util.spec_from_file_location("framework_bench", sys_path)
bench = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bench)


class Runner:
    def __init__(self, output=b"1 example, 0 failures\n", *, outcome="EXITED", returncode=0, elapsed=2.5):
        self.output = output
        self.outcome = outcome
        self.returncode = returncode
        self.elapsed = elapsed
        self.calls = []

    def __call__(self, command, **kwargs):
        self.calls.append((command, kwargs))
        return SimpleNamespace(
            output=self.output,
            outcome=SimpleNamespace(name=self.outcome),
            returncode=self.returncode,
            elapsed_seconds=self.elapsed,
        )


class BenchTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.output_dir = self.root / "run"
        self.entry = {
            "name": "eventstore-inmemory",
            "suite": "nhcore-test-service",
            "match": "Service.EventStore.InMemory with spaces",
            "requires": None,
            "timeout_s": 12,
            "budget_s": None,
        }

    def test_repository_root_uses_the_owned_helper(self):
        helper = self.root / "repo-root"
        helper.write_text("#!/bin/sh\nprintf '%s\\n' \"$NEOHASKELL_ROOT\"\n")
        helper.chmod(0o755)
        with patch.object(bench, "find_root_helper", return_value=helper):
            root = bench.resolve_root(self.root, {"NEOHASKELL_ROOT": str(self.root)})
        self.assertEqual(root, self.root.resolve())

    def test_success_records_real_hspec_count_full_command_and_log(self):
        runner = Runner(b"\x1b[32m3 examples, 0 failures\x1b[0m\n")
        record = bench.run_entry(
            self.entry,
            root=self.root,
            output_dir=self.output_dir,
            env={},
            runner=runner,
            strict=False,
        )
        self.assertEqual(record["status"], "calibration")
        self.assertEqual(record["completed_examples"], 3)
        self.assertEqual(record["wallclock_s"], 2.5)
        command, kwargs = runner.calls[0]
        self.assertEqual(command[-1], "--test-option=Service.EventStore.InMemory with spaces")
        wrapper = self.root / ".agents/skills/neohaskell-best-practices/scripts/with-toolchain"
        self.assertEqual(command[0], str(wrapper))
        self.assertEqual(kwargs["timeout"], 12)
        self.assertEqual(kwargs["max_output_bytes"], bench.MAX_OUTPUT_BYTES)
        self.assertEqual(kwargs["env"]["NEOHASKELL_ROOT"], str(self.root))
        self.assertEqual((self.output_dir / "eventstore-inmemory.log").read_bytes(), runner.output)

    def test_rejects_empty_failed_missing_and_duplicate_hspec_summaries(self):
        for output, returncode, status in [
            (b"0 examples, 0 failures\n", 0, "vacuous"),
            (b"2 examples, 1 failure\n", 0, "test-failed"),
            (b"suite completed without a summary\n", 0, "vacuous"),
            (b"1 example, 0 failures\n1 example, 0 failures\n", 0, "vacuous"),
            (b"2 examples, 0 failures\n", 1, "suite-failed"),
        ]:
            with self.subTest(output=output, returncode=returncode):
                directory = self.root / f"case-{len(list(self.root.iterdir()))}"
                record = bench.run_entry(
                    self.entry,
                    root=self.root,
                    output_dir=directory,
                    env={},
                    runner=Runner(output, returncode=returncode),
                    strict=False,
                )
                self.assertEqual(record["status"], status)

    def test_bounded_failures_and_budget_breach_keep_wallclock_semantics(self):
        timed_out = bench.run_entry(
            self.entry,
            root=self.root,
            output_dir=self.root / "timeout",
            env={},
            runner=Runner(outcome="TIMED_OUT", elapsed=12),
            strict=False,
        )
        limited = bench.run_entry(
            self.entry,
            root=self.root,
            output_dir=self.root / "limited",
            env={},
            runner=Runner(outcome="OUTPUT_LIMITED", elapsed=1),
            strict=False,
        )
        cancelled = bench.run_entry(
            self.entry,
            root=self.root,
            output_dir=self.root / "cancelled",
            env={},
            runner=Runner(outcome="CANCELLED", elapsed=1),
            strict=False,
        )
        budget_entry = {**self.entry, "budget_s": 2}
        breach = bench.run_entry(
            budget_entry,
            root=self.root,
            output_dir=self.root / "breach",
            env={},
            runner=Runner(elapsed=2.5),
            strict=False,
        )
        self.assertEqual(timed_out["status"], "timeout")
        self.assertEqual(cancelled["status"], "cancelled")
        self.assertEqual(limited["status"], "output-limited")
        self.assertEqual(breach["status"], "budget-breach")
        self.assertIn("wallclock", breach["reason"])

    def test_failed_start_is_recorded_with_its_error(self):
        def fail_to_start(*_args, **_kwargs):
            raise FileNotFoundError("with-toolchain is unavailable")

        record = bench.run_entry(
            self.entry,
            root=self.root,
            output_dir=self.output_dir,
            env={},
            runner=fail_to_start,
            strict=False,
        )
        self.assertEqual(record["status"], "failed-start")
        log = (self.output_dir / "eventstore-inmemory.log").read_text()
        self.assertEqual(log, "with-toolchain is unavailable\n")

    def test_postgres_skip_is_visible_and_strict_mode_fails_it(self):
        postgres = {**self.entry, "requires": "postgres"}
        runner = Runner()
        skipped = bench.run_entry(
            postgres,
            root=self.root,
            output_dir=self.root / "skip",
            env={},
            runner=runner,
            strict=False,
        )
        failed_skip = bench.run_entry(
            postgres,
            root=self.root,
            output_dir=self.root / "strict-skip",
            env={},
            runner=runner,
            strict=True,
        )
        self.assertEqual(skipped["status"], "skipped")
        self.assertEqual(failed_skip["status"], "failed-skip")
        self.assertEqual(runner.calls, [])

    def test_run_publishes_incremental_result_and_final_status(self):
        runner = Runner(b"4 examples, 0 failures\n")
        result = bench.record_suites(
            {"schema": 1, "mode": "fixture", "entries": [self.entry]},
            root=self.root,
            output_dir=self.output_dir,
            runner=runner,
            env={},
        )
        saved = json.loads((self.output_dir / "results.json").read_text())
        self.assertEqual(result["status"], "completed")
        self.assertEqual(saved["status"], "completed")
        self.assertEqual(saved["entries"][0]["completed_examples"], 4)
        self.assertIn("not runtime latency", saved["measurement"])
        self.assertTrue((self.output_dir / "eventstore-inmemory.log").is_file())

    def test_config_rejects_empty_or_duplicate_entries(self):
        path = self.root / "config.json"
        for config in [
            {"schema": 1, "mode": "fixture", "entries": []},
            {"schema": 1, "mode": "fixture", "entries": [self.entry, self.entry]},
        ]:
            path.write_text(json.dumps(config))
            with self.assertRaises(ValueError):
                bench.load_config(path)


if __name__ == "__main__":
    unittest.main()
