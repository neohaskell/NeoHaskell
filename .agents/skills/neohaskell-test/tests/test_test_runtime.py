from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.dont_write_bytecode = True
REPOSITORY = next(path for path in Path(__file__).resolve().parents if (path / ".git").exists())


SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))

from test_runtime import cabal_packages, cabal_test_targets, dependency_source_dirs, run_bounded, run_testbed


class TestRuntimeTests(unittest.TestCase):
    def test_nonfinite_timeouts_are_rejected_before_toolchain_entry(self) -> None:
        for script in ("run-doctest", "run-full-suite", "run-testbed"):
            for value in ("nan", "inf"):
                with self.subTest(script=script, value=value):
                    result = subprocess.run([sys.executable, str(SCRIPTS / script), "--timeout", value], capture_output=True, text=True, timeout=5)
                    self.assertEqual(result.returncode, 2)
                    self.assertIn("timeout must", result.stderr)

    def test_cabal_project_discovers_the_declared_local_packages(self) -> None:
        packages = cabal_packages(REPOSITORY)
        names = {package.name for package in packages}
        self.assertEqual(names, {"nhcore", "nhintegrations", "nhtestbed"})
        self.assertTrue(all(package.cabal_file.is_file() for package in packages))

    def test_cabal_test_targets_match_current_component_declarations(self) -> None:
        targets = set(cabal_test_targets(REPOSITORY))
        self.assertIn(("nhcore", "nhcore-test-core"), targets)
        self.assertIn(("nhcore", "nhcore-test-service"), targets)
        self.assertIn(("nhcore", "nhcore-test-integration"), targets)
        self.assertIn(("nhintegrations", "nhintegrations-test"), targets)

    def test_package_source_roots_include_local_cabal_dependencies(self) -> None:
        packages = cabal_packages(REPOSITORY)
        integrations = next(package for package in packages if package.name == "nhintegrations")
        core = next(package for package in packages if package.name == "nhcore")
        self.assertIn(core.source_dirs[0], dependency_source_dirs(integrations, packages))
        self.assertIn("GHC2021", core.language or "")
        self.assertIn("NoImplicitPrelude", core.extensions)

    def test_bounded_process_captures_output(self) -> None:
        result = run_bounded(
            [sys.executable, "-c", "print('finished')"],
            cwd=REPOSITORY,
            env=os.environ.copy(),
            timeout=3,
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.output.decode().strip(), "finished")
        self.assertFalse(result.timed_out)

    def test_bounded_process_kills_timed_out_child(self) -> None:
        result = run_bounded(
            [sys.executable, "-c", "import time; time.sleep(10)"],
            cwd=REPOSITORY,
            env=os.environ.copy(),
            timeout=0.1,
        )
        self.assertTrue(result.timed_out)
        self.assertIsNotNone(result.returncode)

    def test_bounded_process_stops_when_output_exceeds_the_limit(self) -> None:
        result = run_bounded(
            [sys.executable, "-c", "import sys; sys.stdout.write('x' * 4096)"],
            cwd=REPOSITORY,
            env=os.environ.copy(),
            timeout=3,
            max_output_bytes=128,
        )
        self.assertTrue(result.output_limited)
        self.assertLessEqual(len(result.output), 128)

    def test_missing_testbed_tools_are_reported_as_a_skip(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ, {"PATH": ""}):
                status, reason, result = run_testbed(Path(directory), os.environ.copy(), 1)
        self.assertEqual(status, "SKIP")
        self.assertIn("missing prerequisite", reason)
        self.assertIsNone(result)

    def test_missing_curl_is_reported_as_a_testbed_skip(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tools = Path(directory)
            for name in ("hurl", "pg_isready", "bash"):
                executable = tools / name
                executable.write_text("#!/bin/sh\nexit 0\n")
                executable.chmod(0o755)
            env = os.environ.copy()
            env["PATH"] = str(tools)
            with patch.dict(os.environ, {"PATH": str(tools)}):
                status, reason, result = run_testbed(Path(directory), env, 1)
        self.assertEqual(status, "SKIP")
        self.assertIn("curl", reason)
        self.assertIsNone(result)

    def test_testbed_skip_is_nonfatal_by_default_and_fails_in_strict_mode(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tools = Path(directory)
            bash = shutil.which("bash")
            dirname = shutil.which("dirname")
            self.assertIsNotNone(bash)
            self.assertIsNotNone(dirname)
            (tools / "bash").symlink_to(bash)
            (tools / "dirname").symlink_to(dirname)
            env = os.environ.copy()
            env["PATH"] = str(tools)
            env["NEOHASKELL_ROOT"] = str(REPOSITORY)
            script = SCRIPTS / "run-testbed"
            normal = subprocess.run(
                [sys.executable, str(script), "--_inside-toolchain"],
                cwd=REPOSITORY,
                env=env,
                text=True,
                capture_output=True,
                check=False,
                timeout=5,
            )
            strict = subprocess.run(
                [sys.executable, str(script), "--_inside-toolchain", "--strict"],
                cwd=REPOSITORY,
                env=env,
                text=True,
                capture_output=True,
                check=False,
                timeout=5,
            )
        self.assertEqual(normal.returncode, 0, normal.stderr)
        self.assertIn("testbed: SKIP", normal.stdout)
        self.assertEqual(strict.returncode, 1, strict.stderr)
        self.assertIn("testbed: INCOMPLETE", strict.stdout)


if __name__ == "__main__":
    unittest.main(verbosity=2)
