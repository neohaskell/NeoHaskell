import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


OWNER = Path(__file__).resolve().parents[1]
REPO = OWNER.parents[2]
SPEC = importlib.util.spec_from_file_location("lint_gate", OWNER / "scripts/check.py")
GATE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = GATE
SPEC.loader.exec_module(GATE)
HLINT = str(Path(os.environ.get("HLINT", shutil.which("hlint") or "hlint")).resolve())


class StaticChecks(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.owner = self.root / ".agents/skills/neohaskell-code-guidelines"
        (self.owner / "scripts").mkdir(parents=True)
        (self.owner / "references").mkdir()
        shutil.copy(OWNER / "scripts/check.py", self.owner / "scripts/check.py")
        shutil.copy(REPO / ".hlint.yaml", self.root / ".hlint.yaml")
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)
        self.debt = self.owner / "references/lint-debt.json"
        GATE.write_debt(self.debt, {"schema": 1, "hlint": "3.10"}, {})

    def source(self, text, name="core/Example.hs"):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def check(self, expected, *arguments):
        result = subprocess.run([sys.executable, str(self.owner / "scripts/check.py"), "--hlint", HLINT, *arguments], cwd=self.root, text=True, capture_output=True)
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
        return result.stdout + result.stderr

    def accept_existing(self):
        report = self.root / "findings.json"
        self.check(1, "--report", str(report))
        shutil.copy(report, self.debt)
        self.check(0)

    def test_core_alternatives_pass(self):
        self.source("module Example where\nimport Task qualified\nimport Var qualified\nreadValue variable = Var.get variable\ntranslate task = Task.mapError explain task\n")
        self.check(0)

    def test_qualified_and_unqualified_exception_calls_fail(self):
        self.source("module Example where\nimport Control.Exception qualified as Runtime\nfirst action = Runtime.mask_ action\n")
        self.assertIn("Avoid restricted function", self.check(1))
        self.source("module Example where\nimport Control.Exception (mask_)\nfirst action = mask_ action\n")
        self.assertIn("Avoid restricted function", self.check(1))

    def test_owner_and_unrelated_operation_pass(self):
        self.source("module Task where\nimport Control.Exception qualified as Runtime\nprotect action = Runtime.mask_ action\n")
        self.check(0)
        self.source("module Example where\nimport Task qualified\nraise error = Task.throw error\n")
        self.check(0)

    def test_repeated_io_ref_use_in_existing_declaration_fails(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadBoth ref = do\n  Raw.readIORef ref\n")
        self.accept_existing()
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadBoth ref = do\n  Raw.readIORef ref\n  Raw.readIORef ref\n")
        self.assertIn("NEW", self.check(1))

    def test_modified_use_in_existing_declaration_fails(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadValue ref = Raw.readIORef ref\n")
        self.accept_existing()
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadValue ref = Raw.readIORef anotherRef\n")
        self.check(1)

    def test_line_shift_preserves_exact_debt(self):
        path = self.source("module Example where\nimport Data.IORef qualified as Raw\nreadValue ref = Raw.readIORef ref\n")
        self.accept_existing()
        path.write_text("\n\n" + path.read_text())
        self.check(0)

    def test_resolved_debt_requires_removal(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadValue ref = Raw.readIORef ref\n")
        self.accept_existing()
        self.source("module Example where\nimport Var qualified\nreadValue ref = Var.get ref\n")
        self.assertIn("stale debt", self.check(1))
        self.check(0, "--prune-debt")
        self.assertEqual(json.loads(self.debt.read_text())["findings"], [])

    def test_prune_cannot_admit_new_debt(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadValue ref = Raw.readIORef ref\n")
        before = self.debt.read_bytes()
        self.check(1, "--prune-debt")
        self.assertEqual(self.debt.read_bytes(), before)

    def test_report_cannot_overwrite_accepted_debt(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadValue ref = Raw.readIORef ref\n")
        before = self.debt.read_bytes()
        self.check(2, "--report", str(self.debt))
        self.assertEqual(self.debt.read_bytes(), before)

    def test_thread_status_requires_owner(self):
        self.source("module Example where\nimport GHC.Conc qualified as Runtime\nstatus thread = Runtime.threadStatus thread\n")
        self.assertIn("Avoid restricted function", self.check(1))

    def test_new_exception_operation_under_existing_import_fails(self):
        self.source("module Example where\nimport Control.Exception qualified as Runtime\nprotect action = Runtime.mask_ action\n")
        self.accept_existing()
        self.source("module Example where\nimport Control.Exception qualified as Runtime\nprotect action = do\n  Runtime.mask_ action\n  Runtime.allowInterrupt\n")
        self.assertIn("Runtime.allowInterrupt", self.check(1))

    def test_error_translation_uses_task_combinator(self):
        for expression in [
            "Task.recover (\\error -> Task.throw (explain error)) task",
            "task |> Task.recover (\\error -> Task.throw (explain error))",
            "task |> Task.recover (\\error -> explain error |> Task.throw)",
            "task |> Task.recover (\\error -> error |> explain |> Task.throw)",
            "Task.recover (\\error -> Task.throw (explain error))",
        ]:
            with self.subTest(expression=expression):
                self.source(f"module Example where\nimport Basics\nimport Task qualified\ntranslate task = {expression}\n")
                self.assertIn("NeoHaskell: use Task.mapError", self.check(1))
        self.source("module Example where\nimport Task qualified\ntranslate task = Task.mapError explain task\n")
        self.check(0)
        self.source("module Example where\nimport Basics\nimport Task qualified\ntranslate task = task |> Task.mapError explain\n")
        self.check(0)
        self.source("module Example where\nimport Basics\nimport Task qualified\nrecover task = task |> Task.recover retry\n")
        self.check(0)

    def test_suppression_does_not_hide_ownership_rule(self):
        self.source('module Example where\nimport Control.Exception qualified as Runtime\n{-# ANN module ("HLint: ignore Avoid restricted function" :: String) #-}\nprotect action = Runtime.mask_ action\n')
        self.assertIn("Avoid restricted function", self.check(1))

    def test_existing_non_task_false_positive_suppression_is_preserved(self):
        self.source('module Example where\n{-# ANN module ("HLint: ignore NeoHaskell: use Task.when" :: String) #-}\npass :: IO ()\npass = pure ()\nrun :: Bool -> IO () -> IO ()\nrun flag action = if flag then action else pass\n')
        self.check(0)

    def test_workspace_reference_is_counted_in_haskell_and_python(self):
        self.source('module Example where\nfolder = ".agents-state/test"\n')
        self.accept_existing()
        self.source('module Example where\nfolder = ".agents-state/test.agents-state"\n')
        self.check(1)
        self.source('module Example where\nfolder = ".agents-state/test"\n')
        self.source('folder = ".agents-state/logs"\n', "core/test-service/check.py")
        self.check(1)

    def test_tracked_python_deletion_resolves_debt(self):
        path = self.source('folder = ".agents-state/logs"\n', "neo/test/check.py")
        subprocess.run(["git", "add", "neo/test/check.py"], cwd=self.root, check=True)
        self.accept_existing()
        path.unlink()
        self.check(0, "--prune-debt")

    def test_parse_error_cannot_be_accepted(self):
        self.source("module Example where\nvalue = (\n")
        self.assertIn("Parse error", self.check(2))

    def test_existing_suggestion_gate_is_preserved(self):
        self.source("module Example where\nfirst values = values !! 0\n")
        self.assertIn("Use head", self.check(1))


if __name__ == "__main__":
    unittest.main()
