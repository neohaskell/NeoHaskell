import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


OWNER = Path(__file__).resolve().parents[1]
REPO = OWNER.parents[2]
HLINT = str(Path(os.environ.get("HLINT", shutil.which("hlint") or "hlint")).resolve())


class StaticChecks(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.owner = self.root / ".agents/skills/neohaskell-code-guidelines"
        (self.owner / "scripts").mkdir(parents=True)
        shutil.copy(OWNER / "scripts/check.py", self.owner / "scripts/check.py")
        shutil.copy(REPO / ".hlint.yaml", self.root / ".hlint.yaml")
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)

    def source(self, text, name="core/Example.hs"):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def check(self, expected, *arguments):
        result = subprocess.run([sys.executable, str(self.owner / "scripts/check.py"), "--hlint", HLINT, *arguments], cwd=self.root, text=True, capture_output=True)
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
        return result.stdout + result.stderr

    def test_core_alternatives_pass(self):
        self.source("module Example where\nimport Task qualified\nimport Var qualified\nreadValue variable = Var.get variable\ntranslate task = Task.mapError explain task\n")
        self.check(0)

    def test_qualified_and_unqualified_exception_calls_fail(self):
        self.source("module Example where\nimport Control.Exception qualified as Runtime\nfirst action = Runtime.mask_ action\n")
        self.assertIn("Avoid restricted function", self.check(1))
        self.source("module Example where\nimport Control.Exception (mask_)\nfirst action = mask_ action\n")
        self.assertIn("Avoid restricted function", self.check(1))

    def test_owner_and_unrelated_operation_pass(self):
        for module, imported, expression in [
            ("Task", "Control.Exception", "Runtime.mask_ action"),
            ("AsyncTask", "GHC.Conc", "Runtime.threadStatus action"),
            ("Var", "Data.IORef", "Runtime.readIORef action"),
        ]:
            with self.subTest(module=module):
                self.source(f"module {module} where\nimport {imported} qualified as Runtime\noperation action = {expression}\n")
                self.check(0)
        self.source("module Example where\nimport Task qualified\nraise error = Task.throw error\n")
        self.check(0)

    def test_io_exception_owner_passes(self):
        for imported in ["Control.Exception", "Control.Exception.Base"]:
            for qualified in [True, False]:
                with self.subTest(imported=imported, qualified=qualified):
                    import_names = "qualified as Runtime" if qualified else "(finally, onException, catch, try)"
                    prefix = "Runtime." if qualified else ""
                    self.source(f"module IO where\nimport {imported} {import_names}\nfinish action cleanup = {prefix}finally action cleanup\ncleanFailed action cleanup = {prefix}onException action cleanup\nrecover action handler = {prefix}catch action handler\nattempt action = {prefix}try action\n", "core/core/IO.hs")
                    self.check(0)

    def test_application_exception_operations_fail(self):
        for imported in ["Control.Exception", "Control.Exception.Base"]:
            with self.subTest(imported=imported):
                self.source(f"module Example where\nimport {imported} qualified as Runtime\nfinish action cleanup = Runtime.finally action cleanup\n")
                output = self.check(1)
                self.assertIn("Avoid restricted module", output)
                self.assertIn("Avoid restricted function", output)

    def test_io_does_not_own_mutable_state_or_thread_inspection(self):
        for imported, expression in [
            ("Data.IORef", "Runtime.readIORef value"),
            ("GHC.Conc", "Runtime.threadStatus value"),
            ("GHC.Conc.Sync", "Runtime.threadStatus value"),
        ]:
            with self.subTest(imported=imported):
                self.source(f"module IO where\nimport {imported} qualified as Runtime\noperation value = {expression}\n", "core/core/IO.hs")
                output = self.check(1)
                self.assertIn("Avoid restricted module", output)
                self.assertIn("Avoid restricted function", output)

    def test_tracked_repeated_io_ref_use_fails(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadBoth ref = do\n  Raw.readIORef ref\n  Raw.readIORef ref\n")
        subprocess.run(["git", "add", "core/Example.hs"], cwd=self.root, check=True)
        self.assertIn("Avoid restricted function (2)", self.check(1))

    def test_dialect_module_restriction_still_fails(self):
        self.source("module Example where\nimport Data.List qualified as Raw\nvalue = Raw.reverse [1, 2]\n")
        self.assertIn("Avoid restricted module", self.check(1))

    def test_suppression_does_not_hide_error_translation(self):
        self.source('module Example where\nimport Task qualified\n{-# ANN module ("HLint: ignore NeoHaskell: use Task.mapError" :: String) #-}\ntranslate task = Task.recover (\\error -> Task.throw (explain error)) task\n')
        self.assertIn("NeoHaskell: use Task.mapError", self.check(1))

    def test_old_restriction_suppression_is_preserved(self):
        self.source('module Example where\nimport Data.List qualified as Raw\n{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}\nvalue = Raw.reverse [1, 2]\n')
        self.check(0)

    def test_thread_status_requires_owner(self):
        self.source("module Example where\nimport GHC.Conc qualified as Runtime\nstatus thread = Runtime.threadStatus thread\n")
        self.assertIn("Avoid restricted function", self.check(1))

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

    def test_workspace_reference_is_rejected_in_haskell_and_python(self):
        for name, source in [
            ("core/Example.hs", 'module Example where\nfolder = ".agents-state/test"\n'),
            ("core/test/check.py", 'folder = ".agents-state/logs"\n'),
        ]:
            with self.subTest(name=name):
                path = self.source(source, name)
                self.assertIn("Agent workspace in runtime source", self.check(1))
                path.unlink()

    def test_scoped_resource_and_agent_tooling_pass(self):
        self.source('module Example where\nimport Directory qualified\nwithFolder action = Directory.withTemporary action\n')
        self.source('folder = ".agents-state/logs"\n', ".agents/skills/example/scripts/check.py")
        self.check(0)

    def test_parse_error_cannot_be_accepted(self):
        self.source("module Example where\nvalue = (\n")
        self.assertIn("Parse error", self.check(2))

    def test_invalid_hlint_output_fails_visibly(self):
        self.source("module Example where\nvalue = 1\n")
        executable = self.root / "fake-hlint"
        for output, status in [("not JSON", 0), ("{}", 0), ("[]", 1)]:
            with self.subTest(output=output, status=status):
                executable.write_text(f"#!{sys.executable}\nimport sys\nif '--numeric-version' in sys.argv:\n    print('3.10')\nelse:\n    print({output!r})\n    sys.exit({status})\n")
                executable.chmod(0o755)
                self.assertIn("HLint", self.check(2, "--hlint", str(executable)))

    def test_existing_suggestion_gate_is_preserved(self):
        self.source("module Example where\nfirst values = values !! 0\n")
        self.assertIn("Use head", self.check(1))


if __name__ == "__main__":
    unittest.main()
