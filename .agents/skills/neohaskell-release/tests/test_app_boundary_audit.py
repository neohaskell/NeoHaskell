import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "app-boundary-audit.py"


class BoundaryInventoryTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory()
        cls.addClassCleanup(cls.temp.cleanup)
        cls.repo = Path(cls.temp.name)
        cls.git("init", "--quiet")
        (cls.repo / "App.hs").write_text("app = Application.withService service\n")
        (cls.repo / "Runtime.hs").write_text("run = Engine.execute store request\n")
        cls.base = cls.commit()
        (cls.repo / "Runtime.hs").write_text("run = Engine.execute setup request\n")
        (cls.repo / "Guide.md").write_text("Use Application.withConfig for app setup.\n")
        cls.head = cls.commit()

    @classmethod
    def git(cls, *args):
        return subprocess.check_output(
            ["git", "-C", str(cls.repo), *args], text=True
        ).strip()

    @classmethod
    def commit(cls):
        cls.git("add", ".")
        cls.git("-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid",
                "commit", "--quiet", "-m", "fixture")
        return cls.git("rev-parse", "HEAD")

    def audit(self, pattern, base=None):
        return subprocess.run(
            [sys.executable, str(SCRIPT), "--repo", str(self.repo),
             "--base", base or self.base, "--head", self.head, "--pattern", pattern],
            text=True, capture_output=True,
        )

    def test_both_revisions_include_app_runtime_and_documented_candidates(self):
        result = self.audit(r"Engine\.execute|Application\.with")
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual((report["base"], report["head"]), (self.base, self.head))
        for revision in ("base", "head"):
            self.assertIn("App.hs:1:app = Application.withService service", report["candidates"][revision])
        self.assertIn("Engine.execute store request", report["candidates"]["base"])
        self.assertIn("Engine.execute setup request", report["candidates"]["head"])
        self.assertIn("Guide.md:1:Use Application.withConfig", report["candidates"]["head"])
        self.assertIn("Runtime.hs", report["changed_files"])

    def test_no_match_is_empty_evidence(self):
        result = self.audit("MissingSymbol")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["candidates"], {"base": "", "head": ""})

    def test_invalid_inputs_fail_without_a_report(self):
        for pattern, base in (("[", self.base), ("execute", "missing-revision")):
            with self.subTest(pattern=pattern, base=base):
                result = self.audit(pattern, base)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout, "")
                self.assertTrue(result.stderr)


if __name__ == "__main__":
    unittest.main()
