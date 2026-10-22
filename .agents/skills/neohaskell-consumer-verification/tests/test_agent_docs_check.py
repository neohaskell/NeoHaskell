import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO = Path(__file__).resolve().parents[4]
CHECK = REPO / ".agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check"


class AgentDocsCheckTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="agent-docs-fixture-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        shutil.copytree(REPO / "docs/agent", self.root / "docs/agent")
        manifest = json.loads((self.root / "docs/agent/manifest.json").read_text())
        for capability in manifest["capabilities"]:
            for key in ("source", "scenario"):
                path = self.root / capability[key]
                path.parent.mkdir(parents=True, exist_ok=True)
                path.touch()

    def check(self, expected_status, expected_text):
        result = subprocess.run(
            [str(CHECK), "--root", str(self.root), "--structure-only"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, expected_status, result.stderr)
        self.assertIn(expected_text, result.stdout + result.stderr)

    def test_complete_corpus(self):
        self.check(0, "PASS: structure")

    def test_missing_page(self):
        (self.root / "docs/agent/commands.md").unlink()
        self.check(1, "missing pages ['commands.md']")

    def test_broken_link(self):
        index = self.root / "docs/agent/index.md"
        index.write_text(index.read_text().replace("commands.md", "missing.md"))
        self.check(1, "missing linked page 'missing.md'")

    def test_missing_scenario(self):
        (self.root / "testbed/tests/integrations/oauth2-account-connection.hurl").unlink()
        self.check(1, "missing or unsafe scenario")

    def test_unknown_haskell_fence(self):
        syntax = self.root / "docs/agent/syntax-and-core.md"
        syntax.write_text(syntax.read_text().replace("```haskell\n", "```haskell unchecked\n"))
        self.check(1, "unsupported Haskell fence label")


if __name__ == "__main__":
    unittest.main()
