import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO = Path(__file__).resolve().parents[4]
CHECK = REPO / ".agents/skills/neohaskell-consumer-verification/scripts/agent-docs-check"
REGISTRY = Path(".agents/skills/neohaskell-consumer-verification/references/agent-docs-evidence.json")


class AgentDocsCheckTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="agent-docs-fixture-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        shutil.copytree(REPO / "docs/agent", self.root / "docs/agent")
        (self.root / REGISTRY).parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(REPO / REGISTRY, self.root / REGISTRY)
        evidence = json.loads((self.root / REGISTRY).read_text())
        paths = {
            "core/test-service/Main.hs",
            "core/test-auth/Main.hs",
        }
        for capability in evidence["capabilities"].values():
            paths.update((capability["source"], capability["scenario"]))
        for capability in evidence["capabilities"].values():
            paths.update(check["path"] for check in capability["evidence"])
        paths.update(receipt["source"] for receipt in evidence["ignored_fragments"])
        for relative in paths:
            path = self.root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(REPO / relative, path)

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

    def test_internal_source_path(self):
        page = self.root / "docs/agent/project-and-cli.md"
        page.write_text(page.read_text() + "\nSee `neo/src/reconcile/mod.rs`.\n")
        self.check(1, "internal framework path 'neo/src/reconcile/mod.rs'")

    def test_encoded_internal_source_path(self):
        page = self.root / "docs/agent/project-and-cli.md"
        page.write_text(page.read_text() + "\nSee `core%2Fservice%2FService%2FApplication.hs`.\n")
        self.check(1, "internal framework path 'core/service/Service/Application.hs'")

    def test_generated_project_path_is_allowed(self):
        page = self.root / "docs/agent/project-and-cli.md"
        page.write_text(page.read_text() + "\nEdit `src/App.hs` and `tests/Spec.hs`.\n")
        self.check(0, "PASS: structure")

    def test_private_evidence_cannot_be_published(self):
        shutil.copyfile(self.root / REGISTRY, self.root / "docs/agent/evidence.json")
        self.check(1, "unlisted files ['evidence.json']")

    def test_old_manifest_is_rejected(self):
        path = self.root / "docs/agent/manifest.json"
        manifest = json.loads(path.read_text())
        manifest["schema"] = 1
        path.write_text(json.dumps(manifest))
        self.check(1, "expected schema 2")

    def test_public_manifest_rejects_private_metadata(self):
        path = self.root / "docs/agent/manifest.json"
        manifest = json.loads(path.read_text())
        manifest["source"] = "core/service/Service/Application.hs"
        path.write_text(json.dumps(manifest))
        self.check(1, "expected schema 2 with only pages")

    def test_duplicate_private_capability_is_rejected(self):
        path = self.root / REGISTRY
        content = path.read_text()
        path.write_text(content.replace('"capabilities": {', '"capabilities": {"commands": {},', 1))
        self.check(1, "duplicate JSON key 'commands'")

    def test_broken_link(self):
        index = self.root / "docs/agent/index.md"
        index.write_text(index.read_text().replace("commands.md", "missing.md"))
        self.check(1, "missing linked page 'missing.md'")

    def test_missing_scenario(self):
        (self.root / "testbed/tests/integrations/oauth2-account-connection.hurl").unlink()
        self.check(1, "missing or unsafe scenario")

    def test_empty_scenario(self):
        (self.root / "testbed/tests/integrations/oauth2-account-connection.hurl").write_text("")
        self.check(1, "scenario needs a request and assertions")

    def test_unknown_haskell_fence(self):
        syntax = self.root / "docs/agent/syntax-and-core.md"
        syntax.write_text(syntax.read_text().replace("```haskell\n", "```haskell unchecked\n"))
        self.check(1, "unsupported Haskell fence label")

    def test_missing_consumer_evidence(self):
        path = self.root / REGISTRY
        evidence = json.loads(path.read_text())
        evidence["capabilities"]["project-and-cli"]["evidence"] = []
        path.write_text(json.dumps(evidence))
        self.check(1, "project-and-cli lacks required executable evidence")

    def test_missing_evidence_file(self):
        (self.root / REGISTRY).unlink()
        self.check(1, "cannot read valid evidence")

    def test_missing_mcp_spec(self):
        path = self.root / REGISTRY
        evidence = json.loads(path.read_text())
        transports = evidence["capabilities"]["transports"]
        transports["evidence"] = transports["evidence"][:1]
        path.write_text(json.dumps(evidence))
        self.check(1, "transports lacks required executable evidence")

    def test_missing_fragment_receipt(self):
        path = self.root / REGISTRY
        evidence = json.loads(path.read_text())
        evidence["ignored_fragments"].pop()
        path.write_text(json.dumps(evidence))
        self.check(1, "missing ignored fragment receipts")

    def test_stale_fragment_review(self):
        path = self.root / "docs/agent/commands.md"
        path.write_text(path.read_text().replace("getEntityId cmd = Just cmd.entityId", "getEntityId cmd = Nothing"))
        self.check(1, "stale fragment review")

    def test_stale_source_review(self):
        path = self.root / "neo/starter/src/Starter/Counter/Commands/IncrementCounter.hs"
        path.write_text(path.read_text() + "\n")
        self.check(1, "stale source review")

    def test_unregistered_spec(self):
        path = self.root / "core/test-service/Main.hs"
        path.write_text(path.read_text().replace("McpSpec", "RemovedSpec"))
        self.check(1, "spec is not registered")

    def test_failed_evidence_command_blocks_publication(self):
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        cabal = bin_dir / "cabal"
        cabal.write_text("#!/bin/sh\nexit 0\n")
        cabal.chmod(0o755)
        runner = self.root / "testbed/scripts/run-tests.sh"
        runner.parent.mkdir(parents=True, exist_ok=True)
        runner.write_text("#!/bin/sh\necho fixture-scenario-failed >&2\nexit 1\n")
        runner.chmod(0o755)
        env = os.environ.copy()
        env["PATH"] = f"{bin_dir}{os.pathsep}{env['PATH']}"
        env["NEOHASKELL_SHELL_FP"] = "fixture"
        result = subprocess.run(
            [str(CHECK), "--root", str(self.root), "--verify-evidence"],
            capture_output=True,
            text=True,
            check=False,
            env=env,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("fixture-scenario-failed", result.stderr)


if __name__ == "__main__":
    unittest.main()
