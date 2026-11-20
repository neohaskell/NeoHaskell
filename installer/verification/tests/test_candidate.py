"""Exercise candidate artifact containment through the manifest loader."""

import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest


POLICY = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("installer_candidate_verifier", POLICY / "verify.py")
VERIFY = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = VERIFY
SPEC.loader.exec_module(VERIFY)


class CandidateTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="candidate-containment-test-")
        self.root = Path(self.temporary.name)
        self.manifest_dir = self.root / "manifest"
        self.manifest_dir.mkdir()
        self.manifest_path = self.manifest_dir / "candidate.json"
        self.repo = Path(__file__).resolve().parents[3]
        self.source = VERIFY.git_text(self.repo, "rev-parse", "HEAD")
        self.nested_dir = self.manifest_dir / "nested"
        self.nested_dir.mkdir()
        self.nested_current = b"owned nested current fixture"
        self.nested_previous = b"owned nested previous fixture"
        (self.nested_dir / "current.bin").write_bytes(self.nested_current)
        (self.nested_dir / "previous.bin").write_bytes(self.nested_previous)
        self.external_dir = self.root / "outside"
        self.external_dir.mkdir()
        self.external_current = b"owned outside current fixture"
        self.external_previous = b"owned outside previous fixture"
        (self.external_dir / "current.bin").write_bytes(self.external_current)
        (self.external_dir / "previous.bin").write_bytes(self.external_previous)

    def tearDown(self):
        self.temporary.cleanup()

    def artifact(self, name, relative_path, content):
        return {
            "name": name,
            "path": relative_path,
            "sha256": VERIFY.digest_bytes(content),
            "size": len(content),
        }

    def candidate(self, artifacts, previous_artifacts):
        manifest = {
            "schema_version": 1,
            "version": "0.0.2-fixture",
            "source_commit": self.source,
            "artifacts": artifacts,
            "previous_pair": {
                "version": "0.0.1-fixture",
                "artifacts": previous_artifacts,
            },
        }
        self.manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
        return self.manifest_path

    def create_directory_symlink(self):
        link = self.manifest_dir / "redirect"
        try:
            link.symlink_to(self.external_dir, target_is_directory=True)
        except (NotImplementedError, OSError) as error:
            self.skipTest(f"directory symlinks are unavailable: {error}")

    def test_nested_regular_artifacts_are_accepted(self):
        manifest = self.candidate(
            [self.artifact("neo", "nested/current.bin", self.nested_current)],
            [self.artifact("neo", "nested/previous.bin", self.nested_previous)],
        )

        result = VERIFY.load_candidate(manifest, self.repo, self.source)

        self.assertEqual(result.artifacts[0].digest, VERIFY.digest_bytes(self.nested_current))
        self.assertEqual(result.previous_artifacts[0].digest, VERIFY.digest_bytes(self.nested_previous))

    def test_parent_directory_symlink_escapes_are_rejected_for_both_pairs(self):
        self.create_directory_symlink()
        safe_current = self.artifact("neo", "nested/current.bin", self.nested_current)
        safe_previous = self.artifact("neo", "nested/previous.bin", self.nested_previous)
        outside_current = self.artifact("neo", "redirect/current.bin", self.external_current)
        outside_previous = self.artifact("neo", "redirect/previous.bin", self.external_previous)

        for group, current, previous in (
            ("artifacts", [outside_current], [safe_previous]),
            ("previous_pair.artifacts", [safe_current], [outside_previous]),
        ):
            with self.subTest(group=group):
                manifest = self.candidate(current, previous)

                try:
                    result = VERIFY.load_candidate(manifest, self.repo, self.source)
                except VERIFY.VerificationError:
                    continue
                identities = result.artifacts if group == "artifacts" else result.previous_artifacts
                self.fail(f"load_candidate accepted {group} with fixture digest {identities[0].digest}")

    def test_direct_artifact_symlinks_remain_rejected(self):
        link = self.manifest_dir / "linked-current.bin"
        try:
            link.symlink_to(self.external_dir / "current.bin")
        except (NotImplementedError, OSError) as error:
            self.skipTest(f"file symlinks are unavailable: {error}")
        manifest = self.candidate(
            [self.artifact("neo", link.name, self.external_current)],
            [self.artifact("neo", "nested/previous.bin", self.nested_previous)],
        )

        with self.assertRaises(VERIFY.VerificationError):
            VERIFY.load_candidate(manifest, self.repo, self.source)


if __name__ == "__main__":
    unittest.main()
