import json
import subprocess
import shutil
from pathlib import Path
import stat
import tempfile
import unittest
from unittest.mock import patch

import installer_platform as platform


class InstallerPlatform(unittest.TestCase):
    def setUp(self):
        self.metadata_signature = platform.verify_metadata_signature
        verifier = patch.object(platform, "verify_metadata_signature")
        self.verify_signature = verifier.start()
        self.addCleanup(verifier.stop)
        self.path = "/nix/store/00000000000000000000000000000000-neo"
        self.metadata = {
            "narHash": "sha256-hash", "narSize": 512, "references": [], "signatures": ["upstream:signature"],
        }
        self.caches = [
            {"cache": platform.PRIMARY_CACHE, "key": "project:public"},
            {"cache": "https://upstream.test", "key": "upstream:public"},
        ]

    def test_dependency_uses_upstream_that_can_verify_its_signature_and_bytes(self):
        with patch.object(platform, "remote_info", return_value=self.metadata):
            rows = platform.closure_rows({self.path: self.metadata}, {}, self.caches)
        self.assertEqual(rows[0]["locations"], [
            {"cache": "https://upstream.test", "signatures": ["upstream:signature"]}
        ])
        self.verify_signature.assert_called_once_with(self.path, self.metadata, "upstream:public", ["upstream:signature"])

    def test_project_root_must_be_signed_by_the_project_cache(self):
        with patch.object(platform, "remote_info", return_value=self.metadata), self.assertRaisesRegex(
            ValueError, "No approved signed cache"
        ):
            platform.closure_rows({self.path: self.metadata}, {"neo": self.path}, self.caches)

    def test_cache_metadata_must_match_the_built_package(self):
        for field, changed in (("narHash", "sha256-wrong"), ("narSize", 1), ("references", ["extra"])):
            with self.subTest(field=field), patch.object(
                platform, "remote_info", return_value={**self.metadata, field: changed}
            ), self.assertRaisesRegex(ValueError, "Remote NAR identity"):
                platform.closure_rows({self.path: self.metadata}, {}, self.caches)

    def test_failed_clean_store_acquisition_stops_before_pinning(self):
        toolchain = "/nix/store/11111111111111111111111111111111-neo-toolchain"
        signed = {**self.metadata, "signatures": ["project:signature"]}
        closure = {self.path: {**signed, "references": [toolchain]}, toolchain: signed}
        with (patch.object(platform, "output", return_value=json.dumps(closure)),
              patch.object(platform, "remote_info", side_effect=lambda cache, path: closure[path]),
              patch.object(platform, "verify_clean_store", side_effect=ValueError("acquisition failed")),
              patch.object(subprocess, "run") as command,
              self.assertRaisesRegex(ValueError, "acquisition failed")):
            platform.publish_closure({"pair": self.path}, toolchain, "aarch64-linux", "0.18.0",
                                     self.caches, {}, Path("."))
        command.assert_called_once_with(
            ["cachix", "push", "neohaskell", self.path], check=True, env={},
        )

    def test_pair_requires_the_exact_starter_toolchain_as_a_direct_reference(self):
        toolchain = "/nix/store/11111111111111111111111111111111-neo-toolchain"
        different = "/nix/store/22222222222222222222222222222222-neo-toolchain"
        for references in ([], [different], [toolchain, different]):
            with self.subTest(references=references), self.assertRaisesRegex(ValueError, toolchain):
                platform.pair_closure(self.path, toolchain, {self.path: {"references": references}}, "aarch64-linux")

    def test_read_only_store_cleanup_preserves_external_symlink_target(self):
        with tempfile.TemporaryDirectory() as temporary:
            workspace = Path(temporary)
            outside = workspace / "outside"
            outside.write_text("keep")
            outside.chmod(0o444)
            with platform.isolated_store_directory(workspace) as store:
                contents = store / "contents"
                contents.mkdir()
                (contents / "external").symlink_to(outside)
                contents.chmod(0o555)
            self.assertFalse(store.exists())
            self.assertEqual(stat.S_IMODE(outside.stat().st_mode), 0o444)

    def test_isolated_store_directory_resolves_symlinked_workspace(self):
        with tempfile.TemporaryDirectory() as temporary:
            parent = Path(temporary).resolve()
            workspace = parent / "workspace"
            workspace.mkdir()
            alias = parent / "alias"
            alias.symlink_to(workspace, target_is_directory=True)
            with platform.isolated_store_directory(alias) as store:
                self.assertEqual(store.parent, workspace)
            self.assertFalse(store.exists())

    @unittest.skipUnless(shutil.which("openssl"), "OpenSSL is required for Ed25519 validation")
    def test_signed_cache_fingerprint_rejects_an_altered_nar_size(self):
        path = "/nix/store/3161xhazkr55b1vi5h0a43zk377kf4sq-gcc-14.3.0-libgcc"
        metadata = {
            "narHash": "sha256-OGq2IyLKpiivGddIhF1huPNJ8a9NlnajirII8YxqWOU=", "narSize": 153928,
            "references": [],
        }
        key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        signature = (
            "hydra.iohk.io:JRiQoYMZMdtysbhZuXS9EVpwEZ1tWYMxN4l/f1L8LgKMjRnnsF9IeZHnLpG1Zi0aelYGBD9+azuntTg2fBWwCQ=="
        )
        with patch.object(platform, "output", return_value="1raqda6g225jiaipd5jdmzqlkwxqc5fq8j6p36pji9na48ivcsiq"):
            self.metadata_signature(path, metadata, key, [signature])
            metadata["narSize"] += 1
            with self.assertRaises(subprocess.CalledProcessError):
                self.metadata_signature(path, metadata, key, [signature])

    def test_installer_version_reports_the_release_revision_and_target(self):
        revision = "abcdef" * 6 + "0123"
        target = "aarch64-apple-darwin"
        version = "0.18.0"
        for output in ("neo-installer 0.18.0", f"neo-installer {version} (development, {target})"):
            with self.subTest(output=output), patch.object(subprocess, "run", return_value=subprocess.CompletedProcess(
                [], 0, stdout=(output + "\n").encode(), stderr=b""
            )), self.assertRaisesRegex(ValueError, "exact release identity"):
                platform.observe_versions({"neo-installer": "/binary"}, version, revision, target, {})


if __name__ == "__main__":
    unittest.main()
