import json
import subprocess
import unittest
from unittest.mock import patch

import installer_platform as platform


class InstallerPlatform(unittest.TestCase):
    def setUp(self):
        self.path = "/nix/store/00000000000000000000000000000000-neo"
        self.metadata = {
            "narHash": "sha256-hash", "narSize": 512, "references": [], "signatures": ["upstream:signature"],
        }
        self.caches = [
            {"cache": platform.PRIMARY_CACHE, "key": "project:public"},
            {"cache": "https://upstream.test", "key": "upstream:public"},
        ]

    def test_dependency_uses_upstream_that_can_verify_its_signature_and_bytes(self):
        with patch.object(platform, "remote_info", return_value=self.metadata), patch.object(
            subprocess, "run"
        ) as verify:
            rows = platform.closure_rows({self.path: self.metadata}, {}, self.caches)
        self.assertEqual(rows[0]["locations"], [
            {"cache": "https://upstream.test", "signatures": ["upstream:signature"]}
        ])
        verify.assert_called_once_with([
            "nix", "store", "verify", "--store", "https://upstream.test",
            "--option", "trusted-public-keys", "upstream:public", "--sigs-needed", "1", self.path,
        ], check=True)

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

    def test_failed_nix_verification_stops_before_pinning(self):
        actions = []
        def run(command, **kwargs):
            actions.append(command[:3])
            if command[:3] == ["nix", "store", "verify"]:
                raise subprocess.CalledProcessError(1, command)
        signed = {**self.metadata, "signatures": ["project:signature"]}
        with patch.object(platform, "output", return_value=json.dumps({self.path: signed})), patch.object(
            platform, "remote_info", return_value=signed
        ), patch.object(subprocess, "run", side_effect=run), self.assertRaises(subprocess.CalledProcessError):
            platform.publish_closure({"pair": self.path}, "aarch64-linux", "0.18.0", self.caches, {})
        self.assertEqual(actions, [["cachix", "push", "neohaskell"], ["nix", "store", "verify"]])

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
