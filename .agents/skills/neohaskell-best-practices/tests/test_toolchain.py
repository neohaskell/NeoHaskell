from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
REPOSITORY = next(path for path in Path(__file__).resolve().parents if (path / ".git").exists())
TEST_STATE = REPOSITORY / ".agents-state" / "owned-test-fixtures"
TEST_STATE.mkdir(parents=True, exist_ok=True)


SOURCE = Path(__file__).resolve().parents[1] / "scripts"


class ToolchainTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(dir=TEST_STATE)
        self.base = Path(self.temporary.name)
        self.root = self.base / "project"
        self.script_dir = self.root / ".agents/skills/neohaskell-best-practices/scripts"
        self.script_dir.mkdir(parents=True)
        (self.root / "nix").mkdir()
        (self.root / "core").mkdir()
        (self.root / "cabal.project").write_text("packages: core/*.cabal\n")
        (self.root / "cabal.project.dev").write_text("import: cabal.project\noptimization: 0\n")
        (self.root / "nix/hix.nix").write_text("compiler-nix-name = ghc98;\n")
        (self.root / "flake.nix").write_text("inputs = {};\n")
        (self.root / "flake.lock").write_text("{}\n")
        (self.root / "core/nhcore.cabal").write_text("name: nhcore\nbuild-depends: base\n")
        for name in ("repo-root", "toolchain-fp", "with-toolchain"):
            destination = self.script_dir / name
            if name == "with-toolchain":
                absent_profile = self.base / "absent-nix-profile/bin"
                source = (SOURCE / name).read_text().replace(
                    'NIX_PROFILE_BIN="/nix/var/nix/profiles/default/bin"',
                    f'NIX_PROFILE_BIN="{absent_profile}"',
                )
                destination.write_text(source)
            else:
                shutil.copy2(SOURCE / name, destination)
            destination.chmod(0o755)

        self.tools = self.base / "tools"
        self.tools.mkdir()
        for name in ("bash", "dirname", "cat", "cut"):
            self.link_tool(name, self.tools)
        hasher = shutil.which("sha256sum") or shutil.which("shasum")
        if hasher is None:
            self.skipTest("sha256sum and shasum are unavailable")
        self.link_tool(Path(hasher).name, self.tools)
        self.env = {
            "PATH": str(self.tools),
            "HOME": str(self.base),
            "NEOHASKELL_ROOT": str(self.root),
        }

    def tearDown(self) -> None:
        self.temporary.cleanup()

    @staticmethod
    def link_tool(name: str, directory: Path) -> None:
        tool = shutil.which(name)
        if tool:
            destination = directory / name
            if not destination.exists():
                destination.symlink_to(tool)

    def make_command(self, directory: Path, name: str, body: str) -> Path:
        directory.mkdir(parents=True, exist_ok=True)
        path = directory / name
        path.write_text("#!/usr/bin/env bash\n" + body)
        path.chmod(0o755)
        self.link_tool("bash", directory)
        return path

    def run_script(self, name: str, *args: str, env: dict[str, str] | None = None) -> subprocess.CompletedProcess[str]:
        merged = self.env.copy()
        if env:
            merged.update(env)
        return subprocess.run(
            [str(self.tools / "bash"), str(self.script_dir / name), *args],
            cwd=self.base,
            env=merged,
            text=True,
            capture_output=True,
            check=False,
            timeout=10,
        )

    def fingerprint(self) -> str:
        result = self.run_script("toolchain-fp")
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.strip()

    def test_root_discovery_uses_script_ancestors_without_a_root_scripts_directory(self) -> None:
        env = self.env.copy()
        env.pop("NEOHASKELL_ROOT")
        result = self.run_script("repo-root", env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), str(self.root.resolve()))

    def test_explicit_root_is_checked(self) -> None:
        result = self.run_script("repo-root", env={"NEOHASKELL_ROOT": str(self.base)})
        self.assertEqual(result.returncode, 2)
        self.assertIn("must contain cabal.project and nix/hix.nix", result.stderr)

    def test_fingerprint_changes_when_a_cabal_dependency_changes(self) -> None:
        before = self.fingerprint()
        cabal_file = self.root / "core/nhcore.cabal"
        cabal_file.write_text(cabal_file.read_text() + "  text,\n")
        after = self.fingerprint()
        self.assertNotEqual(before, after)

    def test_fingerprint_changes_when_a_nix_module_changes(self) -> None:
        before = self.fingerprint()
        (self.root / "nix/extra.nix").write_text("shell.tools = {};\n")
        after = self.fingerprint()
        self.assertNotEqual(before, after)

    def test_path_witness_restores_pinned_commands_and_preserves_argv(self) -> None:
        witness = self.base / "witness"
        mutated = self.base / "mutated"
        self.make_command(witness, "host-tool", "printf 'witness:%s\\n' \"$@\"\nexit 23\n")
        self.make_command(mutated, "host-tool", "printf 'mutated:%s\\n' \"$@\"\nexit 24\n")
        current_path = f"{mutated}:{self.tools}"
        result = self.run_script(
            "with-toolchain",
            "host-tool",
            "argument with spaces",
            "",
            env={
                "PATH": current_path,
                "NEOHASKELL_SHELL_FP": self.fingerprint(),
                "NEOHASKELL_SHELL_PATH": str(witness),
            },
        )
        self.assertEqual(result.returncode, 23, result)
        self.assertEqual(result.stdout.splitlines(), ["witness:argument with spaces", "witness:"])
        self.assertNotIn("mutated:", result.stdout)

    def test_stale_fingerprint_enters_nix_develop(self) -> None:
        self.make_command(self.tools, "nix", "printf 'nix:%s\\n' \"$@\"\nexit 37\n")
        self.make_command(self.tools, "host-tool", "printf 'host\\n'\n")
        result = self.run_script(
            "with-toolchain",
            "host-tool",
            "arg",
            env={"NEOHASKELL_SHELL_FP": "stale", "NEOHASKELL_SHELL_PATH": str(self.tools)},
        )
        self.assertEqual(result.returncode, 37, result)
        self.assertEqual(result.stdout.splitlines(), ["nix:develop", "nix:--command", "nix:host-tool", "nix:arg"])

    def test_changed_dependency_rejects_a_previously_matching_fingerprint(self) -> None:
        fingerprint = self.fingerprint()
        cabal_file = self.root / "core/nhcore.cabal"
        cabal_file.write_text(cabal_file.read_text() + "  bytestring,\n")
        self.make_command(self.tools, "nix", "printf 'nix:%s\\n' \"$@\"\n")
        result = self.run_script(
            "with-toolchain",
            "host-tool",
            env={"NEOHASKELL_SHELL_FP": fingerprint, "NEOHASKELL_SHELL_PATH": str(self.tools)},
        )
        self.assertEqual(result.returncode, 0, result)
        self.assertIn("nix:develop", result.stdout)

    def test_missing_witness_command_enters_nix(self) -> None:
        self.make_command(self.tools, "nix", "printf 'nix:%s\\n' \"$@\"\n")
        result = self.run_script(
            "with-toolchain",
            "host-tool",
            env={
                "NEOHASKELL_SHELL_FP": self.fingerprint(),
                "NEOHASKELL_SHELL_PATH": str(self.base / "empty-witness"),
            },
        )
        self.assertEqual(result.returncode, 0, result)
        self.assertIn("nix:develop", result.stdout)

    def test_profile_only_nix_is_found(self) -> None:
        profile = self.base / "profile/bin"
        profile.mkdir(parents=True)
        copied = self.script_dir / "with-toolchain-profile-test"
        source = (self.script_dir / "with-toolchain").read_text()
        source = source.replace(
            f'NIX_PROFILE_BIN="{self.base / "absent-nix-profile/bin"}"',
            f'NIX_PROFILE_BIN="{profile}"',
        )
        copied.write_text(source)
        copied.chmod(0o755)
        self.make_command(profile, "nix", "printf 'profile:%s\\n' \"$@\"\nexit 37\n")
        self.make_command(self.tools, "host-tool", "printf 'host\\n'\n")
        result = self.run_script("with-toolchain-profile-test", "host-tool", env={"NEOHASKELL_ROOT": str(self.root)})
        self.assertEqual(result.returncode, 37, result)
        self.assertEqual(result.stdout.splitlines(), ["profile:develop", "profile:--command", "profile:host-tool"])

    def test_no_nix_uses_loud_host_fallback_and_preserves_exit_status(self) -> None:
        self.make_command(self.tools, "host-tool", "printf 'host:%s:%s\\n' \"$NEOHASKELL_TOOLCHAIN_MODE\" \"$@\"\nexit 23\n")
        result = self.run_script("with-toolchain", "host-tool", "arg", env={"NEOHASKELL_ROOT": str(self.root)})
        self.assertEqual(result.returncode, 23, result)
        self.assertEqual(result.stdout, "host:host:arg\n")
        self.assertIn("using unpinned host tools", result.stderr)

    def test_no_command_fails_with_usage(self) -> None:
        result = self.run_script("with-toolchain")
        self.assertEqual(result.returncode, 2)
        self.assertIn("usage:", result.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
