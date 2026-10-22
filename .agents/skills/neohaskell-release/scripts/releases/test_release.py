#!/usr/bin/env python3

import copy
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).parent))
import engine as release

ROOT = release.ROOT
SKILL_ROOT = release.SKILL_ROOT
STATE = ROOT / ".agents-state/neohaskell-release/tests"

NOTE = """---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

Saving an item now reports completion once, even after reconnecting.
"""


class Versions(unittest.TestCase):
    def test_pre_one(self):
        self.assertEqual(release.bump("0.4.2", "breaking"), "0.5.0")
        self.assertEqual(release.bump("0.4.2", "compatible"), "0.4.3")
        self.assertEqual(release.bump("0.4.2", "none"), "0.4.2")
        for bad in ["1.0.0", "0.01.0", "0.2", "v0.2.1", "0.2.1-rc.1"]:
            with self.assertRaises(ValueError):
                release.version(bad)

    def test_commit_impact(self):
        for msg in [
            "feat!: remove old names",
            "fix(api)!: change output",
            "chore: revise\n\nBREAKING CHANGE: old names removed",
            "fix: revise\n\nBREAKING-CHANGE: old names removed",
        ]:
            self.assertEqual(release.commit_impact(msg), "breaking")
        self.assertEqual(release.commit_impact("ci: configure releases"), "none")


class LedgerLocations(unittest.TestCase):
    def ledger_repo(self, values):
        repo = release.Repo(ROOT)
        repo.read = lambda _revision, path: values.get(path)
        return repo

    def test_canonical_history_can_advance(self):
        canonical_path = ROOT / release.LEDGER_PATH
        before = canonical_path.read_bytes()
        newer = json.loads(before)
        newer["attempts"].append(
            release.identify(
                {
                    "group": "platform",
                    "source_sha": "a" * 40,
                    "kind": "release",
                    "version": "0.99.0",
                    "notes": [],
                }
            )
        )
        repo = self.ledger_repo(
            {
                release.LEDGER_PATH: release.canonical(newer),
            }
        )
        self.assertEqual(repo.ledger("new"), newer)
        self.assertEqual(canonical_path.read_bytes(), before)

    def test_live_old_root_ledger_is_fallback(self):
        old = json.loads((ROOT / release.LEDGER_PATH).read_text())
        repo = self.ledger_repo(
            {release.LIVE_LEGACY_LEDGER_PATH: json.dumps(old)}
        )
        self.assertEqual(repo.ledger("old"), old)

    def test_missing_ledger_has_no_release_history(self):
        self.assertEqual(
            self.ledger_repo({}).ledger("initial"),
            {"schema": 1, "attempts": [], "recoveries": []},
        )

    def test_competing_live_ledgers_must_agree(self):
        canonical_data = {"schema": 1, "attempts": [], "recoveries": []}
        old_data = {
            "schema": 1,
            "attempts": [
                release.identify(
                    {
                        "group": "platform",
                        "source_sha": "b" * 40,
                        "kind": "release",
                        "version": "0.1.0",
                        "notes": [],
                    }
                )
            ],
            "recoveries": [],
        }
        repo = self.ledger_repo(
            {
                release.LEDGER_PATH: release.canonical(canonical_data),
                release.LIVE_LEGACY_LEDGER_PATH: release.canonical(old_data),
            }
        )
        with self.assertRaisesRegex(ValueError, "Inconsistent canonical and live"):
            repo.ledger("conflict")

    def test_git_history_reads_old_and_new_revision_ledgers(self):
        STATE.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(dir=STATE) as temporary:
            root = Path(temporary)
            repo = release.Repo(root)
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=root, check=True)
            subprocess.run(["git", "config", "user.email", "fixture@example.invalid"], cwd=root, check=True)
            subprocess.run(["git", "config", "user.name", "Fixture"], cwd=root, check=True)
            historical = (ROOT / release.LEDGER_PATH).read_text()
            old_path = root / release.LIVE_LEGACY_LEDGER_PATH
            old_path.parent.mkdir(parents=True)
            old_path.write_text(historical)
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "historical release history"], cwd=root, check=True)
            old_revision = repo.git("rev-parse", "HEAD")
            before = repo.ledger(old_revision)
            later = copy.deepcopy(before)
            later["attempts"].append(
                release.identify(
                    {
                        "group": "platform",
                        "source_sha": "c" * 40,
                        "kind": "release",
                        "version": "0.98.0",
                        "notes": [],
                    }
                )
            )
            current_path = root / release.LEDGER_PATH
            current_path.parent.mkdir(parents=True)
            current_path.write_text(release.canonical(later))
            old_path.unlink()
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "canonical release history"], cwd=root, check=True)
            new_revision = repo.git("rev-parse", "HEAD")
            self.assertEqual(repo.ledger(old_revision), before)
            self.assertEqual(repo.ledger(new_revision), later)


class TrustedTooling(unittest.TestCase):
    def test_engine_loads_owned_config_for_historical_product_source(self):
        import os
        STATE.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(dir=STATE) as temporary:
            root = Path(temporary)
            (root / ".github/workflows").mkdir(parents=True)
            (root / "neo").mkdir()
            (root / "neo/Cargo.toml").write_text("[package]\n")
            (root / "flake.nix").write_text("{}\n")
            code = (
                "import sys;sys.path.insert(0,sys.argv[1]);import engine;"
                "assert str(engine.ROOT)==sys.argv[2];"
                "assert str(engine.SKILL_ROOT)==sys.argv[3];"
                "assert engine.CONFIG['repository']=='neohaskell/NeoHaskell'"
            )
            result = subprocess.run(
                [sys.executable, "-c", code, str(SKILL_ROOT / "scripts/releases"), str(root), str(SKILL_ROOT)],
                env={**os.environ, "NEOHASKELL_ROOT": str(root), "PYTHONDONTWRITEBYTECODE": "1"},
                capture_output=True, text=True, timeout=20,
            )
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_cache_prime_clears_inherited_exact_source_for_every_command(self):
        import os
        STATE.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(dir=STATE) as temporary:
            root = Path(temporary)
            (root / "neo").mkdir()
            (root / "neo/Cargo.toml").write_text("[package]\n")
            (root / "flake.nix").write_text("{}\n")
            bins = root / "bin"
            bins.mkdir()
            neo = bins / "neo"
            neo.write_text(
                "#!" + sys.executable + "\n"
                "import json,os,sys,time\nfrom pathlib import Path\n"
                "with open(os.environ['CALL_LOG'],'a') as log: log.write(json.dumps({'verb':sys.argv[2],'override':os.getenv('NEO_NEOHASKELL_SOURCE')})+'\\n')\n"
                "if sys.argv[2]=='new':\n p=Path(sys.argv[3]);p.mkdir();(p/'flake.nix').write_text('neohaskellCommit = \\\"'+os.environ['PINNED_REVISION']+'\\\";\\n')\n"
                "if sys.argv[2]=='run': time.sleep(10)\n"
            )
            neo.chmod(0o755)
            nix = bins / "nix"
            nix.write_text("#!/bin/sh\nexit 0\n")
            nix.chmod(0o755)
            log = root / "calls.jsonl"
            expected_revision = "a" * 40
            result = subprocess.run(
                [str(SKILL_ROOT / "scripts/cache-prime")],
                env={**os.environ, "NEOHASKELL_ROOT": str(root), "NEO_BIN": str(neo),
                     "CALL_LOG": str(log), "PATH": str(bins) + os.pathsep + os.environ['PATH'],
                     "NEO_NEOHASKELL_SOURCE": "/must-not-reach-released-build",
                     "PRIME_EXPECTED_REVISION": expected_revision,
                     "PINNED_REVISION": expected_revision, "PRIME_RUN_TIMEOUT": "0.2"},
                capture_output=True, text=True, timeout=20,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            calls = [json.loads(line) for line in log.read_text().splitlines()]
            self.assertEqual([call['verb'] for call in calls], ['new', 'build', 'test', 'run'])
            self.assertEqual([call['override'] for call in calls], [None] * 4)

            log.write_text("")
            mismatch = subprocess.run(
                [str(SKILL_ROOT / "scripts/cache-prime")],
                env={**os.environ, "NEOHASKELL_ROOT": str(root), "NEO_BIN": str(neo),
                     "CALL_LOG": str(log), "PATH": str(bins) + os.pathsep + os.environ['PATH'],
                     "NEO_NEOHASKELL_SOURCE": "/must-not-reach-released-build",
                     "PRIME_EXPECTED_REVISION": expected_revision,
                     "PINNED_REVISION": "b" * 40, "PRIME_RUN_TIMEOUT": "0.2"},
                capture_output=True, text=True, timeout=20,
            )
            self.assertNotEqual(mismatch.returncode, 0)
            self.assertIn(f"expected {expected_revision}", mismatch.stderr)
            mismatch_calls = [json.loads(line) for line in log.read_text().splitlines()]
            self.assertEqual([call['verb'] for call in mismatch_calls], ['new'])


class Fragments(unittest.TestCase):
    def test_contract(self):
        note = release.fragment(".changes/save.md", NOTE)
        self.assertEqual(note["group"], "platform")
        for bad in [
            NOTE.replace("compatible", "breaking"),
            NOTE.replace("## Summary", "## Missing"),
            NOTE.replace("Framework", "Installer"),
            NOTE.replace("group: platform", "group: installer").replace(
                "Framework", "Installer"
            ),
            NOTE.replace("group: platform", "group: platform\ngroup: installer"),
            NOTE.replace(
                "Saving an item now reports completion once, even after reconnecting.",
                "TODO",
            ),
        ]:
            with self.assertRaises(ValueError):
                release.fragment(".changes/save.md", bad)
        for path in [".changes/../save.md", ".changes/nested/save.md", "/tmp/save.md"]:
            with self.assertRaises(ValueError):
                release.fragment(path, NOTE)


class Render(unittest.TestCase):
    def test_bootstrap_preserves_legacy_heading_separation(self):
        for tail in ("\nParagraph\n", "\n\n### Fixed\n\nOld note\n"):
            old = "# Changelog\n\n## Unreleased" + tail
            rendered = release.append_changelog(old, "## 0.4.2 — 2026-09-15\n", True)
            self.assertIn("## Legacy development notes" + tail, rendered)

    def test_render(self):
        note = release.fragment(".changes/save.md", NOTE)
        note["commit"] = "a" * 40
        plan = {
            "group": "platform",
            "version": "0.4.3",
            "base_version": "0.4.2",
            "base_tag": "neo-v0.4.2",
            "date": "2026-09-15",
            "notes": [note],
        }
        body = release.render(plan)
        self.assertIn("## 0.4.3 — 2026-09-15", body)
        self.assertIn("Framework", body)
        self.assertNotIn("Unreleased", body)
        self.assertNotIn("## Added", body)


import github as remote

BREAKING = (
    NOTE.replace("impact: compatible", "impact: breaking").replace(
        "category: Fixed", "category: Breaking changes"
    )
    + """
## Migration

Replace `saveOld` with `save` in apps that store items.

### Verify

Run your app tests and save an item twice; each save should report completion once.

## Agent prompt

```text
Find calls to saveOld in this app and replace them with save, retaining each argument.
Run the existing app tests, then add a regression test for saving twice.
Report files changed and any failing test. Do not change unrelated behavior.
```
"""
)


class FragmentDetails(unittest.TestCase):
    def test_breaking_verification_and_prompt_are_required(self):
        release.fragment(".changes/breaking.md", BREAKING)
        for bad in [
            BREAKING.replace("### Verify", "### Details"),
            BREAKING.replace("```text", "text").replace("```", ""),
            BREAKING.replace(
                "Run your app tests and save an item twice; each save should report completion once.",
                "TODO",
            ),
        ]:
            with self.assertRaises(ValueError):
                release.fragment(".changes/breaking.md", bad)

    def test_empty_verification_and_mislabeled_breaking_change(self):
        empty = BREAKING.replace(
            "Run your app tests and save an item twice; each save should report completion once.",
            "",
        )
        with self.assertRaises(ValueError):
            release.fragment(".changes/empty.md", empty)
        with self.assertRaises(ValueError):
            release.fragment(
                ".changes/wrong.md",
                BREAKING.replace("impact: breaking", "impact: compatible"),
            )

    def test_nested_fence_and_standalone_migration_versions(self):
        note = release.fragment(".changes/breaking.md", BREAKING)
        note["commit"] = "a" * 40
        plan = {
            "group": "platform",
            "version": "0.5.0",
            "base_version": "0.4.2",
            "base_tag": "neo-v0.4.2",
            "date": "2026-09-15",
            "notes": [note],
        }
        result = release.render(plan)
        self.assertIn("Migrate Framework from NeoHaskell 0.4.2 to 0.5.0.", result)
        self.assertIn("<details>", result)
        self.assertIn(note["sections"]["Migration"], result)
        self.assertEqual(result, release.render(plan))
        plan["group"] = "installer"
        with self.assertRaisesRegex(ValueError, "release group"):
            release.render(plan)


class History(unittest.TestCase):
    def setUp(self):
        STATE.mkdir(parents=True, exist_ok=True)
        self.temp = tempfile.TemporaryDirectory(dir=STATE)
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.repo = release.Repo(self.root)
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("config", "user.name", "Fixture")
        self.write(
            release.LEDGER_PATH,
            release.canonical({"schema": 1, "attempts": [], "recoveries": []}),
        )
        self.write(release.CONFIG_PATH, release.canonical(release.CONFIG))
        for profile in release.CONFIG["groups"].values():
            for path, name in profile["packages"].items():
                self.write(
                    path,
                    (
                        "name: " + name + "\nversion: 0.4.1\n"
                        if path.endswith(".cabal")
                        else '[package]\nname = "' + name + '"\nversion = "0.4.1"\n'
                    ),
                )
            for path, name in profile["locks"].items():
                self.write(
                    path, '[[package]]\nname = "' + name + '"\nversion = "0.4.1"\n'
                )
            self.write(
                profile["changelog"],
                "# Old history\n\n## [Unreleased]\n\nExisting published words stay intact.\n",
            )
        self.base = self.commit("ci: install inactive automation")
        self.git("tag", "neo-v0.4.1")
        self.api = FakeAPI()

    def git(self, *args):
        return self.repo.git(*args)

    def write(self, path, text):
        dest = self.root / path
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_text(text)

    def commit(self, message):
        self.git("add", ".")
        self.git("commit", "-qm", message)
        return self.git("rev-parse", "HEAD")

    def note(self, name="save", text=NOTE, message="fix: save once"):
        self.write(".changes/" + name + ".md", text)
        return self.commit(message)

    def prepare(self, plan):
        release.write_edits(self.root, release.expected_edits(self.repo, plan))
        return self.commit("ci(release): prepare reviewed release")

    def bootstrap(self):
        self.note()
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {}, "2026-09-15", ("neo-v0.4.1", "0.4.2")
        )
        return plan, self.prepare(plan)

    def files(self, plan, revision):
        directory = self.root / "assets"
        directory.mkdir(exist_ok=True)
        for child in directory.iterdir():
            child.unlink()
        for name in remote.required_assets(plan):
            (directory / name).write_bytes(("fixture " + name).encode())
        prefix = release.CONFIG["groups"][plan["group"]]["asset_prefix"]
        for target in release.targets_for(plan):
            receipt = {
                "attempt": plan["id"],
                "revision": revision,
                "target": target,
                "binary_hash": release.digest(
                    (directory / (prefix + target)).read_bytes()
                ),
                "group": plan["group"],
                "native_verified": True,
            }
            if plan["group"] == "platform":
                receipt["starter_hashes"] = {
                    n: release.digest((directory / ("neo-starter-" + n)).read_bytes())
                    for n in ("flake.nix", "flake.lock", "cabal.project")
                }
            (directory / ("receipt-" + target + ".json")).write_text(
                release.canonical(receipt)
            )
        if plan["group"] == "platform":
            (directory / "neo-compatibility.json").write_text(
                release.canonical(
                    {
                        "neo_version": plan["version"],
                        "neohaskell": {"source_revision": revision},
                    }
                )
            )
            (directory / "consumer.json").write_text(
                release.canonical(
                    {
                        "attempt": plan["id"],
                        "revision": revision,
                        "binary_hash": release.digest(
                            (
                                directory / (prefix + "x86_64-unknown-linux-gnu")
                            ).read_bytes()
                        ),
                        "verified": True,
                    }
                )
            )
        return remote.seal_artifacts(self.repo, plan, revision, directory)

    def publish_bootstrap(self):
        plan, revision = self.bootstrap()
        remote.publish(
            self.api,
            self.repo,
            plan,
            revision,
            self.files(plan, revision),
            lambda: None,
        )
        import shutil

        shutil.rmtree(self.root / "assets")
        return plan, revision


class FakeAPI:
    def __init__(self):
        self.refs = {}
        self.releases = {}
        self.writes = []
        self.prs = []
        self.next_asset = 1

    def ref(self, name):
        return self.refs.get(name)

    def ensure_ref(self, name, sha):
        if name in self.refs:
            if self.refs[name] != sha:
                raise ValueError("Conflicting immutable ref")
        else:
            self.refs[name] = sha
            self.writes.append(("ref", name))

    def paused(self, identity):
        return self.ref("heads/release-pause/" + identity) is not None

    def pause(self, identity, sha):
        if not self.paused(identity):
            self.ensure_ref("heads/release-pause/" + identity, sha)

    def resume(self, identity):
        del self.refs["heads/release-pause/" + identity]
        self.writes.append(("resume", identity))

    def get_release(self, tag):
        return self.releases.get(tag)

    def create_release(self, tag, sha, title, body):
        item = {
            "id": len(self.releases) + 1,
            "tag_name": tag,
            "draft": True,
            "prerelease": False,
            "body": body,
            "assets": [],
        }
        self.releases[tag] = item
        self.writes.append(("draft", tag))
        return item

    def assets(self, item):
        return item["assets"]

    def read_asset(self, asset):
        return asset["bytes"]

    def upload(self, item, name, content):
        item["assets"].append(
            {
                "id": self.next_asset,
                "name": name,
                "bytes": content,
                "digest": "sha256:" + release.digest(content),
            }
        )
        self.next_asset += 1
        self.writes.append(("upload", name))

    def publish(self, item):
        item["draft"] = False
        self.writes.append(("publish", item["tag_name"]))

    def make_pr(self, repo, plan, edits):
        self.prs.append((plan, edits))
        self.writes.append(("pr", plan["id"]))
        return "https://example.invalid/pr/1"


def artifact_files(repo, plan, revision, directory):
    directory.mkdir(parents=True, exist_ok=True)
    for name in remote.required_assets(plan):
        (directory / name).write_bytes(("fixture " + name).encode())
    prefix = release.CONFIG["groups"][plan["group"]]["asset_prefix"]
    for target in release.targets_for(plan):
        receipt = {
            "attempt": plan["id"],
            "revision": revision,
            "target": target,
            "binary_hash": release.digest((directory / (prefix + target)).read_bytes()),
            "group": plan["group"],
            "native_verified": True,
            "starter_hashes": {
                name: release.digest((directory / ("neo-starter-" + name)).read_bytes())
                for name in ("flake.nix", "flake.lock", "cabal.project")
            },
        }
        (directory / ("receipt-" + target + ".json")).write_text(
            release.canonical(receipt)
        )
    (directory / "neo-compatibility.json").write_text(
        release.canonical(
            {"neo_version": plan["version"], "neohaskell": {"source_revision": revision}}
        )
    )
    (directory / "consumer.json").write_text(
        release.canonical(
            {
                "attempt": plan["id"],
                "revision": revision,
                "binary_hash": release.digest(
                    (directory / (prefix + "x86_64-unknown-linux-gnu")).read_bytes()
                ),
                "verified": True,
            }
        )
    )
    return remote.seal_artifacts(repo, plan, revision, directory)


class TargetPolicies(History):
    def test_native_policy_is_included_before_plan_identity(self):
        plan, _revision = self.bootstrap()
        self.assertEqual(plan["target_policy"], release.TARGET_POLICY)
        self.assertEqual(
            plan["id"],
            release.digest(
                release.canonical(
                    {key: value for key, value in plan.items() if key != "id"}
                )
            ),
        )
        self.assertEqual(release.targets_for(plan), release.TARGETS)

    def test_stripped_and_rehashed_native_plan_fails_generated_validation(self):
        import runpy

        plan, revision = self.bootstrap()
        stripped = {key: value for key, value in plan.items() if key not in {"id", "target_policy"}}
        stripped["id"] = release.digest(release.canonical(stripped))
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            release.expected_edits(self.repo, stripped)
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            release.verify_plan(self.repo, stripped, {})
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            release.verify_generated(self.repo, revision, stripped)
        cli = runpy.run_path(str(SKILL_ROOT / "scripts/semantic-release"))
        seed_output = self.root / "invalid-seed"
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            cli["seed"](self.repo, stripped, revision, seed_output)
        self.assertFalse(seed_output.exists())
        build_output = self.root / "invalid-build"
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            cli["build_native"](
                self.repo, stripped, revision, release.TARGETS[0], "unused", build_output
            )
        self.assertFalse(build_output.exists())
        consumer_output = self.root / "invalid-consumer"
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            cli["consumer"](
                self.repo, stripped, revision, "unused", consumer_output
            )
        self.assertFalse(consumer_output.exists())
        restore_output = self.root / "invalid-restore"
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            remote.restore_bundle(self.api, self.repo, stripped, revision, restore_output)
        self.assertFalse(restore_output.exists())
        seal_output = self.root / "invalid-seal"
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            remote.seal_artifacts(self.repo, stripped, revision, seal_output)
        self.assertFalse(seal_output.exists())
        with self.assertRaisesRegex(ValueError, "does not match its source"):
            remote.publish(self.api, self.repo, stripped, revision, {}, lambda: None)
        self.assertEqual(self.api.writes, [])

    def test_source_validator_rejects_invalid_plan_and_recovery_markers(self):
        plan, revision = self.bootstrap()
        for field, value in (
            ("target_policy", None),
            ("target_policy", True),
            ("target_policy", "future"),
            ("targets", list(release.TARGETS)),
        ):
            with self.subTest(field=field, value=value):
                invalid = {key: item for key, item in plan.items() if key != "id"}
                invalid[field] = value
                invalid["id"] = release.digest(release.canonical(invalid))
                with self.assertRaises(ValueError):
                    release.validate_source_policy(self.repo, invalid)
        recovery = release.recovery_plan(
            self.repo, revision, plan["id"], "The release needs a source correction."
        )
        invalid_recovery = {key: value for key, value in recovery.items() if key != "id"}
        invalid_recovery["target_policy"] = release.TARGET_POLICY
        invalid_recovery["id"] = release.digest(release.canonical(invalid_recovery))
        with self.assertRaisesRegex(ValueError, "inherit"):
            release.validate_source_policy(self.repo, invalid_recovery)

    def test_current_source_policy_rejects_malformed_and_raw_target_metadata(self):
        for metadata in (
            {"native_policy": None},
            {"native_policy": True},
            {"native_policy": "future"},
            {"targets": list(release.TARGETS)},
        ):
            with self.subTest(metadata=metadata):
                config = dict(release.CONFIG)
                config.update(metadata)
                self.write(release.CONFIG_PATH, release.canonical(config))
                source = self.commit("ci: validate release target metadata")
                with self.assertRaises(ValueError):
                    release.source_target_policy(self.repo, source)
                self.assertEqual(self.api.writes, [])

    def test_record_target_policy_rejects_open_target_shapes(self):
        for metadata in (
            {"target_policy": None},
            {"target_policy": True},
            {"target_policy": "future"},
            {"targets": list(release.TARGETS)},
        ):
            with self.subTest(metadata=metadata), self.assertRaises(ValueError):
                release.record_target_policy(metadata)

    def test_legacy_source_without_marker_keeps_four_targets(self):
        config = json.loads(self.repo.read("HEAD", release.CONFIG_PATH))
        del config["native_policy"]
        self.write(release.CONFIG_PATH, release.canonical(config))
        self.commit("chore(release): preserve legacy target policy")
        self.note()
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {}, "2026-09-15", ("neo-v0.4.1", "0.4.2")
        )
        self.assertNotIn("target_policy", plan)
        self.assertEqual(release.targets_for(plan), release.LEGACY_TARGETS)
        self.assertEqual(
            release.validate_source_policy(self.repo, plan), release.LEGACY_TARGETS
        )


class ArchivedTargetCompatibility(unittest.TestCase):
    def test_published_and_frozen_retry_keep_historical_four_target_assets(self):
        historical_targets = {
            "x86_64-unknown-linux-gnu",
            "aarch64-unknown-linux-gnu",
            "x86_64-apple-darwin",
            "aarch64-apple-darwin",
        }
        historical_assets = {
            "neo-x86_64-unknown-linux-gnu",
            "neo-aarch64-unknown-linux-gnu",
            "neo-x86_64-apple-darwin",
            "neo-aarch64-apple-darwin",
            "receipt-x86_64-unknown-linux-gnu.json",
            "receipt-aarch64-unknown-linux-gnu.json",
            "receipt-x86_64-apple-darwin.json",
            "receipt-aarch64-apple-darwin.json",
            "neo-starter-flake.nix",
            "neo-starter-flake.lock",
            "neo-starter-cabal.project",
            "neo-compatibility.json",
            "consumer.json",
        }
        sealed_assets = historical_assets | {
            "release-manifest.json", "release-bundle.zip", "SHA256SUMS"
        }
        STATE.mkdir(parents=True, exist_ok=True)
        repo = release.Repo(ROOT)
        main = repo.revision("HEAD")
        ledger = repo.ledger(main)
        plans = {
            plan["version"]: plan
            for plan in ledger["attempts"]
            if plan["version"] in {"0.10.0", "0.10.1"}
        }
        self.assertEqual(set(plans), {"0.10.0", "0.10.1"})
        self.assertEqual(
            plans["0.10.0"]["id"],
            "a23ecfb94187b2f638db5d92d6b496a5d62b13be51c9434e4f0a218f2e692764",
        )
        self.assertEqual(
            plans["0.10.1"]["id"],
            "6c5252580ac71f70477f0794cf00ae0d8da23cab2dfd50a46450c9e1a5ca5b2e",
        )
        for plan in plans.values():
            self.assertNotIn("target_policy", plan)
            self.assertEqual(set(release.validate_source_policy(repo, plan)), historical_targets)
            self.assertEqual(set(release.targets_for(plan)), historical_targets)
            self.assertEqual(remote.required_assets(plan), historical_assets)
            self.assertIsNone(repo.read(plan["source_sha"], release.LEDGER_PATH))
            self.assertIsNotNone(repo.read(plan["source_sha"], release.LIVE_LEGACY_LEDGER_PATH))
            self.assertNotIn(
                plan["id"],
                {record["id"] for record in repo.ledger(plan["source_sha"])["attempts"]},
            )

        with tempfile.TemporaryDirectory(dir=STATE) as temporary:
            root = Path(temporary)
            api = FakeAPI()
            published = plans["0.10.0"]
            published_revision = release.revision_for(repo, main, published)
            published_files = artifact_files(
                repo, published, published_revision, root / "published"
            )
            self.assertEqual(set(published_files), sealed_assets)
            remote.publish(
                api, repo, published, published_revision, published_files, lambda: None
            )

            pending = plans["0.10.1"]
            pending_revision = release.revision_for(repo, main, pending)
            pending_files = artifact_files(
                repo, pending, pending_revision, root / "pending"
            )
            self.assertEqual(set(pending_files), sealed_assets)
            item = api.create_release(
                release.release_tag(pending),
                pending_revision,
                "NeoHaskell " + pending["version"],
                release.render(pending),
            )
            api.upload(item, "release-bundle.zip", pending_files["release-bundle.zip"])

            result = remote.select_work(api, repo, main)
            self.assertEqual(
                result["items"],
                [{
                    "group": "platform",
                    "attempt": pending["id"],
                    "revision": pending_revision,
                    "reuse": True,
                }],
            )
            import runpy

            cli = runpy.run_path(str(SKILL_ROOT / "scripts/semantic-release"))
            matrix = json.loads(
                cli["work_outputs"](result["items"])["platform_matrix"]
            )["include"]
            self.assertEqual({row["target"] for row in matrix}, historical_targets)

            restored = root / "restored"
            self.assertTrue(
                remote.restore_bundle(api, repo, pending, pending_revision, restored)
            )
            self.assertEqual(
                remote.seal_artifacts(repo, pending, pending_revision, restored),
                pending_files,
            )
            remote.publish(api, repo, pending, pending_revision, pending_files, lambda: None)
            self.assertEqual(
                remote.completed_releases(api, repo, ledger),
                {published["id"]: published_revision, pending["id"]: pending_revision},
            )

            recovery = release.recovery_plan(
                repo, main, pending["id"], "The frozen release needs a source correction."
            )
            self.assertNotIn("target_policy", recovery)
            self.assertEqual(set(release.validate_source_policy(repo, recovery)), historical_targets)


class Prepare(History):
    def test_lockstep_updates_platform_inputs(self):
        plan, revision = self.bootstrap()
        self.assertTrue(release.verify_generated(self.repo, revision, plan))
        for path in release.CONFIG["groups"]["platform"]["packages"]:
            self.assertIn("0.4.2", self.repo.read(revision, path))
        self.assertIsNone(self.repo.read(revision, ".changes/save.md"))
        changelog = self.repo.read(revision, "CHANGELOG.md")
        self.assertIn(release.render(plan), changelog)
        self.assertIn("Existing published words stay intact.", changelog)
        self.assertNotIn("Unreleased", changelog)

    def test_next_version_from_squash_commit(self):
        first, r = self.publish_bootstrap()
        self.note(
            "another",
            NOTE.replace("Saving", "Updating"),
            message="feat: improve updates",
        )
        public = remote.completed_releases(
            self.api, self.repo, self.repo.ledger("HEAD")
        )
        plan = release.plan_release(self.repo, "HEAD", "platform", public, "2026-09-16")
        self.assertEqual(plan["version"], "0.4.3")
        self.assertEqual(plan["base_sha"], r)
        self.assertEqual([n["path"] for n in plan["notes"]], [".changes/another.md"])
        with self.assertRaisesRegex(ValueError, "release group"):
            release.plan_release(self.repo, "HEAD", "installer", public, "2026-09-16")

    def test_real_merge_commit_retains_notes_and_first_parent_origin(self):
        first, baseline = self.publish_bootstrap()
        self.git("checkout", "-qb", "feature")
        self.write("source.txt", "save correctly")
        self.commit("fix: save correctly")
        self.note("merged", message="docs: explain the fix")
        self.assertEqual(
            release.check_checkout(self.repo, baseline), "release fragments valid"
        )
        self.git("checkout", "-q", "main")
        self.git("merge", "--no-ff", "feature", "-m", "fix: merge the save repair")
        merged = self.git("rev-parse", "HEAD")
        self.assertEqual(
            len(self.git("rev-list", "--parents", "-n", "1", merged).split()), 3
        )
        plan = release.plan_release(
            self.repo, merged, "platform", {first["id"]: baseline}, "2026-09-16"
        )
        self.assertEqual(plan["version"], "0.4.3")
        self.assertEqual(
            [(n["path"], n["commit"]) for n in plan["notes"]],
            [(".changes/merged.md", merged)],
        )
        self.assertTrue(release.verify_plan(self.repo, plan, {first["id"]: baseline}))
        prepared = self.prepare(plan)
        self.assertTrue(release.verify_generated(self.repo, prepared, plan))

    def test_rebase_merge_allows_notes_after_implementation(self):
        first, baseline = self.publish_bootstrap()
        self.git("checkout", "-qb", "feature")
        self.write("source.txt", "save correctly")
        self.commit("fix: save correctly")
        original = self.note("rebased", message="docs: explain the fix")
        self.assertEqual(
            release.check_checkout(self.repo, baseline), "release fragments valid"
        )
        self.git("checkout", "-q", "main")
        self.write("ci-config.txt", "independent main change")
        self.commit("ci: configure checks")
        self.git("checkout", "-q", "feature")
        self.git("rebase", "main")
        introducing = self.git("rev-parse", "HEAD")
        self.assertNotEqual(introducing, original)
        self.git("checkout", "-q", "main")
        self.git("merge", "--ff-only", "feature")
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {first["id"]: baseline}, "2026-09-16"
        )
        self.assertEqual(plan["version"], "0.4.3")
        self.assertEqual(
            [(n["path"], n["commit"]) for n in plan["notes"]],
            [(".changes/rebased.md", introducing)],
        )
        self.assertTrue(release.verify_plan(self.repo, plan, {first["id"]: baseline}))

    def test_later_notes_repair_history_without_rewriting_commits(self):
        first, baseline = self.publish_bootstrap()
        self.write("source.txt", "fixed")
        implementation = self.commit("fix: save correctly")
        with self.assertRaisesRegex(ValueError, "fragment"):
            release.plan_release(
                self.repo, "HEAD", "platform", {first["id"]: baseline}, "2026-09-16"
            )
        introducing = self.note("late", message="docs: supply missing release notes")
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {first["id"]: baseline}, "2026-09-16"
        )
        self.assertTrue(self.repo.ancestor(implementation, plan["source_sha"]))
        self.assertEqual(plan["notes"][0]["commit"], introducing)
        self.assertEqual(plan["version"], "0.4.3")

    def test_separate_breaking_commit_requires_migration_in_pending_notes(self):
        first, baseline = self.publish_bootstrap()
        self.write("source.txt", "new save API")
        self.commit("feat!: rename save")
        self.note("compatible", message="docs: describe compatible improvements")
        with self.assertRaisesRegex(ValueError, "migration"):
            release.plan_release(
                self.repo, "HEAD", "platform", {first["id"]: baseline}, "2026-09-16"
            )
        self.note("migration", BREAKING, "docs: explain how to migrate")
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {first["id"]: baseline}, "2026-09-16"
        )
        self.assertEqual(plan["impact"], "breaking")
        self.assertEqual(plan["version"], "0.5.0")
        self.assertEqual(len(plan["notes"]), 2)

    def test_none_fragment_cannot_cover_releasable_history(self):
        first, baseline = self.publish_bootstrap()
        self.write("source.txt", "fixed")
        self.commit("fix: save correctly")
        self.note(
            "internal",
            NOTE.replace("impact: compatible", "impact: none"),
            "ci: document internal work",
        )
        with self.assertRaisesRegex(ValueError, "user-facing"):
            release.plan_release(
                self.repo, "HEAD", "platform", {first["id"]: baseline}, "2026-09-16"
            )

    def test_repeated_main_snapshot_freezes_proposal_date(self):
        first, r = self.publish_bootstrap()
        self.note("next")
        main = self.git("rev-parse", "HEAD")
        remote.select_work(self.api, self.repo, main)
        original = self.api.prs[-1][0]
        remote.select_work(self.api, self.repo, main)
        self.assertEqual(self.api.prs[-1][0], original)
        import datetime

        self.assertEqual(
            original["date"],
            datetime.datetime.fromtimestamp(
                int(self.git("show", "-s", "--format=%ct", main)), datetime.timezone.utc
            )
            .date()
            .isoformat(),
        )

    def test_missing_fragment_fails_closed(self):
        first, r = self.publish_bootstrap()
        self.write("source.txt", "changed")
        self.commit("fix: missing notes")
        with self.assertRaisesRegex(ValueError, "fragment"):
            release.plan_release(
                self.repo, "HEAD", "platform", {first["id"]: r}, "2026-09-16"
            )

    def test_tag_collision_and_foreign_baseline(self):
        self.note()
        self.git("tag", "neo-v0.4.2")
        self.git("tag", "installer-v0.4.1")
        with self.assertRaisesRegex(ValueError, "exists"):
            release.plan_release(
                self.repo, "HEAD", "platform", {}, "2026-09-15", ("neo-v0.4.1", "0.4.2")
            )
        with self.assertRaisesRegex(ValueError, "baseline"):
            release.plan_release(
                self.repo,
                "HEAD",
                "platform",
                {},
                "2026-09-15",
                ("installer-v0.4.1", "0.4.3"),
            )


class Provenance(History):
    def test_extra_edit_fails_even_with_generated_subject(self):
        plan, r = self.bootstrap()
        self.write("unexpected", "bad")
        self.git("add", ".")
        self.git("commit", "--amend", "--no-edit", "-q")
        with self.assertRaisesRegex(ValueError, "Unaccounted"):
            release.verify_generated(self.repo, "HEAD", plan)

    def test_stale_preparation_rejected(self):
        self.note()
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {}, "2026-09-15", ("neo-v0.4.1", "0.4.2")
        )
        self.write("unrelated", "new main commit")
        self.commit("ci: newer main")
        revision = self.prepare(plan)
        with self.assertRaisesRegex(ValueError, "Stale"):
            release.verify_generated(self.repo, revision, plan)

    def test_identity_and_recomputed_plan(self):
        plan, r = self.bootstrap()
        self.assertTrue(release.verify_plan(self.repo, plan, {}))
        altered = copy.deepcopy(plan)
        altered["date"] = "2026-01-01"
        with self.assertRaisesRegex(ValueError, "identity"):
            release.check_identity(altered)
        altered = release.identify(
            {k: v for k, v in plan.items() if k != "id"} | {"impact": "breaking"}
        )
        with self.assertRaisesRegex(ValueError, "deterministic"):
            release.verify_plan(self.repo, altered, {})

    def test_generated_exception_checks_whole_diff(self):
        plan, r = self.bootstrap()
        self.assertIn("verified", release.check_checkout(self.repo, plan["source_sha"]))
        self.write("extra-change", "unauthorized metadata")
        self.git("add", ".")
        self.git("commit", "-q", "--amend", "--no-edit")
        with self.assertRaisesRegex(ValueError, "Unaccounted generated diff"):
            release.check_checkout(self.repo, plan["source_sha"])

    def test_synthetic_pr_merge_requires_feature_notes(self):
        self.git("checkout", "-qb", "feature")
        self.write("app.txt", "new behavior")
        self.commit("feat: add behavior")
        self.git("checkout", "-q", "main")
        self.git("merge", "--no-ff", "feature", "-m", "Merge feature into main")
        with self.assertRaisesRegex(ValueError, "fragment"):
            release.check_checkout(self.repo, self.base)

    def test_squash_title_requires_migration_even_with_ci_commits(self):
        self.note(message="ci: arrange files")
        with self.assertRaisesRegex(ValueError, "migration"):
            release.check_checkout(self.repo, self.base, "feat!: rename an API")

    def test_symlink_fragment(self):
        (self.root / ".changes").mkdir()
        (self.root / ".changes/evil.md").symlink_to(self.root / "CHANGELOG.md")
        self.commit("ci: unsafe fragment")
        with self.assertRaisesRegex(ValueError, "regular"):
            self.repo.fragments("HEAD")


class Recovery(History):
    def test_abandon_restore_burn_version_and_keep_public_baseline(self):
        first, r = self.publish_bootstrap()
        self.note("breaking", BREAKING, "feat!: rename save")
        plan = release.plan_release(
            self.repo, "HEAD", "platform", {first["id"]: r}, "2026-09-16"
        )
        failed = self.prepare(plan)
        self.assertEqual(plan["version"], "0.5.0")
        self.api.ensure_ref("tags/neo-v0.5.0", failed)
        self.api.create_release("neo-v0.5.0", failed, "failed", release.render(plan))
        self.write("source-fix", "corrected compiler error")
        self.commit("ci: repair release source")
        current = self.git("rev-parse", "HEAD")
        result = remote.select_work(
            self.api,
            self.repo,
            current,
            "recover",
            attempt=plan["id"],
            reason="The native build needs a source correction before it can ship.",
        )
        self.assertTrue(self.api.paused(plan["id"]))
        self.assertEqual(len(result["prs"]), 1)
        recovery = self.api.prs[-1][0]
        recovered = self.prepare(recovery)
        self.assertTrue(release.verify_generated(self.repo, recovered, recovery))
        self.assertEqual(self.repo.read(recovered, ".changes/breaking.md"), BREAKING)
        self.assertNotIn(
            release.render(plan), self.repo.read(recovered, "CHANGELOG.md")
        )
        self.assertIn(release.render(first), self.repo.read(recovered, "CHANGELOG.md"))
        replacement = release.plan_release(
            self.repo, recovered, "platform", {first["id"]: r}, "2026-09-17"
        )
        self.assertEqual(replacement["version"], "0.6.0")
        self.assertEqual(replacement["base_version"], "0.4.2")
        self.assertEqual(replacement["notes"][0]["commit"], plan["notes"][0]["commit"])
        self.assertEqual(self.api.ref("tags/neo-v0.5.0"), failed)
        with self.assertRaisesRegex(ValueError, "paused"):
            remote.publish(self.api, self.repo, plan, failed, {}, lambda: None)
        with self.assertRaisesRegex(ValueError, "abandoned"):
            remote.select_work(
                self.api, self.repo, recovered, "resume", attempt=plan["id"]
            )
        self.assertEqual(
            remote.select_work(
                self.api, self.repo, recovered, "recover", attempt=plan["id"]
            )["status"],
            "already abandoned",
        )

    def test_invalid_recovery_does_not_pause_publication(self):
        plan, revision = self.bootstrap()
        with self.assertRaises(ValueError):
            remote.select_work(
                self.api, self.repo, revision, "recover", attempt=plan["id"], reason=""
            )
        self.assertEqual(self.api.writes, [])

    def test_public_release_cannot_be_abandoned(self):
        plan, r = self.publish_bootstrap()
        with self.assertRaisesRegex(ValueError, "Published"):
            remote.select_work(self.api, self.repo, r, "recover", attempt=plan["id"])

    def test_identical_public_retry_is_read_only(self):
        plan, r = self.bootstrap()
        files = self.files(plan, r)
        self.assertTrue(
            remote.publish(self.api, self.repo, plan, r, files, lambda: None)
        )
        writes = list(self.api.writes)
        self.assertFalse(
            remote.publish(self.api, self.repo, plan, r, files, lambda: None)
        )
        self.assertEqual(self.api.writes, writes)

    def test_retry_restores_frozen_build_without_recompiling(self):
        plan, r = self.bootstrap()
        files = self.files(plan, r)
        item = self.api.create_release(
            release.release_tag(plan), r, "draft", release.render(plan)
        )
        self.api.upload(item, "release-bundle.zip", files["release-bundle.zip"])
        result = remote.select_work(self.api, self.repo, r)
        self.assertTrue(result["items"][0]["reuse"])
        import shutil

        shutil.rmtree(self.root / "assets")
        self.assertTrue(
            remote.restore_bundle(self.api, self.repo, plan, r, self.root / "assets")
        )
        restored = remote.seal_artifacts(self.repo, plan, r, self.root / "assets")
        self.assertEqual(restored, files)
        remote.publish(self.api, self.repo, plan, r, restored, lambda: None)
        self.assertEqual(
            sum(w == ("upload", "release-bundle.zip") for w in self.api.writes), 1
        )

    def test_partial_upload_retry_and_conflicting_asset(self):
        plan, r = self.bootstrap()
        files = self.files(plan, r)
        item = self.api.create_release(
            release.release_tag(plan), r, "draft", release.render(plan)
        )
        name = next(iter(files))
        self.api.upload(item, name, files[name])
        remote.publish(self.api, self.repo, plan, r, files, lambda: None)
        self.assertEqual(sum(1 for w in self.api.writes if w == ("upload", name)), 1)
        item["draft"] = True
        item["assets"][0]["digest"] = "sha256:" + "0" * 64
        with self.assertRaisesRegex(ValueError, "differs"):
            remote.publish(self.api, self.repo, plan, r, files, lambda: None)

    def test_pause_rechecked_before_final_publication(self):
        plan, r = self.bootstrap()
        files = self.files(plan, r)

        def guard():
            item = self.api.get_release(release.release_tag(plan))
            if item and len(item["assets"]) == len(files):
                raise ValueError("paused during upload")

        with self.assertRaisesRegex(ValueError, "paused"):
            remote.publish(self.api, self.repo, plan, r, files, guard)
        self.assertTrue(self.api.get_release(release.release_tag(plan))["draft"])


class Artifacts(History):
    def test_complete_platform_receipts_and_checksums(self):
        plan, r = self.bootstrap()
        files = self.files(plan, r)
        checks = remote.checksum_map(files["SHA256SUMS"])
        self.assertEqual(set(checks), set(files) - {"SHA256SUMS"})
        self.assertTrue(all(release.digest(files[n]) == h for n, h in checks.items()))

    def test_wrong_native_revision_and_missing_target(self):
        plan, r = self.bootstrap()
        self.files(plan, r)
        directory = self.root / "assets"
        receipt = directory / ("receipt-" + release.targets_for(plan)[0] + ".json")
        data = json.loads(receipt.read_text())
        data["revision"] = "0" * 40
        receipt.write_text(release.canonical(data))
        with self.assertRaisesRegex(ValueError, "receipt"):
            remote.seal_artifacts(self.repo, plan, r, directory)
        receipt.unlink()
        with self.assertRaisesRegex(ValueError, "Missing"):
            remote.seal_artifacts(self.repo, plan, r, directory)

    def test_starter_and_consumer_provenance(self):
        plan, r = self.bootstrap()
        self.files(plan, r)
        directory = self.root / "assets"
        (directory / "neo-starter-flake.lock").write_text("different seed")
        with self.assertRaisesRegex(ValueError, "receipt"):
            remote.seal_artifacts(self.repo, plan, r, directory)
        self.files(plan, r)
        (directory / "consumer.json").write_text("{}")
        with self.assertRaisesRegex(ValueError, "consumer"):
            remote.seal_artifacts(self.repo, plan, r, directory)

    def test_artifact_sealing_uses_configured_prefix(self):
        from unittest.mock import patch

        plan, revision = self.bootstrap()
        with patch.dict(release.CONFIG["groups"]["platform"], asset_prefix="platform-"):
            files = self.files(plan, revision)
            self.assertIn("platform-x86_64-unknown-linux-gnu", files)
            self.assertNotIn("neo-x86_64-unknown-linux-gnu", files)

    def test_installer_publication_is_not_supported(self):
        plan, r = self.bootstrap()
        plan["group"] = "installer"
        with self.assertRaisesRegex(ValueError, "release group"):
            remote.required_assets(plan)
        self.assertEqual(set(release.CONFIG["groups"]), {"platform"})
        self.assertEqual(set(release.COMPONENTS), {"platform"})


class Bootstrap(History):
    def test_runner_setup_retry_preserves_frozen_release_revision(self):
        plan, revision = self.bootstrap()
        ledger = self.repo.ledger(revision)
        self.write(".github/workflows/semantic-release.yml", "name: Correct runner prerequisites\n")
        current = self.commit("ci(release): install native build prerequisites")
        result = remote.select_work(self.api, self.repo, current)
        self.assertEqual(result["items"], [{
            "group": "platform", "attempt": plan["id"],
            "revision": revision, "reuse": False,
            "target_policy": release.TARGET_POLICY,
        }])
        self.assertEqual(result["prs"], [])
        self.assertEqual(self.repo.ledger(current), ledger)

    def test_installation_is_inactive_without_any_api_call(self):
        class NoAPI:
            def __getattr__(self, name):
                raise AssertionError("Inactive workflow called GitHub: " + name)

        result = remote.select_work(NoAPI(), self.repo, self.base)
        self.assertEqual(result["items"], [])
        self.assertEqual(result["prs"], [])
        self.assertIn("inactive", result["status"])
        self.note()
        self.assertIsNone(
            release.plan_release(self.repo, "HEAD", "platform", {}, "2026-09-15")
        )

    def test_manual_first_release_activates_only_after_verified_publication(self):
        plan, r = self.bootstrap()
        self.assertEqual(
            remote.completed_releases(self.api, self.repo, self.repo.ledger(r)), {}
        )
        result = remote.select_work(self.api, self.repo, r)
        self.assertEqual(
            result["items"],
            [
                {
                    "group": "platform",
                    "attempt": plan["id"],
                    "revision": r,
                    "reuse": False,
                    "target_policy": release.TARGET_POLICY,
                }
            ],
        )
        item = self.api.create_release(
            release.release_tag(plan), r, "manual", release.render(plan)
        )
        self.assertEqual(
            remote.completed_releases(self.api, self.repo, self.repo.ledger(r)), {}
        )
        item["draft"] = False
        with self.assertRaisesRegex(ValueError, "manifest"):
            remote.completed_releases(self.api, self.repo, self.repo.ledger(r))
        item["draft"] = True
        remote.publish(self.api, self.repo, plan, r, self.files(plan, r), lambda: None)
        self.assertEqual(
            remote.completed_releases(self.api, self.repo, self.repo.ledger(r)),
            {plan["id"]: r},
        )

    def test_failed_first_release_does_not_activate_automatic_preparation(self):
        plan, r = self.bootstrap()
        self.api.pause(plan["id"], r)
        recovery = release.recovery_plan(
            self.repo,
            r,
            plan["id"],
            "The first release cannot compile and needs a source correction.",
        )
        self.prepare(recovery)
        self.assertIsNone(
            release.plan_release(self.repo, "HEAD", "platform", {}, "2026-09-16")
        )
        result = remote.select_work(self.api, self.repo, self.git("rev-parse", "HEAD"))
        self.assertEqual(result["items"], [])
        self.assertEqual(result["prs"], [])

    def test_historical_bootstrap_validation_remains_and_new_creation_is_retired(self):
        with self.assertRaisesRegex(ValueError, "notes"):
            release.plan_release(
                self.repo, "HEAD", "platform", {}, "2026-09-15", ("neo-v0.4.1", "0.4.2")
            )
        plan, _revision = self.bootstrap()
        self.assertTrue(plan["bootstrap"])
        self.assertTrue(release.verify_plan(self.repo, plan, {}))
        with self.assertRaisesRegex(ValueError, "retired"):
            remote.select_work(self.api, self.repo, self.base, "bootstrap")
        self.assertEqual(self.api.writes, [])

    def test_github_writer_rejects_new_bootstrap_pr_before_remote_writes(self):
        from unittest.mock import patch

        api = object.__new__(remote.GitHub)
        plan = {"kind": "release", "bootstrap": True}
        with patch.object(remote.GitHub, "request") as request, patch.object(
            remote.GitHub, "ref"
        ) as ref:
            with self.assertRaisesRegex(ValueError, "retired"):
                remote.GitHub.make_pr(api, self.repo, plan, {})
        request.assert_not_called()
        ref.assert_not_called()


class GitHubReads(unittest.TestCase):
    def test_asset_redirect_does_not_forward_release_token(self):
        import io
        import urllib.request
        from unittest.mock import patch

        api = object.__new__(remote.GitHub)
        api.token = "fixture-secret"
        api.prefix = "repos/example/repo"

        def respond(request, timeout):
            self.assertEqual(
                request.get_header("Authorization"), "Bearer fixture-secret"
            )
            redirect = urllib.request.HTTPRedirectHandler().redirect_request(
                request,
                None,
                302,
                "Found",
                {},
                "https://release-assets.githubusercontent.com/asset?signed=fixture",
            )
            self.assertIsNone(redirect.get_header("Authorization"))
            return io.BytesIO(b"verified fixture binary")

        with patch("urllib.request.urlopen", side_effect=respond):
            self.assertEqual(api.read_asset({"id": 123}), b"verified fixture binary")

    def test_draft_lookup_paginates_when_tag_endpoint_only_returns_published(self):
        from unittest.mock import Mock

        api = remote.GitHub.__new__(remote.GitHub)
        tag = "neo-v0.5.0"
        draft = {"id": 17, "tag_name": tag, "draft": True}
        others = [{"id": n + 100, "tag_name": f"other-{n}"} for n in range(100)]
        api.request = Mock(side_effect=[None, others, [draft]])
        self.assertEqual(api.get_release(tag), draft)
        self.assertEqual(
            [call.args for call in api.request.call_args_list],
            [
                (f"releases/tags/{tag}",),
                ("releases?per_page=100&page=1",),
                ("releases?per_page=100&page=2",),
            ],
        )
        published = {**draft, "draft": False}
        api.request = Mock(return_value=published)
        self.assertEqual(api.get_release(tag), published)
        api.request.assert_called_once_with(f"releases/tags/{tag}")

    def test_missing_ambiguous_and_inaccessible_drafts_fail_safely(self):
        from unittest.mock import Mock

        api = remote.GitHub.__new__(remote.GitHub)
        tag = "neo-v0.5.0"
        api.request = Mock(side_effect=[None, []])
        self.assertIsNone(api.get_release(tag))
        for listing in (None, [{"tag_name": tag, "id": 1}, {"tag_name": tag, "id": 2}]):
            with self.subTest(listing=listing):
                api.request = Mock(side_effect=[None, listing])
                with self.assertRaises(ValueError):
                    api.get_release(tag)


class Workflow(unittest.TestCase):
    @staticmethod
    def job_block(workflow, name):
        lines = workflow.splitlines()
        try:
            start = lines.index(f"  {name}:") + 1
        except ValueError:
            raise AssertionError(f"missing workflow job: {name}")
        block = []
        for line in lines[start:]:
            if line.startswith("  ") and not line.startswith("    "):
                break
            block.append(line)
        return "\n".join(block)

    @staticmethod
    def workflow(name):
        return (ROOT / ".github/workflows" / name).read_text()

    def test_release_workflow_preserves_gates_and_native_toolchains(self):
        text = self.workflow("semantic-release.yml")
        job_text = text.split("jobs:\n", 1)[1]
        jobs = {
            line[2:-1]
            for line in job_text.splitlines()
            if line.startswith("  ") and not line.startswith("    ") and line.endswith(":")
        }
        self.assertEqual(
            jobs,
            {"prepare", "platform_seed", "platform_native", "platform_consumer", "platform_cache", "platform_publish", "reconcile"},
        )
        self.assertIn("push:\n    branches: [main]", text)
        self.assertIn("workflow_dispatch:", text)
        self.assertIn("options: [auto, recover, resume]", text)
        self.assertNotIn("baseline:", text)
        self.assertNotIn("version:", text)
        self.assertNotIn("pull_request:", text)
        self.assertNotIn("tags:", text)
        self.assertIn("permissions:\n  contents: read", text)
        self.assertIn("cancel-in-progress: false", text)
        self.assertIn("pull-requests: write", self.job_block(text, "prepare"))
        self.assertIn("contents: write", self.job_block(text, "prepare"))
        for name in ("platform_seed", "platform_native", "platform_consumer"):
            block = self.job_block(text, name)
            self.assertIn("github.ref == 'refs/heads/main'", block)
            self.assertIn("needs.prepare.outputs.platform_has_work == 'true'", block)
            self.assertNotIn("permissions:", block)
        self.assertEqual(text.count("persist-credentials: false"), 12)
        self.assertIn("Release engine self-tests (offline)", text)
        self.assertIn("run: ./.agents/skills/neohaskell-release/scripts/semantic-release --self-test", text)
        for name in ("prepare", "platform_seed", "platform_native", "platform_consumer", "platform_publish", "reconcile"):
            self.assertIn(".agents/skills/neohaskell-release/scripts/semantic-release", self.job_block(text, name))
        self.assertIn("ref: ${{ github.sha }}", self.job_block(text, "prepare"))
        self.assertIn('revision=$(git rev-parse HEAD)', self.job_block(text, "prepare"))
        for name in ("platform_seed", "platform_native", "platform_consumer", "platform_publish"):
            block = self.job_block(text, name)
            self.assertIn("ref: ${{ matrix.revision }}", block)
            self.assertIn("ref: ${{ needs.prepare.outputs.tooling_revision }}", block)
            self.assertIn("path: .agents-state/release-tooling", block)
            self.assertIn("run: ./.agents-state/release-tooling/.agents/skills/neohaskell-release/scripts/semantic-release", block)
        self.assertIn("NEOHASKELL_ROOT: ${{ github.workspace }}", text)
        native = self.job_block(text, "platform_native")
        self.assertIn("cachix/install-nix-action@448cde739abec1eaf0646249e3d25bb8d9bc7702", native)
        self.assertLess(native.index("uses: DeterminateSystems/determinate-nix-action@"), native.index("semantic-release build-native"))
        consumer = self.job_block(text, "platform_consumer")
        self.assertIn("accept-flake-config = true", consumer)
        self.assertIn("ref: ${{ matrix.revision }}", consumer)
        self.assertIn("github.ref == 'refs/heads/main'", consumer)
        publish = self.job_block(text, "platform_publish")
        self.assertIn("needs: [prepare, platform_native, platform_consumer]", publish)
        self.assertIn("github.ref == 'refs/heads/main'", publish)
        self.assertIn("needs.prepare.outputs.platform_has_work == 'true'", publish)
        self.assertIn("contents: write", publish)
        cache = self.job_block(text, "platform_cache")
        self.assertIn("needs: [prepare, platform_native, platform_consumer]", cache)
        self.assertIn("continue-on-error: true", cache)
        self.assertIn("PRIME_EXPECTED_REVISION: ${{ matrix.revision }}", cache)
        self.assertIn("release-${{ matrix.attempt }}-x86_64-unknown-linux-gnu", cache)
        self.assertIn(".agents-state/release-tooling/.agents/skills/neohaskell-release/scripts/cache-prime", cache)
        self.assertIn("CACHIX_AUTH_TOKEN", cache)
        self.assertNotIn("platform_cache", publish)
        self.assertNotIn("CACHIX_AUTH_TOKEN", publish)
        self.assertNotIn("contents: write", cache)
        reconcile = self.job_block(text, "reconcile")
        self.assertIn("needs.platform_publish.result == 'success'", reconcile)
        self.assertIn("contents: write", reconcile)

    def test_cli_rejects_bootstrap_for_preview_and_orchestrate(self):
        script = SKILL_ROOT / "scripts/semantic-release"
        help_result = subprocess.run(
            [sys.executable, str(script), "--help"],
            cwd=ROOT,
            capture_output=True,
            text=True,
        )
        self.assertEqual(help_result.returncode, 0, help_result.stderr)
        self.assertNotIn("bootstrap", help_result.stdout)
        self.assertNotIn("--baseline", help_result.stdout)
        self.assertNotIn("--version", help_result.stdout)
        for command in ("preview", "orchestrate"):
            result = subprocess.run(
                [sys.executable, str(script), command, "--operation", "bootstrap"],
                cwd=ROOT,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("invalid choice: 'bootstrap'", result.stderr)

    def test_workflow_plan_command_matches_the_cli_parser(self):
        import os

        text = self.workflow("semantic-release.yml")
        start = text.index("      - id: plan")
        run_start = text.index("        run: >-\n", start) + len("        run: >-\n")
        command_lines = []
        for line in text[run_start:].splitlines():
            if not line.startswith("          "):
                break
            command_lines.append(line[10:].strip())
        command = " ".join(command_lines)
        self.assertNotIn("--baseline", command)
        self.assertNotIn("--version", command)
        result = subprocess.run(
            ["bash", "-euc", command + " --help"],
            cwd=ROOT,
            env={
                **os.environ,
                "OPERATION": "auto",
                "ATTEMPT": "",
                "REASON": "",
            },
            capture_output=True,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Prepare reviewed release PRs", result.stdout)

    def test_workflow_outputs_are_platform_only_and_preserve_reuse(self):
        import runpy

        cli = runpy.run_path(str(SKILL_ROOT / "scripts/semantic-release"))
        item = {
            "group": "platform",
            "attempt": "platform",
            "revision": "a" * 40,
            "reuse": True,
            "target_policy": release.TARGET_POLICY,
        }
        for selected in ([], [item]):
            outputs = cli["work_outputs"](selected)
            self.assertEqual(set(outputs), {"platform_has_work", "platform_items", "platform_matrix"})
            self.assertEqual(outputs["platform_has_work"], str(bool(selected)).lower())
            self.assertEqual(json.loads(outputs["platform_items"]), selected)
            matrix = json.loads(outputs["platform_matrix"])["include"]
            self.assertEqual({row["target"] for row in matrix}, set(release.TARGETS) if selected else set())
            self.assertTrue(all(row["reuse"] and row["group"] == "platform" for row in matrix))
        historical = {key: value for key, value in item.items() if key != "target_policy"}
        legacy_matrix = json.loads(
            cli["work_outputs"]([historical])["platform_matrix"]
        )["include"]
        self.assertEqual({row["target"] for row in legacy_matrix}, set(release.LEGACY_TARGETS))
        with self.assertRaisesRegex(ValueError, "release group"):
            cli["work_outputs"]([{**item, "group": "installer"}])

    def test_matrix_preserves_fresh_and_reused_items_per_target(self):
        import runpy
        from unittest.mock import patch

        cli = runpy.run_path(str(SKILL_ROOT / "scripts/semantic-release"))
        items = [
            {"group": "platform", "attempt": "a" * 64, "revision": "c" * 40, "reuse": False, "target_policy": release.TARGET_POLICY},
            {"group": "platform", "attempt": "b" * 64, "revision": "d" * 40, "reuse": True, "target_policy": release.TARGET_POLICY},
        ]
        outputs = cli["work_outputs"](items)
        rows = json.loads(outputs["platform_matrix"])["include"]
        self.assertEqual(len(rows), len(items) * len(release.TARGETS))
        self.assertEqual({(row["attempt"], row["target"]): row["reuse"] for row in rows},
                         {(item["attempt"], target): item["reuse"] for item in items for target in release.TARGETS})
        expected = {
            "x86_64-unknown-linux-gnu": "ubuntu-latest",
            "aarch64-apple-darwin": "macos-latest",
        }
        with patch.object(release, "TARGETS", tuple(reversed(release.TARGETS))):
            output = cli["work_outputs"]([{"group": "platform", "target_policy": release.TARGET_POLICY}])
            rows = json.loads(output["platform_matrix"])["include"]
            self.assertEqual({row["target"]: row["runner"] for row in rows}, expected)
        legacy_rows = json.loads(
            cli["work_outputs"]([{"group": "platform"}])["platform_matrix"]
        )["include"]
        self.assertEqual({row["target"] for row in legacy_rows}, set(release.LEGACY_TARGETS))

    def test_release_asset_and_consumer_jobs_use_owned_entrypoints(self):
        read_only = self.workflow("neo-release.yml")
        self.assertIn("contents: read", read_only)
        self.assertNotIn("contents: write", read_only)
        self.assertNotIn("secrets.", read_only)
        self.assertNotIn("softprops/action-gh-release", read_only)
        self.assertIn(".agents/skills/neohaskell-release/scripts/neo-release", read_only)

        ci = self.workflow("neo-ci.yml")
        contract = self.job_block(ci, "consumer-contract")
        self.assertIn(".agents/skills/neohaskell-consumer-verification/scripts/neo-consumer-contract", contract)
        self.assertIn(".agents/skills/neohaskell-release/scripts/neo-release", contract)
        self.assertIn(".agents/skills/neohaskell-release/scripts/cache-prime --self-test", contract)
        self.assertNotIn("CACHIX_AUTH_TOKEN", contract)
        self.assertIn("accept-flake-config = true", contract)
        ide = self.job_block(ci, "ide")
        self.assertIn("neo-dist-check --self-test", ide)
        self.assertIn("run: ./.agents/skills/neohaskell-consumer-verification/scripts/neo-dist-check\n", ide)
        self.assertNotIn("npm run build", ide)

    def test_trusted_cache_population_guard_and_exact_closure_commands(self):
        import re

        ci = self.workflow("neo-ci.yml")
        cache = self.job_block(ci, "cache-populate")
        for guard in (
            "github.event_name == 'push'",
            "github.ref == 'refs/heads/main'",
            "github.ref == 'refs/heads/integration/neo-monorepo'",
            "github.repository == 'neohaskell/NeoHaskell'",
        ):
            self.assertIn(guard, cache)
        self.assertIn("CACHIX_AUTH_TOKEN: ${{ secrets.CACHIX_AUTH_TOKEN }}", cache)
        self.assertRegex(
            cache,
            r"- name: Configure Cachix push\n\s+if: \$\{\{ env\.CACHIX_AUTH_TOKEN != '' \}\}\n\s+uses: cachix/cachix-action@v17",
        )
        self.assertIn("neo-consumer-contract", cache)
        self.assertNotIn("cache-prime", cache)
        self.assertIn("NEO_BIN=", cache)
        release_cache = self.job_block(self.workflow("semantic-release.yml"), "platform_cache")
        self.assertIn("cache-prime", release_cache)

    def test_native_asset_contract_covers_current_and_historical_targets(self):
        script = SKILL_ROOT / "scripts/neo-release"
        current = subprocess.check_output([str(script), "targets"], text=True).splitlines()
        self.assertEqual(set(current), set(release.TARGETS))
        self.assertEqual(release.CONFIG["native_policy"], release.TARGET_POLICY)
        for target in release.LEGACY_TARGETS:
            asset = subprocess.check_output(
                [str(script), "asset-name", target], text=True
            ).strip()
            self.assertEqual(asset, "neo-" + target)
        for target in release.TARGETS:
            self.assertIn(target, self.workflow("neo-release.yml"))
        for target in {"aarch64-unknown-linux-gnu", "x86_64-apple-darwin"}:
            self.assertNotIn(target, self.workflow("neo-release.yml"))
        profile = release.CONFIG["groups"]["platform"]
        self.assertEqual(profile["tag"], "neo-v")
        self.assertEqual(profile["asset_prefix"], "neo-")
        self.assertEqual(release.CONFIG["repository"], "neohaskell/NeoHaskell")
        self.assertIn("scripts/neo-release", (SKILL_ROOT / "scripts/semantic-release").read_text())
        for path in (
            SKILL_ROOT / "scripts/neo-release",
            SKILL_ROOT / "scripts/releases/github.py",
        ):
            text = path.read_text()
            self.assertIn("SHA256SUMS", text)
            self.assertNotIn("SHA256SUMS.txt", text)

    def test_release_pipeline_executes_owned_contract_checks(self):
        import re

        release_workflow = self.workflow("semantic-release.yml")
        self.assertIn(
            "run: ./.agents/skills/neohaskell-release/scripts/semantic-release --self-test",
            release_workflow,
        )
        self.assertTrue((SKILL_ROOT / "scripts/releases/test_release.py").is_file())
        release_script = (SKILL_ROOT / "scripts/semantic-release").read_text()
        self.assertIn("scripts/releases/test_release.py", release_script)
        self.assertIn("fetch-depth: 0", self.job_block(self.workflow("checks.yml"), "tooling"))
        ci = self.workflow("neo-ci.yml")
        match = re.search(r"CONTRACT='([^']+)'", ci)
        self.assertIsNotNone(match)
        for path in (
            "neo/src/main.rs",
            "core/src/Thing.hs",
            "integrations/src/Thing.hs",
            ".agents/skills/neohaskell-release/scripts/cache-prime",
            ".agents/skills/neohaskell-consumer-verification/scripts/neo-consumer-contract",
            ".agents/skills/neohaskell-consumer-verification/scripts/neo-dist-check",
        ):
            with self.subTest(path=path):
                self.assertRegex(path, match[1])

    def test_canonical_ledger_retains_recorded_release_history(self):
        ledger = json.loads((ROOT / release.LEDGER_PATH).read_text())
        recorded = {(plan["group"], plan["version"]) for plan in ledger["attempts"]}
        self.assertTrue({("platform", "0.10.0"), ("platform", "0.10.1")} <= recorded)
        for plan in ledger["attempts"] + ledger["recoveries"]:
            release.check_identity(plan)

    def test_prepared_required_checks_match_source_policy(self):
        config = json.loads((SKILL_ROOT / "references/required-checks.json").read_text())
        self.assertIs(config["strict"], False)
        self.assertEqual(
            {check["context"] for check in config["checks"]},
            {
                "hlint (dialect gate)",
                "ci-gate",
                "adr index (decisions README faithful)",
                "neo-ci-gate",
                "secret scan (gitleaks, deterministic)",
                "owned tooling checks",
                "release fragment check",
            },
        )
        self.assertEqual(len(config["checks"]), 7)
        self.assertEqual({check["app_id"] for check in config["checks"]}, {15368})


GROUPS = {
    "tooling": TrustedTooling,
    "versions": Versions,
    "fragments": Fragments,
    "render": Render,
    "prepare": Prepare,
    "provenance": Provenance,
    "recovery": Recovery,
    "artifacts": Artifacts,
    "target-policies": TargetPolicies,
    "archived-targets": ArchivedTargetCompatibility,
    "workflow": Workflow,
    "ledger": LedgerLocations,
    "bootstrap": Bootstrap,
}


def main():
    groups = sys.argv[1:] or list(GROUPS)
    unknown = set(groups) - set(GROUPS)
    if unknown:
        print("Unknown test group: " + ", ".join(sorted(unknown)), file=sys.stderr)
        return 1
    suite = unittest.TestSuite(
        unittest.defaultTestLoader.loadTestsFromTestCase(GROUPS[g]) for g in groups
    )
    if "fragments" in groups or "render" in groups:
        suite.addTests(
            unittest.defaultTestLoader.loadTestsFromTestCase(FragmentDetails)
        )
    if "recovery" in groups:
        suite.addTests(unittest.defaultTestLoader.loadTestsFromTestCase(GitHubReads))
    return not unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful()


if __name__ == "__main__":
    sys.exit(main())
