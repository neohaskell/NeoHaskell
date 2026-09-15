"""Behavioral release contracts; isolated Git histories and an in-memory publisher."""

import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(Path(__file__).parent))
import engine as release

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
        self.temp = tempfile.TemporaryDirectory()
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
        self.write(
            "installer/Cargo.toml",
            '[package]\nname = "neo-install"\nversion = "0.4.1"\n',
        )
        self.write("installer/CHANGELOG.md", "# Historical installer releases\n")
        self.base = self.commit("ci: install inactive automation")
        self.git("tag", "neo-v0.4.1")
        self.git("tag", "installer-v0.4.1")
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
        for target in release.TARGETS:
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
        return remote.seal_artifacts(plan, revision, directory)

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
        # The real artifacts directory is outside Git; keep fixture commits faithful.
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


class Prepare(History):
    def test_lockstep_excludes_retired_installer(self):
        plan, revision = self.bootstrap()
        self.assertTrue(release.verify_generated(self.repo, revision, plan))
        for path in release.CONFIG["groups"]["platform"]["packages"]:
            self.assertIn("0.4.2", self.repo.read(revision, path))
        self.assertIn("0.4.1", self.repo.read(revision, "installer/Cargo.toml"))
        self.assertEqual(
            self.repo.read(revision, "installer/CHANGELOG.md"),
            "# Historical installer releases\n",
        )
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
        import runpy

        checker = runpy.run_path(str(ROOT / "scripts/spec-check"))
        gate = checker["require_changed_spec"]
        previous = gate.__globals__["ROOT"]
        gate.__globals__["ROOT"] = self.root
        try:
            self.assertEqual(gate(plan["source_sha"]), 0)
            self.write("extra-change", "unauthorized metadata")
            self.git("add", ".")
            self.git("commit", "-q", "--amend", "--no-edit")
            self.assertEqual(gate(plan["source_sha"]), 1)
        finally:
            gate.__globals__["ROOT"] = previous

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
        self.assertTrue(remote.restore_bundle(self.api, plan, r, self.root / "assets"))
        restored = remote.seal_artifacts(plan, r, self.root / "assets")
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
        receipt = directory / ("receipt-" + release.TARGETS[0] + ".json")
        data = json.loads(receipt.read_text())
        data["revision"] = "0" * 40
        receipt.write_text(release.canonical(data))
        with self.assertRaisesRegex(ValueError, "receipt"):
            remote.seal_artifacts(plan, r, directory)
        receipt.unlink()
        with self.assertRaisesRegex(ValueError, "Missing"):
            remote.seal_artifacts(plan, r, directory)

    def test_starter_and_consumer_provenance(self):
        plan, r = self.bootstrap()
        self.files(plan, r)
        directory = self.root / "assets"
        (directory / "neo-starter-flake.lock").write_text("different seed")
        with self.assertRaisesRegex(ValueError, "receipt"):
            remote.seal_artifacts(plan, r, directory)
        self.files(plan, r)
        (directory / "consumer.json").write_text("{}")
        with self.assertRaisesRegex(ValueError, "consumer"):
            remote.seal_artifacts(plan, r, directory)

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

    def test_bootstrap_needs_notes_and_explicit_version(self):
        with self.assertRaisesRegex(ValueError, "notes"):
            release.plan_release(
                self.repo, "HEAD", "platform", {}, "2026-09-15", ("neo-v0.4.1", "0.4.2")
            )
        with self.assertRaisesRegex(ValueError, "explicit"):
            remote.select_work(self.api, self.repo, self.base, "bootstrap")


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
    def test_only_platform_jobs_and_all_publication_gates_remain(self):
        import runpy

        check = runpy.run_path(str(ROOT / "scripts/workflow-check"))
        text = (ROOT / ".github/workflows/semantic-release.yml").read_text()
        jobs = check["job_blocks"](text)
        self.assertEqual(
            set(jobs),
            {
                "prepare",
                "platform_seed",
                "platform_native",
                "platform_consumer",
                "platform_publish",
                "reconcile",
            },
        )
        self.assertNotIn("installer", text)
        for stage in ("seed", "native", "consumer", "publish"):
            self.assertIn(
                "needs.prepare.outputs.platform_has_work == 'true'",
                jobs[f"platform_{stage}"],
            )
        self.assertIn(
            "needs: [prepare, platform_native, platform_consumer]",
            jobs["platform_publish"],
        )
        self.assertIn("needs: platform_publish", jobs["reconcile"])
        self.assertIn("needs.platform_publish.result == 'success'", jobs["reconcile"])

    def test_work_outputs_are_platform_only_and_preserve_reuse(self):
        import runpy

        cli = runpy.run_path(str(ROOT / "scripts/semantic-release"))
        item = {
            "group": "platform",
            "attempt": "platform",
            "revision": "a" * 40,
            "reuse": True,
        }
        for selected in ([], [item]):
            outputs = cli["work_outputs"](selected)
            self.assertEqual(
                set(outputs), {"platform_has_work", "platform_items", "platform_matrix"}
            )
            self.assertEqual(outputs["platform_has_work"], str(bool(selected)).lower())
            self.assertEqual(json.loads(outputs["platform_items"]), selected)
            matrix = json.loads(outputs["platform_matrix"])["include"]
            self.assertEqual(
                {row["target"] for row in matrix},
                set(release.TARGETS) if selected else set(),
            )
            self.assertTrue(
                all(row["reuse"] and row["group"] == "platform" for row in matrix)
            )
        with self.assertRaisesRegex(ValueError, "release group"):
            cli["work_outputs"]([{**item, "group": "installer"}])

    def test_runner_mapping_survives_target_reordering(self):
        import runpy
        from unittest.mock import patch

        cli = runpy.run_path(str(ROOT / "scripts/semantic-release"))
        expected = {
            "x86_64-unknown-linux-gnu": "ubuntu-latest",
            "aarch64-unknown-linux-gnu": "ubuntu-24.04-arm",
            "x86_64-apple-darwin": "macos-15-intel",
            "aarch64-apple-darwin": "macos-latest",
        }
        with patch.object(release, "TARGETS", tuple(reversed(release.TARGETS))):
            output = cli["work_outputs"]([{"group": "platform"}])
            rows = json.loads(output["platform_matrix"])["include"]
            self.assertEqual({row["target"]: row["runner"] for row in rows}, expected)
        with patch.object(release, "TARGETS", (*release.TARGETS, "unsupported")):
            with self.assertRaisesRegex(ValueError, "runners"):
                cli["work_outputs"]([])

    def test_main_only_and_no_legacy_publishers(self):
        import runpy

        check = runpy.run_path(str(ROOT / "scripts/workflow-check"))
        text = (ROOT / ".github/workflows/semantic-release.yml").read_text()
        self.assertEqual(
            check["check_semantic_release"]("semantic-release.yml", text), []
        )
        for old, new in [
            ("cancel-in-progress: false", "cancel-in-progress: true"),
            ("needs: [prepare, platform_native, platform_consumer]", "needs: prepare"),
            ("!cancelled()", "true"),
            ("github.ref == 'refs/heads/main'", "github.ref != 'refs/heads/main'"),
            ("persist-credentials: false", "persist-credentials: true"),
        ]:
            with self.subTest(mutation=old):
                self.assertTrue(
                    check["check_semantic_release"](
                        "semantic-release.yml", text.replace(old, new)
                    )
                )
        checks = (ROOT / ".github/workflows/checks.yml").read_text()
        self.assertIn("ready_for_review, edited]", checks)
        self.assertIn("RELEASE_PR_TITLE:", checks)
        self.assertIn("RELEASE_PR_BODY:", checks)
        for name in ["neo-release.yml", "installer-ci.yml"]:
            old = (ROOT / ".github/workflows" / name).read_text()
            self.assertNotIn("softprops/action-gh-release", old)
            self.assertNotIn("contents: write", old)

    def test_installer_download_checks_run_for_platform_publication_changes(self):
        import re

        workflow = (ROOT / ".github/workflows/installer-ci.yml").read_text()
        pattern = re.search(r"PATTERN='([^']+)'", workflow)[1]
        for path in (
            "installer/tests/consistency.rs",
            "scripts/releases/config.json",
            "scripts/releases/engine.py",
            "scripts/releases/github.py",
            "scripts/semantic-release",
            "scripts/neo-release",
            ".github/workflows/semantic-release.yml",
            ".github/workflows/installer-ci.yml",
        ):
            with self.subTest(path=path):
                self.assertRegex(path, pattern)
        self.assertNotRegex("website/package.json", pattern)

    def test_pr_rollout_ledger_is_empty(self):
        # This assertion intentionally expires at the first generated preparation:
        # thereafter validate every record instead of requiring perpetual inactivity.
        ledger = json.loads((ROOT / release.LEDGER_PATH).read_text())
        for plan in ledger["attempts"] + ledger["recoveries"]:
            release.check_identity(plan)
        self.assertEqual(ledger["schema"], 1)


GROUPS = {
    "versions": Versions,
    "fragments": Fragments,
    "render": Render,
    "prepare": Prepare,
    "provenance": Provenance,
    "recovery": Recovery,
    "artifacts": Artifacts,
    "workflow": Workflow,
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
