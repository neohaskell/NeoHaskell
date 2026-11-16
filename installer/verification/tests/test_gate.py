"""Exercise local Git gate behavior with isolated fixture policies and records."""

import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


POLICY = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("installer_verifier", POLICY / "verify.py")
VERIFY = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = VERIFY
SPEC.loader.exec_module(VERIFY)
FLAKE = '''{
  outputs = { self, pkgs }: let
    ciProject = pkgs.project { shell.tools = { doctest = "latest"; }; };
    flake = pkgs.project.flake {};
    neo = pkgs.callPackage ./nix/neo-package.nix {};
  in flake // {
    devShells = flake.devShells // { ci = ciProject.shell; };
    packages = flake.packages // { inherit neo; };
  };
}
'''


class GitFixture:
    def __init__(self, root, ready=False):
        self.root = root
        self.repo = root / "client"
        self.remote = root / "remote.git"
        self.env = dict(os.environ, GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull)
        self.repo.mkdir()
        self.git("init", "--quiet", "--initial-branch=main")
        self.git("config", "user.name", "Fixture")
        self.git("config", "user.email", "fixture@example.invalid")
        self.policy = self.repo / "installer/verification"
        self.policy.mkdir(parents=True)
        for name in ("verify.py", "inputs.json", "coverage.json"):
            shutil.copyfile(POLICY / name, self.policy / name)
        policy = json.loads((self.policy / "inputs.json").read_text())
        policy["flake_projection"]["independent_shapes"].append({
            "projection_sha256": "e044e408cd79adfa9de31c6dbf294b5c56c8f6ef7956a95d80061655137ea7d7",
            "dependencies": policy["flake_projection"]["independent_shapes"][0]["dependencies"],
        })
        (self.policy / "inputs.json").write_text(json.dumps(policy))
        recipe = POLICY.parents[1] / "nix/neo-package.nix"
        self.write("nix/neo-package.nix", recipe.read_text())
        if ready:
            coverage = json.loads((self.policy / "coverage.json").read_text())
            coverage.update(suite_id="fixture-only-not-platform-acceptance", status="ready",
                            support_floors={"status": "resolved", "values": {"fixture": True}},
                            common_required_cases=["fixture.case"], conditional_cases=[],
                            cases=[{"id": "fixture.case", "capability": "fixture"}],
                            capabilities={"fixture": "Controlled data for gate testing only."})
            for lane in coverage["lanes"]:
                lane.update(required_capabilities=["fixture"], required_cases=[])
            (self.policy / "coverage.json").write_text(json.dumps(coverage))
        self.write("flake.nix", FLAKE)
        self.write("README.md", "Fixture repository.\n")
        self.write("installer/src/main.rs", "fn main() {}\n")
        self.base = self.commit("fixture baseline")
        self.git("init", "--bare", "--quiet", str(self.remote))
        self.git("remote", "add", "origin", str(self.remote))
        self.git("push", "--quiet", "origin", "main")

    def git(self, *args, check=True, cwd=None, data=None):
        result = subprocess.run(["git", *args], cwd=cwd or self.repo, env=self.env,
                                input=data, text=True, capture_output=True, timeout=30)
        if check and result.returncode:
            raise AssertionError(f"git {args} failed with {result.returncode}: {result.stderr}")
        return result

    def write(self, path, data):
        target = self.repo / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(data)
        return target

    def commit(self, message):
        self.git("add", ".")
        self.git("commit", "--quiet", "-m", message)
        return self.git("rev-parse", "HEAD").stdout.strip()

    def cli(self, *args, cwd=None):
        return subprocess.run([sys.executable, "-B", str(self.policy / "verify.py"), *args],
                              cwd=cwd or self.repo, env=self.env, text=True,
                              capture_output=True, timeout=30)

    def setup(self):
        result = self.cli("setup-hooks", "--repo", str(self.repo))
        if result.returncode:
            raise AssertionError(result.stderr)
        return result

    def classify(self, base, target):
        result = self.cli("classify", "--repo", str(self.repo), "--base", base, "--target", target)
        if result.returncode:
            raise AssertionError(result.stderr)
        return json.loads(result.stdout)["families"]

    def evidence(self, source):
        old_global = os.environ.get("GIT_CONFIG_GLOBAL")
        os.environ["GIT_CONFIG_GLOBAL"] = os.devnull
        try:
            inputs, coverage, policy_digest = VERIFY.read_policy(self.policy)
            identity = VERIFY.input_identity(self.repo, source, self.policy)
            suite = VERIFY.suite_identity(self.repo, source, self.policy, coverage, policy_digest)
            root = VERIFY.store_root(self.repo)
            observed = b"Fixture observation. This is not a platform run."
            digest = VERIFY.digest_bytes(observed)
            VERIFY.write_atomic(root / "observations" / digest, observed)
            artifact = VERIFY.ArtifactIdentity("fixture", digest, len(observed))
            candidate = VERIFY.CandidateIdentity(digest, "0.0.2", source, (artifact,), "0.0.1", (artifact,))
            lanes = []
            for lane in coverage["lanes"]:
                lane_id = lane["id"]
                if lane_id.startswith("wsl"):
                    runner = dict(os="windows", architecture="fixture", service_manager="systemd",
                                  runner_id="fixture", wsl_architecture="x86_64" if lane_id.endswith("x86_64") else "aarch64",
                                  native_wsl_dispatch=True)
                else:
                    runner = dict(os="macos" if lane_id.startswith("macos") else "linux",
                                  architecture="arm64" if lane_id.startswith("macos") else "x86_64" if lane_id.endswith("x86_64") else "aarch64",
                                  service_manager="launchd" if lane_id.startswith("macos") else "systemd", runner_id="fixture")
                cases, unresolved = VERIFY.required_cases(coverage, lane_id)
                assert not unresolved
                lanes.append(dict(id=lane_id, runner=runner, capabilities=lane["required_capabilities"],
                                  cases=[dict(id=case, result="passed", observation=dict(path=f"observations/{digest}", sha256=digest)) for case in cases]))
            record = dict(schema_version=1, record_type="production", state="complete", tested_source_commit=source,
                          inputs=identity.as_json(), candidate=candidate.as_json(), suite=suite.as_json(), lanes=lanes)
            self.save_record(root, identity.as_json()["digest"], record)
            return root, identity.as_json()["digest"], record
        finally:
            if old_global is None:
                os.environ.pop("GIT_CONFIG_GLOBAL", None)
            else:
                os.environ["GIT_CONFIG_GLOBAL"] = old_global

    def save_record(self, root, input_digest, record):
        raw = VERIFY.canonical_json(record)
        digest = VERIFY.digest_bytes(raw)
        VERIFY.write_atomic(root / "evidence" / f"{digest}.json", raw)
        VERIFY.set_selection(root, input_digest, dict(schema_version=1, state="complete",
                            candidate_digest=record["candidate"]["digest"], suite_digest=record["suite"]["digest"],
                            record=f"evidence/{digest}.json", record_digest=digest))


class GateTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="neo-installer-gate-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def fixture(self, ready=False):
        return GitFixture(self.root, ready)

    def test_real_push_blocks_relevant_and_accepts_docs(self):
        f = self.fixture()
        f.setup()
        f.write("README.md", "Documentation only.\n")
        docs = f.commit("docs")
        self.assertEqual(f.git("push", "origin", "main", check=False).returncode, 0)
        f.write("installer/src/main.rs", "fn main() { println!(\"change\"); }\n")
        f.commit("installer change")
        rejected = f.git("push", "origin", "main", check=False)
        self.assertNotEqual(rejected.returncode, 0)
        self.assertIn("verify.py run", rejected.stderr)
        self.assertEqual(f.git("rev-parse", "refs/heads/main", cwd=f.remote).stdout.strip(), docs)

    def test_fixture_policy_acceptance_reuse_and_stale_rejection(self):
        f = self.fixture(ready=True)
        f.setup()
        f.write("installer/src/main.rs", "fn main() { println!(\"tested\"); }\n")
        source = f.commit("fixture candidate")
        f.evidence(source)
        f.write("README.md", "Later documentation.\n")
        f.commit("docs after evidence")
        accepted = f.git("push", "origin", "main", check=False)
        self.assertEqual(accepted.returncode, 0, accepted.stderr)
        f.write("installer/src/main.rs", "fn main() { println!(\"stale\"); }\n")
        f.commit("invalidate evidence")
        self.assertNotEqual(f.git("push", "origin", "main", check=False).returncode, 0)

    def test_new_refs_use_destination_reachability_and_foreign_head(self):
        f = self.fixture()
        f.git("switch", "-c", "other")
        f.write("installer/src/main.rs", "fn main() { println!(\"other\"); }\n")
        f.commit("other installer branch")
        f.git("push", "origin", "other")
        f.git("switch", "main")
        f.setup()
        f.git("switch", "-c", "docs")
        f.write("README.md", "New branch docs.\n")
        f.commit("new docs")
        f.git("switch", "other")
        result = f.git("push", "origin", "docs", check=False)
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_new_docs_branch_ignores_old_release_tag_inputs(self):
        f = self.fixture()
        f.git("tag", "-a", "old-release", "-m", "old fixture release")
        f.git("push", "origin", "old-release")
        f.write("installer/src/main.rs", "Already published installer change.\n")
        f.commit("published installer change")
        f.git("push", "origin", "main")
        f.setup()
        f.git("switch", "-c", "fresh-docs")
        f.write("README.md", "Documentation after published installer.\n")
        f.commit("new documentation")
        pushed = f.git("push", "origin", "fresh-docs", check=False)
        self.assertEqual(pushed.returncode, 0, pushed.stderr)
        self.assertIn("no installer inputs changed", pushed.stdout + pushed.stderr)

    def test_multiple_refs_and_deletion(self):
        f = self.fixture()
        f.setup()
        f.git("switch", "-c", "docs")
        f.write("README.md", "Docs.\n")
        f.commit("docs")
        f.git("switch", "-c", "changed", f.base)
        f.write("installer/src/main.rs", "changed\n")
        f.commit("relevant")
        rejected = f.git("push", "--atomic", "origin", "docs", "changed", check=False)
        self.assertNotEqual(rejected.returncode, 0)
        self.assertNotEqual(f.git("show-ref", "--verify", "refs/heads/docs", cwd=f.remote, check=False).returncode, 0)
        self.assertEqual(f.git("push", "origin", "docs", check=False).returncode, 0)
        self.assertEqual(f.git("push", "origin", ":docs", check=False).returncode, 0)

    def test_dirty_checkout_cannot_replace_committed_inputs(self):
        f = self.fixture()
        f.setup()
        f.write("installer/src/main.rs", "committed relevant change\n")
        f.commit("committed")
        f.write("installer/src/main.rs", "fn main() {}\n")
        self.assertNotEqual(f.git("push", "origin", "main", check=False).returncode, 0)

    def test_symlink_user_hook_keeps_invocation_path(self):
        f = self.fixture()
        dispatcher = self.root / "dispatcher"
        dispatcher.write_text('#!/bin/sh\n[ "${0##*/}" = pre-push ] || exit 23\ncat >/dev/null\n')
        dispatcher.chmod(0o755)
        prior = f.repo / ".git/hooks/pre-push"
        prior.symlink_to(dispatcher)
        f.setup()
        f.write("README.md", "Symlink hook docs.\n")
        f.commit("docs")
        pushed = f.git("push", "origin", "main", check=False)
        self.assertEqual(pushed.returncode, 0, pushed.stderr)
        self.assertTrue(prior.is_symlink())

    def test_dynamic_nix_access_does_not_hide_tooling_dependency(self):
        f = self.fixture()
        dynamic = FLAKE.replace("packages = flake.packages // { inherit neo; };",
                                'packages = { neo = (builtins.getAttr ("dev" + "Shells") self).ci; };')
        f.write("flake.nix", dynamic)
        before = f.commit("dynamic package dependency")
        f.write("flake.nix", dynamic.replace('doctest = "latest";', 'doctest = "next";'))
        after = f.commit("dynamic dependency changes")
        self.assertEqual(f.classify(before, after), ["nix-packaging"])

    def test_existing_hooks_arguments_stdin_and_repeated_setup(self):
        f = self.fixture()
        hooks = f.repo / "custom-hooks"
        hooks.mkdir()
        marker = self.root / "pre-commit-marker"
        precommit = hooks / "pre-commit"
        precommit.write_text(f"#!/bin/sh\nprintf preserved > '{marker}'\n")
        precommit.chmod(0o755)
        stdin_file = self.root / "pre-push-input"
        args_file = self.root / "pre-push-args"
        prior = hooks / "pre-push"
        prior.write_text(f"#!/bin/sh\nprintf '%s\\n' \"$@\" > '{args_file}'\ncat > '{stdin_file}'\n")
        prior.chmod(0o755)
        f.git("config", "core.hooksPath", "custom-hooks")
        original = prior.read_bytes()
        f.setup()
        f.setup()
        f.write("README.md", "Preserved hooks.\n")
        tip = f.commit("with hooks")
        self.assertEqual(marker.read_text(), "preserved")
        pushed = f.git("push", "origin", "main", check=False)
        self.assertEqual(pushed.returncode, 0, pushed.stderr)
        self.assertEqual(prior.read_bytes(), original)
        self.assertEqual(args_file.read_text().splitlines(), ["origin", str(f.remote)])
        self.assertIn(tip, stdin_file.read_text())

    def test_prior_hook_refusal_is_preserved(self):
        f = self.fixture()
        prior = f.repo / ".git/hooks/pre-push"
        prior.write_text("#!/bin/sh\nexit 19\n")
        prior.chmod(0o755)
        f.setup()
        f.write("README.md", "Docs.\n")
        f.commit("docs")
        self.assertNotEqual(f.git("push", "origin", "main", check=False).returncode, 0)

    def test_linked_worktree_push_and_default_hook(self):
        f = self.fixture()
        marker = self.root / "worktree-user-hook"
        prior = f.repo / ".git/hooks/pre-push"
        prior.write_text(f"#!/bin/sh\ncat > '{marker}'\n")
        prior.chmod(0o755)
        linked = self.root / "linked"
        f.git("worktree", "add", "-b", "linked", str(linked))
        f.setup()
        (linked / "README.md").write_text("Linked docs.\n")
        f.git("add", "README.md", cwd=linked)
        f.git("commit", "--quiet", "-m", "linked docs", cwd=linked)
        pushed = f.git("push", "origin", "linked", cwd=linked, check=False)
        self.assertEqual(pushed.returncode, 0, pushed.stderr)
        self.assertIn("refs/heads/linked", marker.read_text())

    def test_failed_setup_does_not_poison_retry(self):
        f = self.fixture()
        path = f.policy / "inputs.json"
        original = path.read_text()
        path.write_text(original + "\n")
        self.assertNotEqual(f.cli("setup-hooks").returncode, 0)
        self.assertNotEqual(f.git("config", "--get", "core.hooksPath", check=False).returncode, 0)
        path.write_text(original)
        f.setup()

    def test_flake_dependencies_docs_renames_and_owned_additions(self):
        f = self.fixture()
        f.write("flake.nix", FLAKE.replace('doctest = "latest";', 'doctest = "latest"; hoogle = "latest";'))
        tools = f.commit("dev tools")
        self.assertEqual(f.classify(f.base, tools), [])
        f.write("nix/neoctl-package.nix", "{}: {}\n")
        package = f.commit("package addition")
        self.assertEqual(f.classify(tools, package), ["neoctl-candidate"])
        f.git("mv", "nix/neoctl-package.nix", "other.nix")
        moved = f.commit("rename package")
        self.assertEqual(f.classify(package, moved), ["neoctl-candidate"])
        f.write("neo/starter/README.md", "Docs.\n")
        docs = f.commit("starter docs")
        self.assertEqual(f.classify(moved, docs), [])
        wired = FLAKE.replace("neo = pkgs.callPackage ./nix/neo-package.nix {};", 'neo = "${ciProject}/bin/neo";')
        f.write("flake.nix", wired)
        before = f.commit("tool dependency")
        f.write("flake.nix", wired.replace('doctest = "latest";', 'doctest = "next";'))
        after = f.commit("dependent tool change")
        self.assertEqual(f.classify(before, after), ["nix-packaging"])

    def test_incomplete_fixture_and_stale_records_cannot_pass(self):
        f = self.fixture(ready=True)
        f.setup()
        f.write("installer/src/main.rs", "candidate\n")
        source = f.commit("candidate")
        root, identity, record = f.evidence(source)
        record["record_type"] = "fixture"
        f.save_record(root, identity, record)
        self.assertNotEqual(f.git("push", "origin", "main", check=False).returncode, 0)
        record["record_type"] = "production"
        record["lanes"].pop()
        f.save_record(root, identity, record)
        self.assertNotEqual(f.git("push", "origin", "main", check=False).returncode, 0)

    def test_policy_change_invalidates_installed_snapshot(self):
        f = self.fixture()
        f.setup()
        path = f.policy / "inputs.json"
        path.write_text(path.read_text() + "\n")
        f.commit("policy change")
        self.assertNotEqual(f.git("push", "origin", "main", check=False).returncode, 0)


    def test_nested_user_git_hook_does_not_deadlock(self):
        f = self.fixture()
        prior = f.repo / ".git/hooks/pre-commit"
        prior.write_text("#!/bin/sh\ngit add README.md\n")
        prior.chmod(0o755)
        f.setup()
        f.write("README.md", "Nested Git operation.\n")
        f.git("add", "README.md")
        committed = f.git("commit", "--quiet", "-m", "nested user hook", check=False)
        self.assertEqual(committed.returncode, 0, committed.stderr)
        self.assertNotIn("locked", committed.stderr)

    def test_duplicate_lanes_and_corrupt_pointer_report_rejection(self):
        f = self.fixture(ready=True)
        f.setup()
        f.write("installer/src/main.rs", "candidate\n")
        source = f.commit("candidate")
        root, identity, record = f.evidence(source)
        record["lanes"].append(record["lanes"][0])
        f.save_record(root, identity, record)
        duplicate = f.git("push", "origin", "main", check=False)
        self.assertNotEqual(duplicate.returncode, 0)
        self.assertIn("duplicate lanes", duplicate.stderr)
        (root / "selection" / f"{identity}.json").write_text("[]")
        malformed = f.git("push", "origin", "main", check=False)
        self.assertNotEqual(malformed.returncode, 0)
        self.assertIn("JSON object", malformed.stderr)
        self.assertNotIn("Traceback", malformed.stderr)

    def test_candidate_selection_and_observation_changes_reject(self):
        f = self.fixture(ready=True)
        f.setup()
        f.write("installer/src/main.rs", "candidate\n")
        source = f.commit("candidate")
        root, identity, record = f.evidence(source)
        pointer = root / "selection" / f"{identity}.json"
        selected = json.loads(pointer.read_text())
        selected["candidate_digest"] = "f" * 64
        pointer.write_text(json.dumps(selected))
        different = f.git("push", "origin", "main", check=False)
        self.assertNotEqual(different.returncode, 0)
        self.assertIn("candidate", different.stderr)
        f.save_record(root, identity, record)
        observation = record["lanes"][0]["cases"][0]["observation"]["path"]
        (root / observation).write_text("Changed fixture observation.\n")
        changed = f.git("push", "origin", "main", check=False)
        self.assertNotEqual(changed.returncode, 0)
        self.assertIn("observation digest", changed.stderr)

    def test_annotated_tag_and_new_worktree_use_actual_targets(self):
        f = self.fixture()
        f.setup()
        f.git("tag", "-a", "docs-tag", "-m", "fixture tag")
        self.assertEqual(f.git("push", "origin", "docs-tag", check=False).returncode, 0)
        linked = self.root / "new-linked"
        f.git("worktree", "add", "-b", "new-linked", str(linked))
        (linked / "README.md").write_text("New linked worktree docs.\n")
        f.git("add", "README.md", cwd=linked)
        f.git("commit", "--quiet", "-m", "new linked docs", cwd=linked)
        pushed = f.git("push", "origin", "new-linked", cwd=linked, check=False)
        self.assertEqual(pushed.returncode, 0, pushed.stderr)


if __name__ == "__main__":
    unittest.main()
