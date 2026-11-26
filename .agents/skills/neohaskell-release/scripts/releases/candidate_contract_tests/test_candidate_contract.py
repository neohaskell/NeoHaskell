import base64
import copy
import hashlib
import json
from pathlib import Path
import shutil
import sys
import unittest


RELEASES = Path(__file__).resolve().parents[1]
if str(RELEASES) not in sys.path:
    sys.path.insert(0, str(RELEASES))
import candidate
from candidate_contract_tests.generate_fixtures import (
    CandidateFixtureCorpus, _commit_source, _git, temporary_corpus_directory, write_seeded_candidate,
)
from candidate_contract_tests.test_release_binding import ReleaseBindingCases
ROOT = candidate.release.ROOT


def _hash(raw):
    return hashlib.sha256(raw).hexdigest()


def _identity(name, raw):
    return {"name": name, "sha256": _hash(raw), "size": len(raw)}


def _commit_fixture_source(root, corpus):
    shutil.copytree(corpus.root / "source", root)
    return _commit_source(root)


def _refresh_receipt_links(core, table):
    for receipt in table["receipts"]:
        digest = candidate._canonical_digest(receipt)
        role, platform = receipt["role"], receipt["platform"]
        if role == "native-neo":
            core["native_neo"][platform]["build_receipt_sha256"] = digest
        elif role == "native-installer":
            core["native_installers"][platform]["build_receipt_sha256"] = digest
        else:
            output = role[len("nix-"):]
            core["packages"][platform][output]["build_receipt_sha256"] = digest


def _refresh_receipt_context(core, table):
    context = core["context"]
    for receipt in table["receipts"]:
        receipt.update({
            "context_sha256": candidate._canonical_digest(context),
            "source_tree_sha256": context["source_tree_sha256"],
            "build_tree_sha256": context["build_tree_sha256"],
            "build_inputs_sha256": candidate._canonical_digest(core["build_inputs"]),
            "recipe_set_sha256": core["build_inputs"]["recipe_set_sha256"],
            "producer_sha256": core["build_inputs"]["recipe_set_sha256"],
            "version": core["version"],
        })
    _refresh_receipt_links(core, table)


def _write_candidate(directory, core, closure_bytes=None, receipt_table=None):
    if closure_bytes is not None:
        (directory / "closures.json").write_bytes(closure_bytes)
        core["closures"] = _identity("closures.json", closure_bytes)
    if receipt_table is not None:
        receipt_bytes = candidate.canonical_json(receipt_table)
        _refresh_receipt_links(core, receipt_table)
        (directory / "build-receipts.json").write_bytes(receipt_bytes)
        core["build_receipts"] = _identity("build-receipts.json", receipt_bytes)
    core_bytes = candidate.canonical_json(core)
    (directory / "candidate.json").write_bytes(core_bytes)
    return core_bytes


class CandidateContract(ReleaseBindingCases, unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.corpus_temp = temporary_corpus_directory()
        cls.addClassCleanup(cls.corpus_temp.cleanup)
        cls.corpus = CandidateFixtureCorpus(ROOT, Path(cls.corpus_temp.name) / "corpus")
        cls.fixture_set = json.loads((cls.corpus.root / "fixture-set.json").read_text())
        cls.seeded_version = candidate.release.bump(cls.corpus.version, "compatible")
        cls.seeded_directory = Path(cls.corpus_temp.name) / "seeded"
        write_seeded_candidate(cls.corpus, cls.seeded_directory, cls.seeded_version)
        cls.source_temp = temporary_corpus_directory(prefix="candidate-contract-source-")
        cls.source_root = Path(cls.source_temp.name) / "repo"
        cls.addClassCleanup(cls.source_temp.cleanup)
        revision = _commit_fixture_source(cls.source_root, cls.corpus)
        cls.source_revision = revision
        cls.source_tree, cls.source_index = candidate._git_tree(
            cls.source_root, revision, candidate._registry()[1]["limits"],
        )
        cls.source_tree_sha256 = _hash(cls.source_tree)
        if revision != cls.fixture_set["source_revision"]:
            raise AssertionError("fixture source revision changed")
        if cls.source_tree_sha256 != cls.fixture_set["source_tree_sha256"]:
            raise AssertionError("fixture source tree changed")

    def fixture_dir(self):
        return self.corpus.root / "current"

    def parse_fixture(self, directory=None):
        directory = directory or self.fixture_dir()
        return candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)

    def copy_artifacts(self, temporary, source=None):
        directory = Path(temporary) / "candidate"
        shutil.copytree(source or self.fixture_dir(), directory)
        return directory

    def parse_mutated_core(self, core):
        return candidate.parse_candidate(candidate.canonical_json(core), self.fixture_dir())

    def test_full_parse_only_fixture_binds_the_accepted_closure_shape(self):
        current = self.parse_fixture()
        self.assertEqual(current.version, self.fixture_set["version"])
        self.assertEqual(current.context_kind, "engineering")
        self.assertEqual(self.fixture_set["candidate_ids"], {"current": current.candidate_id})
        closure = json.loads((self.fixture_dir() / "closures.json").read_bytes())
        self.assertEqual(len(closure["paths"]), 76)
        self.assertEqual(max(len(item["references"]) for item in closure["paths"]), 65)
        self.assertEqual(sum(item["nar_size"] for item in closure["paths"]), 5044109824)
        self.assertTrue(self.fixture_set["parse_only"])
        self.assertTrue(self.fixture_set["synthetic_metadata_only"])

    def test_malformed_role_maps_and_signature_values_fail_at_the_boundary(self):
        original = self.parse_fixture().core
        core = copy.deepcopy(original)
        core["native_neo"] = []
        with self.assertRaisesRegex(ValueError, "native Neo roles must be an object"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        core["auxiliary_assets"] = []
        with self.assertRaisesRegex(ValueError, "auxiliary assets must be an object"):
            self.parse_mutated_core(core)
        table = json.loads((self.fixture_dir() / "closures.json").read_bytes())
        table["paths"][0]["locations"][0]["signatures"] = [{}]
        with temporary_corpus_directory(prefix="candidate-contract-signature-shape-") as temporary:
            directory = self.copy_artifacts(temporary)
            core = copy.deepcopy(original)
            core_bytes = _write_candidate(directory, core, candidate.canonical_json(table))
            with self.assertRaisesRegex(ValueError, "signature metadata must contain strings"):
                candidate.parse_candidate(core_bytes, directory)

    def test_source_binding_rederives_committed_and_seeded_build_trees(self):
        committed = candidate.bind_source(self.parse_fixture(), self.source_root)
        seeded = candidate.bind_source(self.parse_fixture(self.seeded_directory), self.source_root)
        self.assertEqual(committed.context_kind, "engineering")
        self.assertEqual(committed.source_tree_sha256, committed.build_tree_sha256)
        self.assertEqual(seeded.source_tree_sha256, self.source_tree_sha256)
        self.assertNotEqual(seeded.source_tree_sha256, seeded.build_tree_sha256)
        self.assertEqual((committed.version, seeded.version), (self.corpus.version, self.seeded_version))

    def test_selection_binds_ordered_source_bound_candidates(self):
        current = candidate.bind_source(self.parse_fixture(self.seeded_directory), self.source_root)
        previous = candidate.bind_source(self.parse_fixture(), self.source_root)
        raw = candidate.canonical_json({
            "schema_version": 1,
            "current_candidate_id": current.candidate_id,
            "previous_candidate_id": previous.candidate_id,
        })
        selection = candidate.parse_verification_selection(raw, current, previous)
        self.assertEqual(selection.current_version, self.seeded_version)
        self.assertEqual(selection.previous_version, self.corpus.version)
        self.assertEqual(selection.selection_id, _hash(raw))
        reversed_raw = candidate.canonical_json({
            "schema_version": 1,
            "current_candidate_id": previous.candidate_id,
            "previous_candidate_id": current.candidate_id,
        })
        with self.assertRaisesRegex(ValueError, "ordered candidates"):
            candidate.parse_verification_selection(reversed_raw, current, previous)
        forged = candidate.SourceBoundCandidate(
            current.candidate_id, current.change_source, current.build_revision,
            current.source_tree_sha256, current.build_tree_sha256, current.version,
            current.context_kind, current.artifact_directory,
        )
        with self.assertRaisesRegex(ValueError, "source-bound candidates"):
            candidate.parse_verification_selection(raw, forged, previous)

    def test_registry_digest_and_cache_key_follow_authoritative_current_sources(self):
        registry = candidate.contract_registry_projection()["registry"]
        keys = {item["key"] for item in registry["trusted_cache_keys"]}
        self.assertIn("hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=", keys)
        self.assertNotIn("hydra.iohk.org:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=", keys)
        flake = (ROOT / "flake.nix").read_bytes()
        self.assertIn(b"hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=", flake)
        self.assertNotIn(b"hydra.iohk.org:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=", flake)
        self.assertEqual(
            self.parse_fixture().core["contract_registry_sha256"],
            candidate.contract_registry_sha256(),
        )

    def test_native_neo_roles_are_derived_from_the_existing_policy_owner(self):
        core = self.parse_fixture().core
        resolved = candidate.contract_registry_projection()["resolved"]
        self.assertEqual(set(core["native_neo"]), set(resolved["native_neo"]["targets"]))
        self.assertEqual(
            set(core["auxiliary_assets"]), set(resolved["auxiliary_assets"]),
        )
        self.assertEqual(len(core["native_neo"]) + len(core["native_installers"]) + 9, 14)

    def test_every_one_of_fourteen_output_roles_is_required(self):
        core = self.parse_fixture().core
        registry = candidate._registry()[1]
        cases = []
        cases.extend(("native_neo", target) for target in core["native_neo"])
        cases.extend(("native_installers", target) for target in core["native_installers"])
        cases.extend(("packages", system, role) for system, roles in core["packages"].items() for role in roles)
        self.assertEqual(len(cases), 14)
        for path in cases:
            with self.subTest(path=path):
                missing = copy.deepcopy(core)
                if path[0] == "packages":
                    del missing[path[0]][path[1]][path[2]]
                else:
                    del missing[path[0]][path[1]]
                with self.assertRaises(ValueError):
                    self.parse_mutated_core(missing)

    def test_platform_bootstrap_and_auxiliary_inventories_are_closed(self):
        original = self.parse_fixture().core
        target = next(iter(original["native_installers"]))
        core = copy.deepcopy(original)
        del core["native_installers"][target]
        with self.assertRaisesRegex(ValueError, "installer roles"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        core["native_installers"]["unknown-platform"] = core["native_installers"][target]
        with self.assertRaisesRegex(ValueError, "installer roles"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        del core["bootstraps"]["posix"]
        with self.assertRaisesRegex(ValueError, "bootstrap roles"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        core["auxiliary_assets"]["unexpected.json"] = next(iter(core["auxiliary_assets"].values()))
        with self.assertRaisesRegex(ValueError, "Auxiliary"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        del core["packages"]["x86_64-linux"]
        with self.assertRaisesRegex(ValueError, "Nix platforms"):
            self.parse_mutated_core(core)

    def test_pair_members_and_output_path_forms_are_exact(self):
        original = self.parse_fixture().core
        core = copy.deepcopy(original)
        core["packages"]["x86_64-linux"]["pair"]["members"]["neo"] = core["packages"]["x86_64-linux"]["neoctl"]["store_path"]
        with self.assertRaisesRegex(ValueError, "pair members"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        path = core["packages"]["x86_64-linux"]["neo"]["store_path"]
        core["packages"]["x86_64-linux"]["neo"]["store_path"] = path.rsplit("-", 1)[0] + "-neo.drv"
        with self.assertRaisesRegex(ValueError, "derivation"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        core["packages"]["x86_64-linux"]["neo"]["store_path"] = 'builtins.getFlake(".")#neo'
        with self.assertRaisesRegex(ValueError, "realized store-path"):
            self.parse_mutated_core(core)
        core = copy.deepcopy(original)
        core["packages"]["x86_64-linux"]["neo"]["nar_hash"] = "sha256-not-base64"
        with self.assertRaisesRegex(ValueError, "SRI"):
            self.parse_mutated_core(core)

    def test_closure_rejects_missing_references_and_unreachable_paths(self):
        fixture = self.fixture_dir()
        original_core = self.parse_fixture().core
        table = json.loads((fixture / "closures.json").read_bytes())
        missing_reference = copy.deepcopy(table)
        first = next(path for path in missing_reference["paths"] if path["references"])
        first["references"][0] = "/nix/store/" + "z" * 32 + "-missing-reference"
        with temporary_corpus_directory(prefix="candidate-contract-missing-ref-") as temporary:
            directory = self.copy_artifacts(temporary)
            core = copy.deepcopy(original_core)
            core_bytes = _write_candidate(directory, core, candidate.canonical_json(missing_reference))
            with self.assertRaisesRegex(ValueError, "missing reference"):
                candidate.parse_candidate(core_bytes, directory)
        unreachable = copy.deepcopy(table)
        missing_path = "/nix/store/" + "y" * 32 + "-unreachable-extra"
        unreachable["paths"].append({
            "store_path": missing_path,
            "nar_hash": "sha256-" + base64.b64encode(hashlib.sha256(b"unreachable").digest()).decode("ascii"),
            "nar_size": 1,
            "references": [],
            "locations": [{
                "cache": "https://neohaskell.cachix.org",
                "signatures": ["neohaskell.cachix.org-1:" + base64.b64encode(b"\0" * 64).decode("ascii")],
            }],
        })
        unreachable["paths"].sort(key=lambda item: item["store_path"])
        with temporary_corpus_directory(prefix="candidate-contract-unreachable-") as temporary:
            directory = self.copy_artifacts(temporary)
            core = copy.deepcopy(original_core)
            core_bytes = _write_candidate(directory, core, candidate.canonical_json(unreachable))
            with self.assertRaisesRegex(ValueError, "unreachable extra"):
                candidate.parse_candidate(core_bytes, directory)

    def test_materialized_artifacts_are_hashed_again_from_regular_files(self):
        core = self.parse_fixture().core
        name = next(iter(core["native_installers"].values()))["file"]["name"]
        with temporary_corpus_directory(prefix="candidate-contract-byte-flip-") as temporary:
            directory = self.copy_artifacts(temporary)
            path = directory / name
            content = path.read_bytes()
            path.write_bytes(bytes([content[0] ^ 1]) + content[1:])
            with self.assertRaisesRegex(ValueError, "bytes differ"):
                candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)
        with temporary_corpus_directory(prefix="candidate-contract-file-size-") as temporary:
            directory = self.copy_artifacts(temporary)
            core = copy.deepcopy(core)
            core["native_installers"][next(iter(core["native_installers"]))]["file"]["size"] += 1
            core_bytes = _write_candidate(directory, core)
            with self.assertRaisesRegex(ValueError, "size differs"):
                candidate.parse_candidate(core_bytes, directory)
        with temporary_corpus_directory(prefix="candidate-contract-symlink-") as temporary:
            directory = self.copy_artifacts(temporary)
            outside = Path(temporary) / "outside"
            outside.write_bytes(b"outside candidate directory")
            target = directory / name
            target.unlink()
            target.symlink_to(outside)
            with self.assertRaisesRegex(ValueError, "not a regular file"):
                candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)
        with temporary_corpus_directory(prefix="candidate-contract-extra-file-") as temporary:
            directory = self.copy_artifacts(temporary)
            (directory / "unlisted.txt").write_bytes(b"extra")
            with self.assertRaisesRegex(ValueError, "unlisted files"):
                candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)

    def test_source_tree_and_generated_diff_mutations_fail_source_binding(self):
        for mutation in ("source", "generated-diff"):
            with self.subTest(mutation=mutation), temporary_corpus_directory(prefix="candidate-contract-source-mutation-") as temporary:
                directory = self.copy_artifacts(temporary, self.seeded_directory)
                core = json.loads((directory / "candidate.json").read_bytes())
                receipts = json.loads((directory / "build-receipts.json").read_bytes())
                if mutation == "source":
                    core["context"]["source_tree_sha256"] = "0" * 64
                else:
                    core["context"]["version_seed"]["generated_diff_sha256"] = "0" * 64
                _refresh_receipt_context(core, receipts)
                core_bytes = _write_candidate(directory, core, receipt_table=receipts)
                parsed = candidate.parse_candidate(core_bytes, directory)
                expected = "source tree identity differs" if mutation == "source" else "exact generated diff differs"
                with self.assertRaisesRegex(ValueError, expected):
                    candidate.bind_source(parsed, self.source_root)

    def test_build_receipt_cannot_forge_a_different_source_tree(self):
        with temporary_corpus_directory(prefix="candidate-contract-receipt-") as temporary:
            directory = self.copy_artifacts(temporary)
            core = json.loads((directory / "candidate.json").read_bytes())
            receipts = json.loads((directory / "build-receipts.json").read_bytes())
            receipts["receipts"][0]["source_tree_sha256"] = "0" * 64
            core_bytes = _write_candidate(directory, core, receipt_table=receipts)
            with self.assertRaisesRegex(ValueError, "receipt source, recipe, context, or version"):
                candidate.parse_candidate(core_bytes, directory)

    def test_engineering_identity_cannot_be_retagged_as_a_release(self):
        core = self.parse_fixture().core
        core["context"]["kind"] = "release"
        core["context"]["build_revision"] = "b" * 40
        core["context"].pop("version_seed")
        core["context"]["plan_id"] = "c" * 64
        core["context"]["seed_receipt_sha256"] = "d" * 64
        with temporary_corpus_directory(prefix="candidate-contract-release-retag-") as temporary:
            directory = self.copy_artifacts(temporary)
            receipts = json.loads((directory / "build-receipts.json").read_bytes())
            _refresh_receipt_context(core, receipts)
            core_bytes = _write_candidate(directory, core, receipt_table=receipts)
            with self.assertRaisesRegex(ValueError, "starter pin differs"):
                candidate.parse_candidate(core_bytes, directory)

    def test_missing_bootstrap_generator_source_fails_closed(self):
        with temporary_corpus_directory(prefix="candidate-contract-missing-generator-") as temporary:
            root = Path(temporary) / "repo"
            shutil.copytree(self.corpus.root / "source", root)
            generator = root / "neo/lifecycle/scripts/generate_bootstraps.py"
            generator.unlink()
            revision = _commit_source(root)
            tree, _index = candidate._git_tree(root, revision, candidate._registry()[1]["limits"])
            with temporary_corpus_directory(prefix="candidate-contract-missing-generator-artifacts-") as artifact_tmp:
                directory = self.copy_artifacts(artifact_tmp)
                core = json.loads((directory / "candidate.json").read_bytes())
                receipts = json.loads((directory / "build-receipts.json").read_bytes())
                core["context"].update({
                    "change_source": revision,
                    "build_revision": revision,
                    "source_tree_sha256": _hash(tree),
                    "build_tree_sha256": _hash(tree),
                })
                _refresh_receipt_context(core, receipts)
                core_bytes = _write_candidate(directory, core, receipt_table=receipts)
                parsed = candidate.parse_candidate(core_bytes, directory)
                with self.assertRaisesRegex(ValueError, "Missing committed source input.*generate_bootstraps.py"):
                    candidate.bind_source(parsed, root)

    def test_candidate_json_encoding_rejects_duplicate_keys_whitespace_and_booleans(self):
        directory = self.fixture_dir()
        with self.assertRaisesRegex(ValueError, "Duplicate JSON object key"):
            candidate.parse_candidate(b'{"schema_version":2,"schema_version":2}', directory)
        canonical = (directory / "candidate.json").read_bytes()
        with self.assertRaisesRegex(ValueError, "not canonical"):
            candidate.parse_candidate(canonical + b"\n", directory)
        core = self.parse_fixture().core
        core["schema_version"] = True
        with self.assertRaisesRegex(ValueError, "Boolean values"):
            candidate.parse_candidate(candidate.canonical_json(core), directory)
        with self.assertRaisesRegex(ValueError, "Floating point"):
            candidate.canonical_json({"number": 1.25})


if __name__ == "__main__":
    unittest.main()
