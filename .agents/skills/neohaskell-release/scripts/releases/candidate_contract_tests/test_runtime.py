"""Cross-language PARSE evidence. These tests never call the source binder."""
import copy
import hashlib
import json
from pathlib import Path
import shutil
import unittest

import candidate


from candidate_contract_tests.generate_fixtures import CandidateFixtureCorpus, temporary_corpus_directory
CASES = json.loads((Path(__file__).parent / "runtime_cases.json").read_bytes())


def apply_case(directory, case):
    changed_tables = set()
    for edit in case["edits"]:
        path = directory / edit["file"]
        operation = edit["op"]
        raw = path.read_bytes()
        if operation == "starter-number":
            raw = b'{"unused_signed_zero":' + edit["text"].encode() + b',' + raw[1:]
            path.write_bytes(raw)
            core_path = directory / "candidate.json"
            core = json.loads(core_path.read_bytes())
            identity = hashlib.sha256(raw).hexdigest()
            core["build_inputs"]["starter"]["lock_sha256"] = identity
            core["auxiliary_assets"][path.name].update(sha256=identity, size=len(raw))
            receipts_path = directory / "build-receipts.json"
            receipts = json.loads(receipts_path.read_bytes())
            expected = candidate._expected_output_receipts(core)
            for row in receipts["receipts"]:
                row["build_inputs_sha256"] = candidate._canonical_digest(core["build_inputs"])
                expected[row["role"], row["platform"]][0]["build_receipt_sha256"] = candidate._canonical_digest(row)
            receipts_path.write_bytes(candidate.canonical_json(receipts))
            core_path.write_bytes(candidate.canonical_json(core))
            changed_tables.add("build-receipts.json")
        elif operation == "byteflip":
            body = bytearray(raw)
            body[edit["offset"]] = edit["byte"]
            path.write_bytes(body)
        elif operation == "symlink":
            outside = directory.parent / ("outside-" + path.name)
            outside.write_bytes(raw)
            path.unlink()
            path.symlink_to(outside)
        elif operation in {"suffix", "replace-prefix"}:
            encoded = edit["text"].encode()
            path.write_bytes(raw + encoded if operation == "suffix" else encoded + raw[1:])
        else:
            value = json.loads(raw)
            parts = edit["pointer"].split("/")[1:]
            target = value
            for part in parts[:-1]:
                target = target[int(part)] if isinstance(target, list) else target[part]
            last = parts[-1]
            if isinstance(target, list):
                if operation == "remove":
                    del target[int(last)]
                elif last == "-":
                    target.append(copy.deepcopy(edit["value"]))
                else:
                    target[int(last)] = copy.deepcopy(edit["value"])
            elif operation == "remove":
                del target[last]
            else:
                target[last] = copy.deepcopy(edit["value"])
            path.write_bytes(json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).encode())
            if edit["file"] in {"closures.json", "build-receipts.json"}:
                changed_tables.add(edit["file"])
    if changed_tables:
        core_path = directory / "candidate.json"
        core = json.loads(core_path.read_bytes())
        for name in changed_tables:
            raw = (directory / name).read_bytes()
            role = "closures" if name == "closures.json" else "build_receipts"
            core[role]["sha256"] = hashlib.sha256(raw).hexdigest()
            core[role]["size"] = len(raw)
        core_path.write_bytes(candidate.canonical_json(core))


def metadata_only(directory):
    registry = candidate.contract_registry_projection()["registry"]
    limits = registry["limits"]
    raw = (directory / "candidate.json").read_bytes()
    core = candidate._loads(raw, limits["max_core_bytes"], limits["max_core_depth"], limits["max_json_nodes"], True)
    if candidate.canonical_json(core) != raw:
        raise ValueError("Noncanonical core")
    checked = candidate._validate_core_shape(core, registry)
    metadata = {}
    for identity, maximum, kind in checked["identities"]:
        if kind in {"metadata", "bootstrap"}:
            metadata[identity["name"]] = candidate._read_regular_file(directory, identity["name"], identity["size"], identity["sha256"], maximum, True)
    candidate._starter_fields(core, metadata, directory, registry)
    tables = []
    for role in ("closures", "build_receipts"):
        body = metadata[core[role]["name"]]
        table = candidate._loads(body, limits["max_table_bytes"], limits["max_core_depth"], limits["max_json_nodes"], True)
        if candidate.canonical_json(table) != body:
            raise ValueError("Noncanonical table")
        tables.append(table)
    candidate._validate_closure_table(tables[0], core, registry, checked["output_roots"])
    candidate._validate_receipts_table(tables[1], core, registry)
    for entry in core["bootstraps"].values():
        if hashlib.sha256(raw).hexdigest().encode() in metadata[entry["file"]["name"]]:
            raise ValueError("Self-embedded candidate identity")


class RuntimeContract(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.corpus_temp = temporary_corpus_directory()
        cls.addClassCleanup(cls.corpus_temp.cleanup)
        cls.corpus = CandidateFixtureCorpus(candidate.release.ROOT, Path(cls.corpus_temp.name) / "corpus")

    def test_generation_refuses_owned_or_protected_outputs(self):
        with temporary_corpus_directory() as temporary:
            root = Path(temporary).resolve()
            occupied = root / "occupied"
            occupied.mkdir()
            sentinel = occupied / "sentinel"
            sentinel.write_bytes(b"preserve existing evidence")
            link = root / "linked"
            link.symlink_to(occupied, target_is_directory=True)
            protected = candidate.release.ROOT
            for destination in (occupied, link / "corpus", link / ".." / "corpus", protected / ".agents/corpus", protected / "neo/lifecycle/contract/corpus"):
                with self.subTest(destination=destination), self.assertRaises(ValueError):
                    CandidateFixtureCorpus(candidate.release.ROOT, destination)
            self.assertEqual(sentinel.read_bytes(), b"preserve existing evidence")
            self.assertEqual(set(occupied.iterdir()), {sentinel})

    def test_selected_source_rejects_escaping_recipe_paths(self):
        with temporary_corpus_directory() as temporary:
            root = Path(temporary)
            selected = root / "selected-source"
            shutil.copytree(self.corpus.root / "source", selected)
            config_path = selected / candidate.CONFIG_RELATIVE_PATH
            config = json.loads(config_path.read_bytes())
            config["groups"]["platform"]["packages"]["../../escaped.cabal"] = "escaped"
            config_path.write_bytes(candidate.canonical_json(config))
            with self.assertRaises(ValueError):
                CandidateFixtureCorpus(selected, root / "corpus")
            self.assertFalse((root / "escaped.cabal").exists())

    def test_release_version_bump_updates_the_only_parse_fixture(self):
        with temporary_corpus_directory() as temporary:
            root = Path(temporary)
            selected = root / "next-release"
            shutil.copytree(self.corpus.root / "source", selected)
            profile = json.loads((selected / candidate.CONFIG_RELATIVE_PATH).read_bytes())["groups"]["platform"]
            updated_version = candidate.release.bump(self.corpus.version, "compatible")
            for path, name in {**profile["packages"], **profile["locks"]}.items():
                manifest = selected / path
                manifest.write_text(candidate.release.change_version(manifest.read_text(), path, name, updated_version))
            after = CandidateFixtureCorpus(selected, root / "corpus")
            self.assertEqual({path.name for path in after.root.iterdir()}, {"current", "fixture-set.json", "runtime_cases.json", "source"})
            directory = after.root / "current"
            parsed = candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)
            index = json.loads((after.root / "fixture-set.json").read_bytes())
            self.assertEqual(parsed.version, updated_version)
            self.assertEqual(index["version"], updated_version)
            self.assertEqual(index["candidate_ids"], {"current": parsed.candidate_id})

    def test_runtime_registry_matches_release_contract(self):
        registry_path = candidate.release.ROOT / "neo/lifecycle/contract/registry.json"
        self.assertEqual(registry_path.read_bytes(), candidate.contract_registry_bytes())

    def test_current_candidate_parses(self):
        fixtures = json.loads((self.corpus.root / "fixture-set.json").read_bytes())
        directory = self.corpus.root / "current"
        parsed = candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)
        self.assertEqual(fixtures["candidate_ids"], {"current": parsed.candidate_id})
        metadata_only(directory)

    def test_shared_canonical_vectors(self):
        limits = candidate.contract_registry_projection()["registry"]["limits"]
        for vector in CASES["canonical_vectors"]:
            with self.subTest(raw=vector["raw"]):
                raw = vector["raw"].encode()
                try:
                    value = candidate._loads(raw, limits["max_core_bytes"], limits["max_core_depth"], limits["max_json_nodes"], True)
                    accepted = candidate.canonical_json(value) == raw
                except ValueError:
                    accepted = False
                self.assertEqual(accepted, vector["valid"])

    def test_shared_integer_vectors(self):
        limits = candidate.contract_registry_projection()["registry"]["limits"]
        for vector in CASES["integer_vectors"]:
            with self.subTest(raw=vector["raw"]):
                try:
                    candidate._loads(vector["raw"].encode(), limits["max_core_bytes"], limits["max_core_depth"], limits["max_json_nodes"], True)
                    accepted = True
                except ValueError:
                    accepted = False
                self.assertEqual(accepted, vector["valid"])

    def test_shared_negative_cases(self):
        for case in CASES["cases"]:
            with self.subTest(name=case["name"]), temporary_corpus_directory() as temporary:
                directory = Path(temporary) / "candidate"
                shutil.copytree(self.corpus.root / "current", directory)
                apply_case(directory, case)
                for key, operation in [("metadata_valid", lambda: metadata_only(directory)), ("materialized_valid", lambda: candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory))]:
                    try:
                        operation()
                        accepted = True
                    except ValueError:
                        accepted = False
                    self.assertEqual(accepted, case[key], key)
