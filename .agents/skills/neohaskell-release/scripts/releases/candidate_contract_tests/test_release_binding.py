import copy
import importlib.machinery
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
from unittest.mock import patch

import candidate
from candidate_contract_tests.generate_fixtures import temporary_corpus_directory


def commit(root, message):
    from .test_candidate_contract import _git
    _git(root, 'add', '-A')
    _git(root, '-c', 'user.name=Candidate Fixture', '-c',
         'user.email=candidate@example.invalid', '-c', 'commit.gpgSign=false',
         'commit', '-qm', message)
    return _git(root, 'rev-parse', 'HEAD').decode().strip()


def source_identity(root, revision, core):
    tree, index = candidate._git_tree(root, revision, candidate._registry()[1]['limits'])
    core['context'].update(change_source=revision, source_tree_sha256=candidate._sha256(tree))
    core['build_inputs']['recipe_set_sha256'] = candidate._source_inputs(
        root, index, candidate._registry()[1]['source_inputs']['recipe'])[0]
    return tree, index


def write_bound_input(directory, core):
    from .test_candidate_contract import _refresh_receipt_context, _write_candidate
    table = json.loads((directory / 'build-receipts.json').read_bytes())
    _refresh_receipt_context(core, table)
    return candidate.parse_candidate(_write_candidate(directory, core, receipt_table=table), directory)


def maintained_seed(root, plan, revision, output):
    loader = importlib.machinery.SourceFileLoader(
        'candidate_test_semantic_release', str(candidate.SKILL_ROOT / 'scripts/semantic-release'))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    owner = importlib.util.module_from_spec(spec)
    loader.exec_module(owner)
    real_run = subprocess.run
    calls = []

    def synthetic_nix_and_compat(args, **kwargs):
        if args == ['nix', 'flake', 'lock', '--update-input', 'neohaskell']:
            path = Path(kwargs['cwd']) / 'flake.lock'
            lock = json.loads(path.read_bytes())
            node = lock['nodes']['neohaskell']
            node['locked'].update(rev=revision, narHash='sha256-' + 'A' * 43 + '=',
                                  lastModified=1800000000, revCount=400)
            node['original']['rev'] = revision
            path.write_text(json.dumps(lock, indent=2) + '\n')
            calls.append('synthetic-nix-lock')
            return subprocess.CompletedProcess(args, 0)
        if args[0] == str(candidate.SKILL_ROOT / 'scripts/neo-release'):
            calls.append('compat-not-executed')
            return subprocess.CompletedProcess(args, 0)
        if args[0] != 'git':
            raise AssertionError('Unexpected external command ' + repr(args))
        return real_run(args, **kwargs)

    with patch.object(owner, 'STATE', output.parent / 'seed-state'), patch.object(
            owner.subprocess, 'run', side_effect=synthetic_nix_and_compat):
        owner.seed(candidate.release.Repo(root), plan, revision, output)
    if calls != ['synthetic-nix-lock', 'compat-not-executed']:
        raise AssertionError('Maintained seed did not exercise the expected boundaries')
    return owner


def release_fixture(temporary, corpus, mutation=None):
    from .test_candidate_contract import _commit_fixture_source, _git, _identity
    root = Path(temporary) / 'repo'
    _commit_fixture_source(root, corpus)
    (root / candidate.CANDIDATE_RELATIVE_PATH).write_bytes(
        Path(candidate.__file__).read_bytes())
    ledger = root / candidate.release.LEDGER_PATH
    ledger.write_text(candidate.release.canonical({'schema': 1, 'attempts': [], 'recoveries': []}))
    (root / 'CHANGELOG.md').write_text('# History\n')
    commit(root, 'ci: fixture release baseline')
    _git(root, 'tag', 'neo-v0.17.1')
    note = root / '.changes/fix.md'
    note.parent.mkdir()
    note.write_text('---\ngroup: platform\ncomponent: Framework\nimpact: compatible\ncategory: Fixed\n---\n\n## Summary\n\nSaving an item now reports completion once, even after reconnecting.\n')
    if mutation is not None:
        mutation(root)
    source = commit(root, 'fix: completion')
    repo = candidate.release.Repo(root)
    plan = candidate.release.plan_release(repo, source, 'platform', {}, '2026-11-16',
                                          ('neo-v0.17.1', '0.17.2'))
    plan = candidate.release.identify({**{k: v for k, v in plan.items() if k != 'id'},
        'distribution_policy': 'installer-v1',
        'contract_registry_sha256': candidate.contract_registry_sha256()})
    candidate.release.write_edits(root, candidate.release.expected_edits(repo, plan))
    revision = commit(root, 'ci: generated preparation')
    if candidate.release.verify_generated(repo, revision, plan) is not True:
        raise AssertionError('Maintained preparation did not verify')
    directory = Path(temporary) / 'candidate'
    shutil.copytree(corpus.root / 'current-0.17.2', directory)
    core = json.loads((directory / 'candidate.json').read_bytes())
    source_identity(root, source, core)
    core['context'] = {k: v for k, v in core['context'].items() if k != 'version_seed'}
    core['context'].update(kind='release', build_revision=revision, plan_id=plan['id'])
    seed_dir = Path(temporary) / 'seed'
    owner = maintained_seed(root, plan, revision, seed_dir)
    owner.apply_seed(repo, plan, revision, seed_dir)
    for name in ('flake.nix', 'flake.lock', 'cabal.project'):
        artifact = 'neo-starter-' + name
        raw = (seed_dir / artifact).read_bytes()
        (directory / artifact).write_bytes(raw)
        core['auxiliary_assets'][artifact] = _identity(artifact, raw)
        if raw != (root / 'neo/starter' / name).read_bytes():
            raise AssertionError('Maintained apply_seed differs from exported seed')
    refresh_seed(root, directory, core)
    return root, directory, core, plan


def refresh_seed(root, directory, core):
    from .test_candidate_contract import _identity
    starter = core['build_inputs']['starter']
    revision = core['context']['build_revision']
    overlays = {}
    for name, field in [('flake.nix', 'flake_sha256'), ('flake.lock', 'lock_sha256'),
                        ('cabal.project', 'cabal_project_sha256')]:
        artifact = 'neo-starter-' + name
        raw = (directory / artifact).read_bytes()
        overlays['neo/starter/' + name] = raw
        core['auxiliary_assets'][artifact] = _identity(artifact, raw)
        starter[field] = candidate._sha256(raw)
    starter.update(framework_revision=revision, framework_nar_hash=json.loads(
        overlays['neo/starter/flake.lock'])['nodes']['neohaskell']['locked']['narHash'])
    core['context']['seed_receipt_sha256'] = candidate._canonical_digest({
        'schema_version': 1, 'plan_id': core['context']['plan_id'],
        'build_revision': revision, **starter})
    tree, index = candidate._git_tree(root, revision, candidate._registry()[1]['limits'])
    core['context']['build_tree_sha256'] = candidate._sha256(candidate._overlay_tree_bytes(tree, overlays))
    core['build_inputs']['cargo_lock_sha256'] = candidate._sha256(
        candidate._git_blob(root, index, 'neo/Cargo.lock')[1])


class ReleaseBindingCases:
    def test_release_binding_accepts_maintained_preparation_and_synthetic_nix_seed(self):
        with temporary_corpus_directory(prefix='candidate-release-binding-') as temporary:
            root, directory, core, plan = release_fixture(temporary, self.corpus)
            repo = candidate.release.Repo(root)
            self.assertNotIn(plan, repo.ledger(core['context']['change_source'])['attempts'])
            self.assertIn(plan, repo.ledger(core['context']['build_revision'])['attempts'])
            bound = candidate.bind_source(write_bound_input(directory, core), root)
            self.assertEqual(bound.context_kind, 'release')
            self.assertEqual(bound.version, '0.17.2')
            self.assertNotEqual(bound.change_source, bound.build_revision)
            tree, index = candidate._git_tree(root, bound.build_revision, candidate._registry()[1]['limits'])
            self.assertNotEqual(bound.build_tree_sha256, candidate._sha256(tree))
            self.assertNotEqual(candidate._git_blob(root, index, 'neo/starter/flake.nix')[1],
                                (directory / 'neo-starter-flake.nix').read_bytes())

    def test_release_seed_rejects_unrelated_edits_and_changed_source_identity(self):
        with temporary_corpus_directory(prefix='candidate-release-seed-negative-') as temporary:
            root, directory, original, plan = release_fixture(temporary, self.corpus)
            originals = {p.name: p.read_bytes() for p in directory.glob('neo-starter-*')}
            mutations = ['flake-extra', 'project-extra', 'dependency', 'extra-node',
                         'lock-root', 'lock-url', 'lock-original-url', 'lock-type',
                         'lock-extra-field', 'input-extra-field']
            for mutation in mutations:
                with self.subTest(mutation=mutation):
                    for name, raw in originals.items():
                        (directory / name).write_bytes(raw)
                    core = copy.deepcopy(original)
                    if mutation in ('flake-extra', 'project-extra'):
                        name = 'flake.nix' if mutation == 'flake-extra' else 'cabal.project'
                        path = directory / ('neo-starter-' + name)
                        path.write_bytes(path.read_bytes() + b'\n# unexpected generated edit\n')
                    else:
                        path = directory / 'neo-starter-flake.lock'
                        lock = json.loads(path.read_bytes())
                        node = lock['nodes']['neohaskell']
                        if mutation == 'dependency':
                            lock['nodes']['root']['inputs']['unrelated'] = 'neohaskell'
                        elif mutation == 'extra-node':
                            lock['nodes']['unexpected'] = copy.deepcopy(node)
                        elif mutation == 'lock-root':
                            lock['root'] = 'neohaskell'
                        elif mutation == 'lock-url':
                            node['locked']['url'] = 'https://example.invalid/other.git'
                        elif mutation == 'lock-original-url':
                            node['original']['url'] = 'https://example.invalid/other.git'
                        elif mutation == 'lock-type':
                            node['locked']['type'] = 'path'
                        elif mutation == 'lock-extra-field':
                            node['locked']['unexpected'] = 'value'
                        else:
                            node['inputs'] = {'unexpected': 'root'}
                        path.write_text(json.dumps(lock))
                    refresh_seed(root, directory, core)
                    with self.assertRaisesRegex(ValueError, 'Release starter'):
                        candidate.bind_source(write_bound_input(directory, core), root)

    def test_release_binding_rejects_extra_preparation_file_and_stale_seed_identity(self):
        with temporary_corpus_directory(prefix='candidate-release-diff-negative-') as temporary:
            root, directory, core, plan = release_fixture(temporary, self.corpus)
            bad_plan = copy.deepcopy(core)
            bad_plan['context']['plan_id'] = '0' * 64
            with self.assertRaisesRegex(ValueError, 'Release plan identity is absent'):
                candidate.bind_source(write_bound_input(directory, bad_plan), root)
            bad_tree = copy.deepcopy(core)
            bad_tree['context']['build_tree_sha256'] = '0' * 64
            with self.assertRaisesRegex(ValueError, 'Release build tree identity'):
                candidate.bind_source(write_bound_input(directory, bad_tree), root)
            bad_seed = copy.deepcopy(core)
            bad_seed['context']['seed_receipt_sha256'] = '0' * 64
            with self.assertRaisesRegex(ValueError, 'Release seed identity'):
                candidate.bind_source(write_bound_input(directory, bad_seed), root)
            from .test_candidate_contract import _git
            _git(root, 'reset', '--hard', core['context']['build_revision'])
            (root / 'unrelated.txt').write_text('extra preparation file\n')
            _git(root, 'add', 'unrelated.txt')
            _git(root, '-c', 'user.name=Candidate Fixture', '-c',
                 'user.email=candidate@example.invalid', '-c', 'commit.gpgSign=false',
                 'commit', '--amend', '--no-edit', '-q')
            core['context']['build_revision'] = _git(root, 'rev-parse', 'HEAD').decode().strip()
            old = core['build_inputs']['starter']['framework_revision']
            for path in directory.glob('neo-starter-*'):
                path.write_bytes(path.read_bytes().replace(old.encode(), core['context']['build_revision'].encode()))
            refresh_seed(root, directory, core)
            with self.assertRaisesRegex(ValueError, 'Release preparation does not match'):
                candidate.bind_source(write_bound_input(directory, core), root)

    def test_selection_accepts_distinct_immutable_historical_owner_bytes(self):
        from .test_candidate_contract import _commit_fixture_source
        with temporary_corpus_directory(prefix='candidate-historical-selection-') as temporary:
            previous_root = Path(temporary) / 'previous-source'
            _commit_fixture_source(previous_root, self.corpus)
            previous = candidate.bind_source(self.parse_fixture('previous-0.17.1'), previous_root)

            def historical_owners(root):
                engine = root / candidate.ENGINE_RELATIVE_PATH
                engine.write_bytes(engine.read_bytes() + b'\n')
                registry = root / candidate.CONTRACT_RELATIVE_PATH
                registry.write_text(json.dumps(json.loads(registry.read_bytes()), indent=4) + '\n')

            release_tmp = Path(temporary) / 'release'
            release_tmp.mkdir()
            root, directory, core, plan = release_fixture(release_tmp, self.corpus, historical_owners)
            current = candidate.bind_source(write_bound_input(directory, core), root)
            self.assertNotEqual((previous_root / candidate.ENGINE_RELATIVE_PATH).read_bytes(),
                                (root / candidate.ENGINE_RELATIVE_PATH).read_bytes())
            self.assertNotEqual(previous.change_source, current.change_source)
            raw = candidate.canonical_json({'schema_version': 1,
                'current_candidate_id': current.candidate_id, 'previous_candidate_id': previous.candidate_id})
            selection = candidate.parse_verification_selection(raw, current, previous)
            self.assertEqual((selection.current_version, selection.previous_version), ('0.17.2', '0.17.1'))
            with self.assertRaisesRegex(ValueError, 'ordered candidates'):
                candidate.parse_verification_selection(raw, previous, current)

    def test_historical_source_rejects_unknown_registry_and_configuration_semantics(self):
        from .test_candidate_contract import _commit_fixture_source
        for mutation in ('registry', 'config', 'policy'):
            with self.subTest(mutation=mutation), temporary_corpus_directory(prefix='candidate-history-negative-') as temporary:
                root = Path(temporary) / 'repo'
                _commit_fixture_source(root, self.corpus)
                relative = candidate.CONTRACT_RELATIVE_PATH if mutation == 'registry' else candidate.CONFIG_RELATIVE_PATH
                path = root / relative
                value = json.loads(path.read_bytes())
                if mutation == 'registry':
                    value['limits']['max_core_bytes'] += 1
                elif mutation == 'config':
                    value['groups']['platform']['asset_prefix'] = 'other-'
                else:
                    value['native_policy'] = 'unknown-native-policy'
                path.write_text(json.dumps(value))
                revision = commit(root, 'ci: incompatible source semantics')
                directory = self.copy_artifacts(temporary, 'previous-0.17.1')
                core = json.loads((directory / 'candidate.json').read_bytes())
                tree, index = source_identity(root, revision, core)
                core['context'].update(build_revision=revision, build_tree_sha256=candidate._sha256(tree))
                expected = {
                    'registry': 'parser limit|contract registry',
                    'config': 'Committed release configuration',
                    'policy': 'supported native-v2 policy',
                }[mutation]
                with self.assertRaisesRegex(ValueError, expected):
                    candidate.bind_source(write_bound_input(directory, core), root)
