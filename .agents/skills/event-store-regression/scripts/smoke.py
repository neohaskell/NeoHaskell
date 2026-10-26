#!/usr/bin/env python3
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
SKILL = Path(__file__).resolve().parents[1]


def catalog():
    return json.loads((SKILL / 'references/recipes.json').read_text())


def check(root, data):
    root = root.resolve()
    errors = []
    if not (root / data['helper']).is_file():
        errors.append('Maintained regression helper is missing')
    names = set()
    for recipe in data['recipes']:
        name = recipe['name']
        if name in names:
            errors.append(f'Duplicate recipe {name}')
        names.add(name)
        path = (root / recipe['source']).resolve()
        if not path.is_relative_to(root) or not path.is_file():
            errors.append(f'Missing recipe source {name}')
            continue
        source = path.read_text()
        if f'it "{recipe["test"]}"' not in source:
            errors.append(f'Missing exact example {name}')
        if f'Regression.knownBad "{name}" backend' not in source:
            errors.append(f'Missing controlled-fault fixture {name}')
        if recipe['examples'] < 1 or recipe['assertion'] != f'REGRESSION_ASSERT:{name}':
            errors.append(f'Invalid recipe expectation {name}')
    if not data['recipes']:
        errors.append('Recipe collection is empty')
    if errors:
        raise ValueError('; '.join(errors))


def load_evidence(root):
    path = root / '.agents/skills/regression-evidence/scripts/evidence.py'
    spec = importlib.util.spec_from_file_location('event_store_evidence', path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def smoke(root, selected, timeout):
    root = root.resolve()
    data = catalog()
    check(root, data)
    recipes = [row for row in data['recipes'] if not selected or row['name'] in selected]
    if selected and set(selected) != {row['name'] for row in recipes}:
        raise ValueError('Unknown recipe selection')
    evidence = load_evidence(root)
    names = {data['helper'], *(row['source'] for row in recipes)}
    before = {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in names}
    parent = root / '.agents-state/event-store-regression'
    parent.mkdir(parents=True, exist_ok=True)
    output = Path(tempfile.mkdtemp(prefix='run-', dir=parent)).resolve()
    receipt = {'schema': 1, 'status': 'incomplete', 'source_hashes': before, 'recipes': []}
    try:
        for recipe in recipes:
            row = {'name': recipe['name'], 'backend': recipe['backend'], 'fault': recipe['fault'], 'runs': []}
            receipt['recipes'].append(row)
            command = [str(root / '.agents/skills/tdd/scripts/test-match'), recipe['test'], recipe['suite']]
            for mode in ('good', 'bad'):
                environment = dict(os.environ, NEOHASKELL_ROOT=str(root))
                environment.pop('NEOHASKELL_REGRESSION_BAD', None)
                environment.pop('NEOHASKELL_REGRESSION_BACKEND', None)
                if mode == 'bad':
                    environment['NEOHASKELL_REGRESSION_BAD'] = recipe['name']
                    environment['NEOHASKELL_REGRESSION_BACKEND'] = recipe['backend']
                filename = recipe['name'] + '-' + mode + '.log'
                result = evidence.run(command, root, output / filename, timeout, environment)
                observed = evidence.classify_runtime(
                    result.returncode,
                    result.text,
                    'pass' if mode == 'good' else 'fail',
                    recipe['test'],
                    recipe['assertion'],
                    runner_outcome=result.runner_outcome,
                    process_outcome=result.process_outcome,
                    requires_runner_receipt=result.requires_runner_receipt,
                )
                if observed['selected_examples'] != recipe['examples']:
                    raise ValueError(f'Example selection changed for {recipe["name"]}')
                row['runs'].append({
                    'mode': mode,
                    'argv': result.command,
                    'exit': result.returncode,
                    'process_outcome': result.process_outcome,
                    'runner_outcome': result.runner_outcome,
                    'runner_receipt': str(result.runner_receipt_path.relative_to(output)) if result.runner_receipt_path else None,
                    'runner_receipt_sha256': result.runner_receipt_sha256,
                    'log': filename,
                    'log_sha256': hashlib.sha256((output / filename).read_bytes()).hexdigest(),
                    'observed': observed,
                })
        after = {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in names}
        if after != before:
            raise ValueError('Selected regression sources changed during execution')
        receipt['status'] = 'observed'
    except (ValueError, OSError) as error:
        receipt['error'] = str(error)
        raise
    finally:
        (output / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
        print(output / 'receipt.json')
    return receipt


def main():
    parser = argparse.ArgumentParser(description='Check existing event-store recipes or execute known-good and controlled-bad sensitivity runs.')
    parser.add_argument('action', choices=['list', 'check', 'run'])
    parser.add_argument('--repo', type=Path)
    parser.add_argument('--recipe', action='append')
    parser.add_argument('--timeout', type=float, default=180)
    args = parser.parse_args()
    root = args.repo.resolve() if args.repo else Path(subprocess.check_output(['git', '-C', str(SKILL), 'rev-parse', '--show-toplevel'], text=True).strip())
    try:
        if not 0 < args.timeout < float('inf'):
            raise ValueError('timeout must be finite and positive')
        if args.action == 'list':
            print(json.dumps(catalog()['recipes'], indent=2))
        elif args.action == 'check':
            check(root, catalog())
            print('Recipe locations checked. Runtime sensitivity has not been executed.')
        else:
            smoke(root, args.recipe, args.timeout)
    except (ValueError, OSError) as error:
        print(f'event-store-regression: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
