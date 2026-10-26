#!/usr/bin/env python3
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import statistics
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
BEST_PRACTICES_SCRIPTS = Path(__file__).resolve().parents[2] / 'neohaskell-best-practices' / 'scripts'
sys.path.insert(0, str(BEST_PRACTICES_SCRIPTS))
from bounded_process import ProcessOutcome, run_bounded

SUMMARY = re.compile(r'^(\d+) examples?, (\d+) failures?\s*$', re.MULTILINE)
MAX_OUTPUT_BYTES = 32 * 1024 * 1024


def number(value, name, minimum=0):
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value) or value < minimum:
        raise ValueError(f'{name} must be finite and at least {minimum}')
    return value


def command(value):
    if not isinstance(value, list) or not value or any(not isinstance(item, str) or '\0' in item for item in value):
        raise ValueError('Commands must be nonempty argument arrays')
    return value


def revision(cwd):
    result = subprocess.run(['git', '-C', str(cwd), 'rev-parse', '--verify', 'HEAD'], capture_output=True, text=True, timeout=5)
    return result.stdout.strip() if result.returncode == 0 else None


def inputs(side):
    root = Path(side['cwd']).resolve()
    result = {}
    for name in side['inputs']:
        path = (root / name).resolve()
        if not path.is_relative_to(root) or not path.is_file():
            raise ValueError(f'Invalid measured input {name}')
        result[name] = hashlib.sha256(path.read_bytes()).hexdigest()
    if not result:
        raise ValueError('Each side needs explicit measured inputs')
    return result


def run(argv, cwd, timeout, logfile):
    result = run_bounded(
        command(argv),
        cwd=Path(cwd),
        timeout=timeout,
        max_output_bytes=MAX_OUTPUT_BYTES,
    )
    logfile.write_bytes(result.output)
    if result.outcome is ProcessOutcome.TIMED_OUT:
        raise ValueError(f'Command timed out; evidence at {logfile}')
    if result.outcome is ProcessOutcome.OUTPUT_LIMITED:
        raise ValueError(f'Output exceeds the supported 32 MiB result size; inspect {logfile}')
    return result.returncode, result.elapsed_seconds, result.output.decode('utf-8', errors='replace')


def observation(protocol, code, elapsed, text):
    if code != 0:
        raise ValueError(f'Measurement command failed with exit {code}')
    kind = protocol['record']
    if kind == 'driver-json':
        rows = text.strip().splitlines()
        value = json.loads(rows[-1]) if rows else {}
        if value.get('metric') != protocol['metric'] or value.get('unit') != protocol['unit']:
            raise ValueError('Driver metric or unit differs from the protocol')
        number(value.get('value'), 'value')
        for key in ('completed', 'errors', 'rejected', 'retries'):
            count = value.get(key)
            if isinstance(count, bool) or not isinstance(count, int) or count < (1 if key == 'completed' else 0):
                raise ValueError(f'{key} must be a valid integer work count')
        if value['errors'] != 0 or value.get('outcome_verified') is not True:
            raise ValueError('Driver reported errors or an unverified outcome')
        return value
    if protocol['metric'] != 'elapsed' or protocol['unit'] != 's':
        raise ValueError('Command duration must be elapsed seconds')
    if kind == 'hspec':
        summaries = SUMMARY.findall(text)
        if len(summaries) != 1 or int(summaries[0][0]) < 1 or int(summaries[0][1]) != 0:
            raise ValueError('Expected one nonempty successful Hspec summary')
        return {'value': elapsed, 'completed': int(summaries[0][0]), 'errors': 0, 'rejected': 0, 'retries': 0}
    if kind == 'compiler':
        pattern = protocol.get('work_pattern')
        if pattern and not re.search(pattern, text, re.MULTILINE):
            raise ValueError('Expected compiler work was not observed')
        if not pattern and protocol.get('scenario') != 'unchanged':
            raise ValueError('Clean and incremental compiler samples require observed work')
        return {'value': elapsed, 'completed': 0 if not pattern else 1, 'errors': 0, 'rejected': 0, 'retries': 0}
    raise ValueError('Unknown observation contract')


def measure(protocol_path, root, output=None):
    protocol_path = protocol_path.resolve()
    protocol = json.loads(protocol_path.read_text())
    for name in ('claim', 'completion', 'settings', 'metric', 'unit'):
        if not protocol.get(name):
            raise ValueError(f'Protocol needs {name}')
    if protocol.get('kind') not in {'runtime', 'compile', 'suite'}:
        raise ValueError('kind must be runtime, compile or suite')
    record_for_kind = {'runtime': 'driver-json', 'compile': 'compiler', 'suite': 'hspec'}
    if protocol.get('record') != record_for_kind[protocol['kind']]:
        raise ValueError('Observation contract must match the measured kind')
    if protocol['kind'] == 'compile' and protocol.get('scenario') not in {'clean', 'incremental', 'unchanged'}:
        raise ValueError('Compilation needs a clean, incremental or unchanged scenario')
    samples = protocol.get('samples', 5)
    warmups = protocol.get('warmups', 1)
    if isinstance(samples, bool) or not isinstance(samples, int) or samples < 5:
        raise ValueError('Use at least five measured samples per side')
    if isinstance(warmups, bool) or not isinstance(warmups, int) or warmups < 0:
        raise ValueError('warmups must be a nonnegative integer')
    timeout = number(protocol.get('timeout_seconds', 600), 'timeout_seconds', 0.001)
    identities = {}
    revisions = {}
    for name in ('baseline', 'treatment'):
        side = protocol[name]
        side['cwd'] = str((protocol_path.parent / side['cwd']).resolve())
        command(side['argv'])
        inputs(side)
        revisions[name] = revision(side['cwd'])
    state = root / '.agents-state'
    state.mkdir(exist_ok=True)
    if output is None:
        parent = state / 'framework-benchmark'
        parent.mkdir(exist_ok=True)
        output = Path(tempfile.mkdtemp(prefix='run-', dir=parent))
    else:
        output = output.resolve()
        if not output.is_relative_to(state.resolve()):
            raise ValueError('Output must stay inside repository-root .agents-state')
        output.mkdir(parents=True, exist_ok=False)
    result = {'schema': 1, 'status': 'incomplete', 'protocol': protocol, 'input_hashes': identities, 'revisions': revisions, 'samples': []}
    (output / 'protocol.json').write_bytes(protocol_path.read_bytes())
    try:
        for index in range(samples + warmups):
            for name in ('baseline', 'treatment'):
                side = protocol[name]
                if revision(side['cwd']) != revisions[name]:
                    raise ValueError(f'{name} revision changed')
                prefix = f'{index:03d}-{name}'
                if side.get('prepare'):
                    code, _, _ = run(side['prepare'], side['cwd'], timeout, output / (prefix + '-prepare.log'))
                    if code:
                        raise ValueError(f'{name} preparation failed with exit {code}')
                current = inputs(side)
                if name not in identities:
                    identities[name] = current
                elif current != identities[name]:
                    raise ValueError(f'{name} measured inputs changed')
                code, elapsed, text = run(side['argv'], side['cwd'], timeout, output / (prefix + '.log'))
                observed = observation(protocol, code, elapsed, text)
                if inputs(side) != identities[name]:
                    raise ValueError(f'{name} measured inputs changed during execution')
                if revision(side['cwd']) != revisions[name]:
                    raise ValueError(f'{name} revision changed during execution')
                result['samples'].append({'side': name, 'warmup': index < warmups, 'argv': side['argv'], 'exit': code, 'command_elapsed_s': elapsed, 'observation': observed, 'log': prefix + '.log'})
        result['summary'] = {}
        for name in ('baseline', 'treatment'):
            values = [sample['observation']['value'] for sample in result['samples'] if sample['side'] == name and not sample['warmup']]
            result['summary'][name] = {'median': statistics.median(values), 'minimum': min(values), 'maximum': max(values), 'count': len(values)}
        result['status'] = 'recorded'
    except (ValueError, OSError, KeyError, json.JSONDecodeError) as error:
        result['error'] = str(error)
        raise
    finally:
        (output / 'results.json').write_text(json.dumps(result, indent=2) + '\n')
        print(output / 'results.json')
    return result


def main():
    parser = argparse.ArgumentParser(description='Record alternating matched benchmark samples and their evidence.')
    parser.add_argument('protocol', type=Path)
    parser.add_argument('--repo', type=Path)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    root = args.repo.resolve() if args.repo else Path(subprocess.check_output(['git', '-C', str(Path(__file__).parent), 'rev-parse', '--show-toplevel'], text=True).strip())
    try:
        measure(args.protocol, root, args.output)
    except (ValueError, OSError, KeyError, json.JSONDecodeError) as error:
        print(f'framework-benchmark: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
