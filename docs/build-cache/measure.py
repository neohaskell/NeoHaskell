#!/usr/bin/env python3
"""Record a command observation without conflating cache conditions or job time.

record OUTPUT --scenario NAME --stage NAME --local-state DESCRIPTION
              --remote-state DESCRIPTION -- COMMAND [ARG ...]
summarize OUTPUT [OUTPUT ...]

Run record from the checkout being measured. It refuses tracked modifications;
controlled mutations must be committed on disposable branches. Logs are retained
verbatim plus timestamped lines. Only successful, comparable groups with >=3
samples get a median. This does not infer cache hits or separate compile/link
from arbitrary command output: use separate stage commands and inspect logs.
"""
import argparse
from collections import defaultdict
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import time


def read_command(args):
    return subprocess.check_output(args, text=True).strip()


def summarize(paths):
    groups = defaultdict(list)
    if len({path.resolve() for path in paths}) != len(paths):
        raise ValueError('duplicate observation directory')
    for path in paths:
        entry = json.loads((path / 'observation.json').read_text())
        for filename, digest in entry['logs'].items():
            if hashlib.sha256((path / filename).read_bytes()).hexdigest() != digest:
                raise ValueError(f'log digest mismatch: {path / filename}')
        key = tuple(entry[k] for k in ('revision', 'system', 'scenario', 'stage',
                                       'local_state', 'remote_state', 'lock_sha256')) + (tuple(entry['command']),)
        groups[key].append(entry)
    results = []
    for key, entries in groups.items():
        successful = [e['elapsed_s'] for e in entries if e['exit_code'] == 0]
        results.append({'revision': key[0], 'system': key[1], 'scenario': key[2],
                        'stage': key[3], 'local_state': key[4], 'remote_state': key[5],
                        'command': key[7], 'observations_s': [e['elapsed_s'] for e in entries],
                        'exit_codes': [e['exit_code'] for e in entries], 'n': len(entries),
                        'median_s': statistics.median(successful) if len(successful) >= 3 and
                        len(successful) == len(entries) else None})
    return results


def record(args):
    if read_command(['git', 'status', '--porcelain', '--untracked-files=no']):
        raise ValueError('commit tracked changes before recording an observation')
    args.output.mkdir(parents=True, exist_ok=False)
    revision = read_command(['git', 'rev-parse', 'HEAD'])
    checkout = Path(read_command(['git', 'rev-parse', '--show-toplevel']))
    start_wall = time.time()
    start = time.monotonic()
    with (args.output/'command.log').open('wb') as log, (args.output/'lines.jsonl').open('w') as timed:
        process = subprocess.Popen(args.command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        with process.stdout:
            for line in process.stdout:
                log.write(line)
                log.flush()
                timed.write(json.dumps({'elapsed_s': time.monotonic()-start,
                                        'text': line.decode('utf-8', errors='replace')}) + '\n')
                timed.flush()
                sys.stdout.buffer.write(line)
                sys.stdout.buffer.flush()
        code = process.wait()
    entry = {'schema': 1, 'revision': revision, 'system': platform.machine() + '-' + platform.system(),
             'platform': platform.platform(), 'command': args.command,
             'scenario': args.scenario, 'stage': args.stage,
             'local_state': args.local_state, 'remote_state': args.remote_state,
             'started_unix_s': start_wall, 'elapsed_s': time.monotonic()-start,
             'exit_code': code, 'lock_sha256': hashlib.sha256((checkout/'flake.lock').read_bytes()).hexdigest(),
             'github_run_id': os.environ.get('GITHUB_RUN_ID'),
             'github_job': os.environ.get('GITHUB_JOB'),
             'logs': {name: hashlib.sha256((args.output/name).read_bytes()).hexdigest()
                      for name in ('command.log', 'lines.jsonl')}}
    (args.output/'observation.json').write_text(json.dumps(entry, indent=2)+'\n')
    return code


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    subs = parser.add_subparsers(dest='action', required=True)
    run = subs.add_parser('record')
    run.add_argument('output', type=Path)
    for field in ('scenario', 'stage', 'local-state', 'remote-state'):
        run.add_argument('--'+field, required=True)
    report = subs.add_parser('summarize')
    report.add_argument('paths', type=Path, nargs='+')
    before, separator, command = sys.argv[1:], False, []
    if '--' in before:
        index = before.index('--')
        before, command, separator = before[:index], before[index+1:], True
    args = parser.parse_args(before)
    if args.action == 'summarize':
        print(json.dumps(summarize(args.paths), indent=2))
        return 0
    if not separator or not command:
        parser.error('record needs -- COMMAND [ARGS...]')
    args.command = command
    return record(args)


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        print(f'measure: {error}', file=sys.stderr)
        sys.exit(1)
