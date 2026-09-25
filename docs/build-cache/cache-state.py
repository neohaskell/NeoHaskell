#!/usr/bin/env python3
"""Record exact component output and public-cache state before a Nix build."""
import argparse
import hashlib
import json
import platform
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
COMPONENTS = (
    'nhcore:lib:nhcore', 'nhintegrations:lib:nhintegrations', 'nhtestbed:lib:nhtestbed',
    'nhcore:test:nhcore-test-core', 'nhcore:test:nhcore-test-auth', 'nhcore:test:nhcore-test-integration',
    'nhcore:test:nhcore-test-service', 'nhintegrations:test:nhintegrations-test', 'nhtestbed:exe:nhtestbed',
)
# Keep this list aligned with flake.nix nixConfig.extra-substituters.
CACHES = ('https://cache.iog.io', 'https://neohaskell.cachix.org')
STORE = re.compile(r'^/nix/store/[a-z0-9]{32}-[^/\s]+$')
TIMEOUT_S = 5


def system():
    machine = {'arm64': 'aarch64', 'AMD64': 'x86_64'}.get(platform.machine(), platform.machine())
    return f'{machine}-{platform.system().lower()}'


def expression():
    names = ('ci-components',) + COMPONENTS
    fields = ''.join(f' "{name}" = {{ drv = p."{name}".drvPath; out = toString p."{name}"; }};' for name in names)
    return f'p: {{{fields}}}'


def local_state(path):
    try:
        result = subprocess.run(['nix', 'path-info', path], capture_output=True, text=True, check=False)
    except OSError as error:
        return {'state': 'unavailable', 'error': str(error)}
    if result.returncode == 0:
        return {'state': 'present'}
    detail = (result.stdout + result.stderr)[-2000:]
    if 'is not valid' in detail or "don't know how to build" in detail:
        return {'state': 'absent'}
    return {'state': 'unavailable', 'error': detail}


def narinfo(cache, path, timeout=TIMEOUT_S):
    digest = path.removeprefix('/nix/store/').split('-', 1)[0]
    url = f'{cache.rstrip("/")}/{digest}.narinfo'
    started = time.monotonic()
    try:
        request = urllib.request.Request(url, headers={'User-Agent': 'neohaskell-cache-state/1'})
        with urllib.request.urlopen(request, timeout=timeout) as response:
            code = int(response.getcode())
            body = response.read(64 * 1024).decode('utf-8', errors='replace')
    except urllib.error.HTTPError as error:
        state = 'absent' if error.code in (404, 410) else 'network-error'
        return {'state': state, 'http_status': error.code, 'url': url,
                'elapsed_s': time.monotonic() - started, 'error': str(error)}
    except (urllib.error.URLError, TimeoutError, OSError) as error:
        return {'state': 'network-error', 'url': url,
                'elapsed_s': time.monotonic() - started, 'error': str(error)}
    if code in (404, 410):
        return {'state': 'absent', 'http_status': code, 'url': url,
                'elapsed_s': time.monotonic() - started}
    if code != 200:
        return {'state': 'network-error', 'http_status': code, 'url': url,
                'elapsed_s': time.monotonic() - started, 'error': f'HTTP {code}'}
    fields = dict(line.split(': ', 1) for line in body.splitlines() if ': ' in line)
    if fields.get('StorePath') != path:
        return {'state': 'network-error', 'http_status': code, 'url': url,
                'elapsed_s': time.monotonic() - started, 'error': 'StorePath mismatch'}
    return {'state': 'present', 'http_status': code, 'url': url,
            'elapsed_s': time.monotonic() - started}


def save(path, record):
    path.write_text(json.dumps(record, indent=2) + '\n')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--timeout', type=float, default=TIMEOUT_S)
    args = parser.parse_args()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    record = {
        'schema': 1, 'status': 'unavailable', 'comparison_unusable': True,
        'started_unix_s': time.time(),
        'revision': revision, 'lock_sha256': hashlib.sha256((ROOT / 'flake.lock').read_bytes()).hexdigest(),
        'system': system(), 'caches': list(CACHES), 'outputs': {}, 'errors': [],
    }
    eval_started = time.monotonic()
    command = ['nix', 'eval', '--accept-flake-config', '--json',
               f'.#packages.{record["system"]}', '--apply', expression()]
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, check=False)
    eval_log = args.output.with_name('cache-state-evaluation.log')
    eval_log.write_text(result.stderr)
    record['evaluation'] = {
        'eval_s': time.monotonic() - eval_started, 'exit_code': result.returncode,
        'stderr_log': str(eval_log),
    }
    if result.returncode != 0:
        record['errors'].append(f'evaluation exited {result.returncode}')
        save(args.output, record)
        raise RuntimeError(f'Nix evaluation failed; see {eval_log}')
    values = json.loads(result.stdout)
    for name in ('ci-components',) + COMPONENTS:
        value = values[name]
        if not (STORE.fullmatch(value['drv']) and STORE.fullmatch(value['out'])):
            raise ValueError(f'invalid evaluated paths for {name}')
        record['outputs'][name] = {'drv': value['drv'], 'out': value['out']}
    probe_started = time.monotonic()
    for name, output in record['outputs'].items():
        local = local_state(output['out'])
        remote = {cache: narinfo(cache, output['out'], args.timeout) for cache in CACHES}
        output['local_state'] = local['state']
        output['local'] = local
        output['remote'] = remote
        if local['state'] == 'unavailable':
            record['errors'].append(f'{name}: local state unavailable')
        for cache, state in remote.items():
            if state['state'] == 'network-error':
                record['errors'].append(f'{name}: {cache}: network error')
    record['probe_s'] = time.monotonic() - probe_started
    record['comparison_unusable'] = any(
        output['local_state'] == 'unavailable' or
        any(state['state'] not in ('present', 'absent') for state in output['remote'].values())
        for output in record['outputs'].values())
    record['status'] = 'complete'
    record['elapsed_s'] = time.monotonic() - started
    save(args.output, record)
    return 0


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
        raise SystemExit(f'cache-state: invalid evaluated structure: {error}')
