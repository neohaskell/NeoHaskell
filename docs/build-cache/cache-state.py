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
import urllib.parse
from pathlib import Path

COMPONENTS = (
    'nhcore:lib:nhcore', 'nhintegrations:lib:nhintegrations', 'nhtestbed:lib:nhtestbed',
    'nhcore:test:nhcore-test-core', 'nhcore:test:nhcore-test-auth', 'nhcore:test:nhcore-test-integration',
    'nhcore:test:nhcore-test-service', 'nhintegrations:test:nhintegrations-test', 'nhtestbed:exe:nhtestbed',
)
# Flake additions are not included in `nix config show` outside evaluation.
# Keep these aligned with flake.nix nixConfig.extra-substituters.
CACHES = ('https://cache.iog.io', 'https://neohaskell.cachix.org')
PUBLIC_CACHES = frozenset((*CACHES, 'https://cache.nixos.org', 'https://install.determinate.systems'))
STORE = re.compile(r'^/nix/store/[a-z0-9]{32}-[^/\s]+$')
TIMEOUT_S = 5


def configured_caches(settings):
    values = settings['substituters']['value']
    if not isinstance(values, list) or not all(isinstance(value, str) for value in values):
        raise ValueError('unexpected substituters configuration format')
    digest = hashlib.sha256(json.dumps(values, sort_keys=True).encode()).hexdigest()
    caches = set(CACHES)
    unknown = 0
    for value in values:
        parsed = urllib.parse.urlsplit(value)
        normalized = value.rstrip('/')
        if normalized in PUBLIC_CACHES and not (parsed.username or parsed.password or parsed.query or parsed.fragment):
            caches.add(normalized)
        else:
            # Never persist arbitrary URLs, which may contain credentials.
            unknown += 1
    return sorted(caches), {'sha256': digest, 'unrecognized_count': unknown}


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
    # The harness may live in a separate checkout from the measured revision.
    root = Path(subprocess.check_output(['git', 'rev-parse', '--show-toplevel'], text=True).strip())
    revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip()
    record = {
        'schema': 1, 'status': 'unavailable', 'comparison_unusable': True,
        'started_unix_s': time.time(),
        'revision': revision, 'lock_sha256': hashlib.sha256((root / 'flake.lock').read_bytes()).hexdigest(),
        'system': system(), 'caches': list(CACHES), 'outputs': {}, 'errors': [],
    }
    try:
        settings = json.loads(subprocess.check_output(['nix', 'config', 'show', '--json'],
                             cwd=root, text=True, stderr=subprocess.DEVNULL))
        caches, config = configured_caches(settings)
        record['caches'] = caches
        record['substituter_config'] = config
        if config['unrecognized_count']:
            record['errors'].append('unrecognized substituters were not probed; URLs redacted')
    except (subprocess.CalledProcessError, OSError, KeyError, TypeError, ValueError):
        record['errors'].append('effective substituter configuration unavailable')
    eval_started = time.monotonic()
    command = ['nix', 'eval', '--accept-flake-config', '--json',
               f'.#packages.{record["system"]}', '--apply', expression()]
    result = subprocess.run(command, cwd=root, capture_output=True, text=True, check=False)
    eval_log = args.output.with_suffix('.evaluation.log')
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
        remote = {cache: narinfo(cache, output['out'], args.timeout) for cache in record['caches']}
        output['local_state'] = local['state']
        output['local'] = local
        output['remote'] = remote
        if local['state'] == 'unavailable':
            record['errors'].append(f'{name}: local state unavailable')
        for cache, state in remote.items():
            if state['state'] == 'network-error':
                record['errors'].append(f'{name}: {cache}: network error')
    record['probe_s'] = time.monotonic() - probe_started
    record['comparison_unusable'] = bool(record['errors']) or any(
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
