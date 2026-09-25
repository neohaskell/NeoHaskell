#!/usr/bin/env python3
"""Seven controlled cache cases in an explicitly disposable extraction checkout.

Usage: pilot-mutations.py DISPOSABLE_CHECKOUT OUTPUT_DIRECTORY
Requires the foundation + consumer pilot patch and a real local PostgreSQL fixture
for the last case. Creates throwaway commits and returns to the starting commit.
Never point this at a working branch: detached HEAD and clean tracked files are
required. No shared Nix store is deleted. All commands/logs and output paths are
retained. This is correctness evidence, not an isolated performance comparison.
"""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

COMPONENTS = {
    'foundation': 'nhfoundation:lib:nhfoundation',
    'foundation-test': 'nhfoundation:test:nhfoundation-test',
    'core': 'nhcore:lib:nhcore',
    'core-test': 'nhcore:test:nhcore-test-core',
    'auth-test': 'nhcore:test:nhcore-test-auth',
    'service-test': 'nhcore:test:nhcore-test-service',
    'integration-test': 'nhcore:test:nhcore-test-integration',
    'integrations': 'nhintegrations:lib:nhintegrations',
    'integrations-test': 'nhintegrations:test:nhintegrations-test',
    'testbed': 'nhtestbed:exe:nhtestbed',
    'consumer': 'nhcore-consumer:exe:nhcore-consumer',
}


def run(args, cwd, **kwargs):
    return subprocess.run(args, cwd=cwd, check=True, **kwargs)


def text(args, cwd):
    return subprocess.check_output(args, cwd=cwd, text=True).strip()


def capture(args, cwd, folder, name, expected=0, env=None):
    started = time.time()
    with (folder/(name+'.log')).open('w') as log:
        result = subprocess.run(args, cwd=cwd, env=env, text=True,
                                stdout=log, stderr=subprocess.STDOUT)
    record = {'command': args, 'started_unix_s': started, 'elapsed_s': time.time()-started,
              'exit_code': result.returncode, 'expected_exit_code': expected}
    (folder/(name+'.json')).write_text(json.dumps(record, indent=2)+'\n')
    if result.returncode != expected:
        raise ValueError(f'{name}: got exit {result.returncode}, expected {expected}; see {folder}')


def state(repo, system, folder):
    expr = 'p: {' + ''.join(f' "{label}" = {{ drv = p."{attr}".drvPath; out = toString p."{attr}"; }};'
                            for label, attr in COMPONENTS.items()) + '}'
    command = ['nix','eval','--accept-flake-config','--json',f'.#packages.{system}','--apply',expr]
    with (folder/'evaluation.log').open('w') as log:
        values = json.loads(subprocess.check_output(command, cwd=repo, text=True, stderr=log))
    (folder/'paths.json').write_text(json.dumps(values, indent=2)+'\n')
    return values


def commit(repo, message):
    run(['git','add','--all'], repo)
    run(['git','-c','user.name=Cache mutation','-c','user.email=cache-mutation@neohaskell.org',
         'commit','-m',message],repo,stdout=subprocess.DEVNULL)


def mutate(repo, case):
    files = {
        'markdown': 'README.md',
        'test-only': 'foundation/test/Main.hs',
        'implementation': 'foundation/core/Text.hs',
        'sibling': 'core/core/Int.hs',
        'flag': 'foundation/nhfoundation.cabal',
        'runtime-fixture': 'testbed/tests/integrations/openapi.hurl',
    }
    path = repo/files[case]
    before = path.read_text()
    if case == 'implementation':
        assert before.count('isEmpty = Data.Text.null\n') == 1
        after = before.replace('isEmpty = Data.Text.null\n','isEmpty text = text |> Data.Text.null\n')
    elif case == 'flag':
        assert before.count('\nlibrary\n  import: common_cfg\n') == 1
        after = before.replace('\nlibrary\n  import: common_cfg\n',
                               '\nlibrary\n  import: common_cfg\n  ghc-options: -O0\n')
    elif case == 'runtime-fixture':
        after = before + '\nGET http://localhost:8080/cache-fixture-must-fail\nHTTP 200\n'
    elif case == 'markdown':
        after = before + '\nCache-isolation disposable fixture.\n'
    else:
        after = before + '\n-- Cache-isolation disposable source mutation.\n'
    path.write_text(after)
    return {'path': files[case], 'before_sha256': hashlib.sha256(before.encode()).hexdigest(),
            'after_sha256': hashlib.sha256(after.encode()).hexdigest()}


def main():
    repo, output = map(lambda x: Path(x).resolve(), sys.argv[1:])
    if subprocess.run(['git','symbolic-ref','-q','HEAD'], cwd=repo, stdout=subprocess.DEVNULL).returncode == 0:
        raise ValueError('pilot requires detached HEAD in a disposable checkout')
    if text(['git','status','--porcelain'], repo):
        raise ValueError('pilot checkout must be clean, including untracked files')
    output.mkdir(parents=True, exist_ok=False)
    baseline = text(['git','rev-parse','HEAD'],repo)
    system = text(['nix','eval','--impure','--raw','--expr','builtins.currentSystem'],repo)
    initial = output/'seed';initial.mkdir()
    base = state(repo, system, initial)
    targets = ['nix','build','--accept-flake-config','-L','--no-link',
               *['.#'+attr for attr in COMPONENTS.values()],'.#ci-components']
    capture(targets,repo,initial,'build')
    results=[]
    try:
        for case in ['no-change','markdown','test-only','implementation','sibling','flag','runtime-fixture']:
            folder=output/case;folder.mkdir()
            mutation = None
            if case != 'no-change':
                mutation=mutate(repo,case)
                commit(repo,'test: disposable cache case '+case)
                (folder/'mutation.patch').write_text(text(['git','show','--format=fuller','HEAD'],repo)+'\n')
            revision=text(['git','rev-parse','HEAD'],repo)
            current=state(repo,system,folder)
            changed=[name for name in base if current[name] != base[name]]
            expected = {
                'no-change': [], 'markdown': [], 'test-only': ['foundation-test'],
                'implementation': list(COMPONENTS),
                'sibling': [name for name in COMPONENTS if name not in ['foundation','foundation-test']],
                'flag': list(COMPONENTS), 'runtime-fixture': [],
            }[case]
            if set(changed) != set(expected):
                raise ValueError(f'{case}: changed {changed}; expected {expected}')
            capture(targets,repo,folder,'build')
            result={'case':case,'revision':revision,'changed':changed,'expected':expected,
                    'mutation':mutation,'build_exit_code':0}
            if case == 'runtime-fixture':
                bundle=text(['nix','eval','--accept-flake-config','--raw','.#ci-components.outPath'],repo)
                manifest=json.loads((Path(bundle)/'manifest.json').read_text())
                env={**os.environ,'NHTESTBED_BINARY':current['testbed']['out']+'/bin/nhtestbed',
                     'PATH':manifest['runtime']+'/bin:'+os.environ['PATH']}
                capture(['bash','testbed/scripts/run-tests.sh'],repo,folder,'fixture-execution',expected=1,env=env)
                log=(folder/'fixture-execution.log').read_text()
                if 'cache-fixture-must-fail' not in log:
                    raise ValueError('runtime failure did not execute the mutated fixture')
                result['fixture_expected_failure_verified']=True
            results.append(result)
            (output/'results.json').write_text(json.dumps({'baseline':baseline,'system':system,'cases':results},indent=2)+'\n')
            run(['git','checkout','--detach',baseline],repo,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
    finally:
        # Mutations were committed; returning to baseline never discards edits.
        if not text(['git','status','--porcelain','--untracked-files=no'],repo):
            run(['git','checkout','--detach',baseline],repo,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)


if __name__=='__main__':
    main()
