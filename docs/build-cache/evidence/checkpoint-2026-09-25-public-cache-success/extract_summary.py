import json
import re
from datetime import datetime
from pathlib import Path

base = Path('/tmp/nh-public-cache-success-36170358310')
run = json.loads((base / 'run.json').read_text())
full_log = (base / 'full.log').read_text()


def parse_time(value):
    return datetime.fromisoformat(value.replace('Z', '+00:00'))


def duration(step):
    return round((parse_time(step['completedAt']) - parse_time(step['startedAt'])).total_seconds(), 3)


def find_job(prefix):
    return next(job for job in run['jobs'] if job['name'] == prefix)


def find_step(job, name):
    return next(step for step in job['steps'] if step['name'] == name)


def hspec_report(platform):
    report = (base / f'verification-{platform}/component-report.log').read_text()
    summaries = re.findall(r'(?m)^\d+ examples, \d+ failures, \d+ pending$', report)
    literal = summaries[-1]
    examples, failures, pending = map(int, re.match(r'(\d+) examples, (\d+) failures, (\d+) pending', literal).groups())
    return {'literal': literal, 'examples': examples, 'failures': failures, 'pending': pending}


def cache_copy_lines(platform):
    job_name = f'verify-published ({platform}-latest)'
    step_name = 'Fetch signed closure from Cachix and upstream caches with all builders disabled'
    project_markers = (
        'neohaskell-ci-components', 'neohaskell-components.json', 'nhcore-lib-nhcore',
        'nhintegrations-lib-nhintegrations', 'nhcore-test-nhcore-test-core',
        'nhcore-test-nhcore-test-service', 'nhtestbed-exe-nhtestbed',
    )
    project = []
    upstream_pg = []
    command_lines = []
    for line in full_log.splitlines():
        fields = line.split('\t', 2)
        if len(fields) != 3:
            continue
        job, step, log_entry = fields
        if job != job_name:
            continue
        timestamp_match = re.match(r'([^ ]+Z) (.*)', log_entry)
        if timestamp_match is None:
            continue
        timestamp, message = timestamp_match.groups()
        clean_message = re.sub(r'\x1b\[[0-9;]*m', '', message)
        if clean_message.startswith('./dev nix-components'):
            command_lines.append(clean_message)
        if step != step_name:
            continue
        match = re.match(r"copying path '([^']+)' from '([^']+)'\.\.\.", message)
        if not match:
            continue
        path, source = match.groups()
        item = {'timestamp': timestamp, 'path': path, 'source': source}
        if source == 'https://neohaskell.cachix.org' and any(marker in path for marker in project_markers):
            project.append(item)
        if source == 'https://cache.nixos.org' and (path.endswith('-pg_config') or path.endswith('-pg_config.env')):
            upstream_pg.append(item)
    return project, upstream_pg, command_lines


platforms = {}
for platform in ('ubuntu', 'macos'):
    publisher = find_job(f'cachix-push ({platform}-latest)')
    consumer = find_job(f'verify-published ({platform}-latest)')
    publisher_build = find_step(publisher, 'Build to populate cache')
    consumer_install = find_step(consumer, 'Run DeterminateSystems/determinate-nix-action@v3.22.2')
    consumer_fetch = find_step(consumer, 'Fetch signed closure from Cachix and upstream caches with all builders disabled')
    consumer_test = find_step(consumer, 'Execute the fetched core test binary')
    producer_dir = base / f'producer-{platform}'
    consumer_dir = base / f'verification-{platform}' / 'component-artifact'
    producer_root = (producer_dir / 'root.txt').read_text().strip()
    consumer_root = (consumer_dir / 'root.txt').read_text().strip()
    closure_paths = len(json.loads((producer_dir / 'closure.json').read_text()))
    producer_metadata = json.loads((producer_dir / 'producer.json').read_text())
    project, upstream_pg, workflow_commands = cache_copy_lines(platform)
    platforms[platform] = {
        'jobs': {
            'publisher': {'name': publisher['name'], 'id': publisher['databaseId'], 'status': publisher['status'], 'conclusion': publisher['conclusion']},
            'consumer': {'name': consumer['name'], 'id': consumer['databaseId'], 'status': consumer['status'], 'conclusion': consumer['conclusion']},
        },
        'producer': {
            'root': producer_root,
            'closure_paths': closure_paths,
            'revision': producer_metadata['revision'],
            'derivation': producer_metadata['derivation'],
        },
        'consumer': {
            'root': consumer_root,
            'root_matches_producer': consumer_root == producer_root,
            'fetch_mode': '--from-public-caches',
            'core_hspec': hspec_report(platform),
            'workflow_commands': workflow_commands,
        },
        'cache_copy_evidence': {
            'project_outputs_from_cachix': project,
            'pg_config_from_cache_nixos': upstream_pg,
        },
        'timings_from_job_metadata': {
            'publisher_build': {
                'step': publisher_build['name'], 'startedAt': publisher_build['startedAt'],
                'completedAt': publisher_build['completedAt'], 'duration_s': duration(publisher_build),
            },
            'consumer_nix_install': {
                'step': consumer_install['name'], 'startedAt': consumer_install['startedAt'],
                'completedAt': consumer_install['completedAt'], 'duration_s': duration(consumer_install),
            },
            'consumer_fetch': {
                'step': consumer_fetch['name'], 'startedAt': consumer_fetch['startedAt'],
                'completedAt': consumer_fetch['completedAt'], 'duration_s': duration(consumer_fetch),
            },
            'consumer_core_test': {
                'step': consumer_test['name'], 'startedAt': consumer_test['startedAt'],
                'completedAt': consumer_test['completedAt'], 'duration_s': duration(consumer_test),
            },
        },
    }

consumer_log_lines = []
for line in full_log.splitlines():
    fields = line.split('\t', 2)
    if len(fields) == 3 and fields[0].startswith('verify-published '):
        consumer_log_lines.append(fields[2])
no_consumer_build_or_eval = [
    line for line in consumer_log_lines
    if re.search(r'(^|\s)(nix (eval|build)|cabal (build|v2-build)|ghc( |$)|building derivation|will be built|build succeeded)', line, re.I)
]

summary = {
    'run': {
        'id': run['databaseId'], 'head_sha': run['headSha'], 'head_branch': run['headBranch'],
        'event': run['event'], 'workflow': run['workflowName'], 'status': run['status'],
        'conclusion': run['conclusion'], 'startedAt': run['startedAt'], 'updatedAt': run['updatedAt'],
        'url': run['url'],
    },
    'timing_context': 'single-run correctness workflow timing from job metadata; not a matched benchmark',
    'consumer_build_or_evaluation_log_matches': no_consumer_build_or_eval,
    'platforms': platforms,
    'evidence_files': {
        'run_metadata_jobs': str(base / 'run.json'),
        'full_log': str(base / 'full.log'),
        'commands_and_exit_codes': str(base / 'commands.txt'),
        'producer_ubuntu': str(base / 'producer-ubuntu'),
        'producer_macos': str(base / 'producer-macos'),
        'verification_ubuntu': str(base / 'verification-ubuntu'),
        'verification_macos': str(base / 'verification-macos'),
    },
}
(base / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')

with (base / 'evidence-lines.log').open('w') as evidence:
    for platform in ('ubuntu', 'macos'):
        evidence.write(f'[{platform}]\n')
        for category in ('project_outputs_from_cachix', 'pg_config_from_cache_nixos'):
            evidence.write(f'{category}:\n')
            for item in platforms[platform]['cache_copy_evidence'][category]:
                evidence.write(f"{item['timestamp']} copying path '{item['path']}' from '{item['source']}'...\n")
        evidence.write('workflow commands:\n')
        for command_line in platforms[platform]['consumer']['workflow_commands']:
            evidence.write(command_line + '\n')
        evidence.write('\n')
