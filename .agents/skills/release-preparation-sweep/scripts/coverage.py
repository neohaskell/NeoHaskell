from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

MANIFEST = '.agents/skills/neohaskell-release/references/manifest.json'
VERSION = re.compile(r'^## (\d+\.\d+\.\d+)\b', re.MULTILINE)


def git(checkout: Path, *args: str) -> str:
    result = subprocess.run(['git', '-C', str(checkout), *args], capture_output=True, text=True)
    if result.returncode:
        raise ValueError(f'git {args[0]} failed in {checkout}: {result.stderr.strip()}')
    return result.stdout.strip()


def source_fragment(checkout: Path, revision: str, path: str) -> str:
    result = subprocess.run(
        ['git', '-C', str(checkout), 'show', f'{revision}:{path}'],
        capture_output=True,
        text=True,
    )
    if result.returncode:
        raise ValueError(f'source fragment is unavailable: {revision}:{path}')
    return result.stdout


def digest(text: str) -> str:
    return hashlib.sha256(text.encode()).hexdigest()


def notes_by_path(attempt: dict) -> dict[str, dict]:
    notes = attempt['notes']
    paths = [note['path'] for note in notes]
    if len(paths) != len(set(paths)):
        raise ValueError('plan contains duplicate fragment paths')
    for note in notes:
        if digest(note['text']) != note['hash']:
            raise ValueError(f"fragment hash mismatch: {note['path']}")
    return {note['path']: note for note in notes}


def added_attempt(parent: dict, child: dict) -> dict:
    old = {attempt['id']: attempt for attempt in parent['attempts']}
    new = {attempt['id']: attempt for attempt in child['attempts']}
    added = [attempt for identity, attempt in new.items() if identity not in old]
    if len(added) != 1 or any(new.get(identity) != value for identity, value in old.items()):
        raise ValueError('manifest does not add exactly one attempt without altering earlier attempts')
    if parent['recoveries'] != child['recoveries']:
        raise ValueError('manifest recoveries changed')
    attempt = added[0]
    if attempt['kind'] != 'release' or attempt['group'] != 'platform' or attempt['bootstrap'] is not False:
        raise ValueError('added attempt is not an ordinary platform release')
    return attempt


def compare_notes(older: dict[str, dict], selected: dict[str, dict], source_text: dict[str, str]) -> tuple[list[str], list[dict]]:
    missing = sorted(set(older) - set(selected))
    revised = []
    for path in sorted(set(older) & set(selected)):
        old_note = older[path]
        selected_note = selected[path]
        if old_note['hash'] != selected_note['hash']:
            if source_text.get(path) != selected_note['text']:
                raise ValueError(f'candidate note does not match the current source fragment: {path}')
            revised.append({'path': path, 'older_hash': old_note['hash'], 'selected_hash': selected_note['hash']})
    return missing, revised


def main() -> int:
    parser = argparse.ArgumentParser(description='Check one cumulative generated platform release')
    parser.add_argument('--source', required=True, type=Path)
    parser.add_argument('--candidate', required=True, type=Path)
    parser.add_argument('--older', action='append', default=[], metavar='NUMBER=CHECKOUT')
    args = parser.parse_args()
    source = args.source.resolve()
    candidate = args.candidate.resolve()
    output = {'status': 'FAIL', 'source_sha': git(source, 'rev-parse', 'HEAD'), 'candidate_head': git(candidate, 'rev-parse', 'HEAD'), 'older': []}
    try:
        if git(candidate, 'rev-parse', 'HEAD^') != output['source_sha']:
            raise ValueError('candidate parent differs from trusted source')
        source_manifest = json.loads((source / MANIFEST).read_text())
        candidate_manifest = json.loads((candidate / MANIFEST).read_text())
        selected_attempt = added_attempt(source_manifest, candidate_manifest)
        if selected_attempt['source_sha'] != output['source_sha']:
            raise ValueError('selected attempt source differs from trusted source')
        selected_notes = notes_by_path(selected_attempt)
        source_text = {}
        for path, note in selected_notes.items():
            fragment = (source / path).read_text()
            if fragment != note['text']:
                raise ValueError(f'candidate note differs from current source fragment: {path}')
            source_text[path] = fragment
        source_versions = Counter(VERSION.findall((source / 'CHANGELOG.md').read_text()))
        candidate_versions = Counter(VERSION.findall((candidate / 'CHANGELOG.md').read_text()))
        added_versions = candidate_versions - source_versions
        removed_versions = source_versions - candidate_versions
        if removed_versions or added_versions != Counter({selected_attempt['version']: 1}):
            raise ValueError('candidate does not add exactly the selected changelog version')
        output['version'] = selected_attempt['version']
        output['plan_id'] = selected_attempt['id']
        output['changelog_entries'] = list(added_versions.elements())
        revised = []
        missing = []
        old_paths = set()
        for item in args.older:
            number, separator, raw_path = item.partition('=')
            if not separator or not number.isdecimal():
                raise ValueError(f'invalid --older value: {item}')
            old_checkout = Path(raw_path).resolve()
            old_head = git(old_checkout, 'rev-parse', 'HEAD')
            old_source = git(old_checkout, 'rev-parse', 'HEAD^')
            ancestor = subprocess.run(['git', '-C', str(candidate), 'merge-base', '--is-ancestor', old_source, output['source_sha']])
            if ancestor.returncode:
                raise ValueError(f'older PR {number} source is not an ancestor of selected source')
            parent = json.loads(git(old_checkout, 'show', f'{old_source}:{MANIFEST}'))
            child = json.loads((old_checkout / MANIFEST).read_text())
            old_attempt = added_attempt(parent, child)
            if old_attempt['source_sha'] != old_source:
                raise ValueError(f'older PR {number} plan source mismatch')
            old_notes = notes_by_path(old_attempt)
            for path, note in old_notes.items():
                if source_fragment(old_checkout, old_source, path) != note['text']:
                    raise ValueError(f'older PR {number} note differs from its source fragment: {path}')
            absent, changed = compare_notes(old_notes, selected_notes, source_text)
            missing.extend({'pr': int(number), 'path': path} for path in absent)
            revised.extend({'pr': int(number), **note} for note in changed)
            old_paths.update(old_notes)
            output['older'].append({'number': int(number), 'head_sha': old_head, 'source_sha': old_source, 'plan_id': old_attempt['id'], 'covered_paths': len(old_notes) - len(absent)})
        output['older'].sort(key=lambda item: item['number'])
        output['missing'] = missing
        output['revised'] = revised
        output['new_paths'] = sorted(set(selected_notes) - old_paths)
        if missing:
            raise ValueError('selected plan omits older release fragments')
        output['status'] = 'PASS'
    except (ValueError, FileNotFoundError, KeyError) as exc:
        output['error'] = str(exc)
    print(json.dumps(output, indent=2))
    return 0 if output['status'] == 'PASS' else 1


if __name__ == '__main__':
    sys.exit(main())
